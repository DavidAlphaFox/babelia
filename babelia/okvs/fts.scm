(define-module (babelia okvs fts))

(import (only (rnrs) error))

(import (srfi srfi-1))
(import (srfi srfi-9))
(import (babelia stemmer))
(import (babelia bytevector))
(import (babelia okvs mapping))
(import (babelia okvs engine))
(import (babelia okvs ustore))
(import (babelia okvs counter))
(import (babelia okvs multimap))


;; TODO: i18n
(define english (make-stemmer "english"))

(define-record-type <fts>
  (make-fts engine ustore prefix limit apply for-each-map)
  fts?
  (engine fts-engine)
  (prefix fts-prefix)
  (ustore fts-ustore)
  (limit fts-limit)
  (apply %fts-apply)
  (for-each-map %fts-for-each-map))

(export make-fts)

(define (fts-for-each-map fts sproc pproc lst)
  ((%fts-for-each-map fts) sproc pproc lst))

;; TODO: make it the same signature as scheme apply
(define (fts-apply fts thunk)
  ((%fts-apply fts) thunk))

(define ALPHANUM (string->list "0123456789abcdefghijklmnopqrstuvwxyz"))

(define (maybe-space char)
  (if (member char ALPHANUM)
      char
      #\space))

(define (sane? word)
  (< 2 (string-length word) 64))

(define (uniquify lst)
  (delete-duplicates lst string=?))

(define (text->stems text) ;; TODO: optimize
  (uniquify
   (map (lambda (word) (stem english word))
        (filter sane?
                (string-split
                 (list->string (map maybe-space (string->list (string-downcase text))))
                 #\space)))))

(define %subspace-counter-word '(0))
(define %subspace-counter-stem '(1))
(define %subspace-multimap-stem '(2))
(define %subspace-mapping-text '(3))

(define (fts-stem-increment transaction fts ulid)
  (let ((counter (make-counter (fts-engine fts)
                               (append (fts-prefix fts) %subspace-counter-stem))))
    (counter-increment transaction counter ulid)))

(define (stem-count-ref transaction fts stem)
  (let ((counter (make-counter (fts-engine fts)
                               (append (fts-prefix fts) %subspace-counter-stem))))
    (counter-ref transaction counter stem)))

(define (fts-stem-add transaction fts stem ulid)
  (let ((multimap (make-multimap (fts-engine fts)
                                 (append (fts-prefix fts) %subspace-multimap-stem))))
    (multimap-add transaction multimap stem ulid)))

(define (fts-index-stem transaction fts text uid)
  (let ((stems (map (lambda (stem) (object->ulid transaction (fts-ustore fts) stem))
                    (text->stems text))))
    (let loop ((stems stems))
      (unless (null? stems)
        ;; XXX: If you look close enough, you will figure that
        ;; okvs/multimap can do what okvs/counter does. That is,
        ;; okvs/multimap can also count and serve as a counter.  The
        ;; thing is that FDB does not allow to quickly count all the
        ;; keys in a given range, that is why the okvs interface is
        ;; what it is.  Also, we need fts-stem-count to be very fast
        ;; to compute the discriminant stem at query time.  I think,
        ;; it is faster to compute the sum of values of an okvs/map as
        ;; used in okvs/counter that is at most 2^16 values to sum,
        ;; O(1) complexity. Meanwhile to count the number of keys in a
        ;; okvs/multimap that might be much bigger that 2^16 with O(n)
        ;; complexity where n is the number of items in the multimap.
        ;; All that explains, the duplicate work in the following
        ;; code:
        (fts-stem-increment transaction fts (car stems))
        (fts-stem-add transaction fts (car stems) uid)
        (loop (cdr stems))))
    (not (null? stems))))

(define (text->words text) ;; TODO: optimize
  (filter sane?
          (string-split
           (list->string (map maybe-space (string->list (string-downcase text))))
           #\space)))

(define (text->bag transaction fts text)
  (let ((words (text->words text)))
    (let loop ((words* (uniquify words))
               (bag '()))
      (if (null? words*)
          bag
          (loop (cdr words*) (cons (list (object->ulid transaction
                                                       (fts-ustore fts)
                                                       (car words*))
                                         (count (lambda (x) (string=? x (car words*))) words))
                                    bag))))))

(define (fts-text-store transaction fts uid text)
  (let ((mapping (make-mapping (fts-engine fts)
                               (append (fts-prefix fts) %subspace-mapping-text))))
    ;; there must be no ulid->sha256->object indirections to improve
    ;; query-time performance.

    ;; TODO: massage TEXT to a representation that is perfect for
    ;; query time.
    (mapping-set transaction mapping uid (text->bag transaction fts text))))

(define (fts-word-increment transaction fts ulid)
  (let ((counter (make-counter (fts-engine fts)
                               (append (fts-prefix fts) %subspace-counter-word))))
    (counter-increment transaction counter ulid)))

(define (fts-index-text transaction fts text uid)
  ;; store raw TEXT
  (fts-text-store transaction fts uid text)
  ;; store WORDS count to compute IDF
  (let ((words (map (lambda (word) (object->ulid transaction (fts-ustore fts) word))
                    (uniquify (text->words text)))))
    (for-each (lambda (word) (fts-word-increment transaction fts word)) words)))

(define-public (fts-index transaction fts uid text)
  "Index TEXT string with UID as an identifier."
  ;; TODO: check whether uid already is indexed.
  (when (fts-index-stem transaction fts text uid)
    (fts-index-text transaction fts text uid)))

(define (fts-query-parse tx fts string)
  (let loop ((keywords (filter (compose not string-null?)
                               (string-split (string-downcase string) #\space)))
             (positives '())
             (negatives '()))
    (if (null? keywords)
        (values #t positives negatives)
        (if (char=? (string-ref (car keywords) 0) #\-)
            (loop (cdr keywords)
                  positives
                  (let* ((keyword (substring (car keywords) 1))
                         (negative (maybe-object->ulid tx (fts-ustore fts) keyword)))
                    (if negative
                        (cons negative negatives)
                        negatives)))
            (let* ((keyword (car keywords))
                   (positive (maybe-object->ulid tx (fts-ustore fts) keyword)))
              (if positive
                  (loop (cdr keywords)
                        (cons (cons positive
                                    (object->ulid tx (fts-ustore fts) (stem english keyword)))
                              positives)
                        negatives)
                  (values #f #f #f))))))) ;; unknown positive keyword => no results.

;; TODO: return #f when there is no such object
(define maybe-object->ulid object->ulid)

(define (tiptop limit uid+score) ;; TODO optimize with a scheme mapping
  (define (maybe-take lst limit)
    (if (<= (length lst) limit)
            lst
            (take lst limit)))
  (maybe-take (sort uid+score (lambda (a b) (>= (cdr a) (cdr b)))) limit))

(define (assoc* key alist)
  (assoc key alist bytevector=?))

(define (text-ref tx fts uid)
  (let ((mapping (make-mapping (fts-engine fts)
                               (append (fts-prefix fts) %subspace-mapping-text))))
    (mapping-ref tx mapping uid)))

(define (score tx fts uid negatives positives)
  ;; TODO: Think about how to handle structured document like html
  ;; where they are fields that are more important than others. It
  ;; might be called "field boosting" or "weight scoring".

  ;; TODO: Research BM25

  ;; TODO: Maybe research how to bonus small texts

  ;; XXX: In any case, make it work, then benchmark, then make it fast
  (let* ((bag (text-ref tx fts uid))
         (words (map car bag)))
    (if (any (lambda (negative) (member negative words bytevector=?)) negatives)
        (cons #f #f)
        (let loop ((positives positives)
                   (score 0))
          (cond
           ;; return
           ((null? positives) (cons uid score))
           ;; good, increment score
           ((assoc* (car positives) bag)
            => (lambda (x) (loop (cdr positives) (+ score (cadr x)))))
           ;; a positive keyword is not found in the bag, hence the
           ;; document is not a match, so ignore it
           (else (cons uid #f)))))))

(define %null-query '(#f #f #f))

(define (most-discriminant tx fts stems)
  ;; compute the most discriminant stem
  (let loop ((stems stems)
             (min (inf))
             (out #f))
    (if (null? stems)
        out
        (let ((count (stem-count-ref tx fts (car stems))))
          (if (< count min)
              (loop (cdr stems) count (car stems))
              (loop (cdr stems) min out))))))

(define (seed->candidates tx fts seed)
  (let ((multimap (make-multimap (fts-engine fts)
                                 (append (fts-prefix fts) %subspace-multimap-stem))))
    (multimap-ref tx multimap seed)))

(define (fts-query-prepare/transaction tx fts string)
  (call-with-values (lambda () (fts-query-parse tx fts string))
    (lambda (continue? positives negatives)
      (if (not continue?)
          %null-query
          (list #t
                (seed->candidates tx fts (most-discriminant tx fts (map cdr positives)))
                (lambda (tx fts uid)
                  (score tx fts uid negatives (map car positives))))))))

(define (fts-query-prepare okvs fts string)
  (apply values
         (fts-apply fts
                    (lambda ()
                      (engine-in-transaction (fts-engine fts) okvs
                        (lambda (tx) (fts-query-prepare/transaction tx fts string)))))))

(define-public (fts-query okvs fts string)
  ;; XXX: fts-query takes an okvs as argument, instead of a
  ;; transaction.  Workers will spawn their own transactions.  It may
  ;; be possible to pass transaction as argument, and retrieve the
  ;; okvs handle from it but that would be mis-leading regarding the
  ;; guarantees that fts-query provides.

  ;; TODO: validate query: a) do not accept only negation b) do not
  ;; accept the exclusion of a positive word.

  ;; Prepare the arguments of fts-for-each-map.
  (call-with-values (lambda () (fts-query-prepare okvs fts string))
    (lambda (continue? candidates score/transaction)
      (if (not continue?)
          '()
          (let ((out '()))
            (fts-for-each-map
             fts
             (lambda (uid+score)
               ;; keep top 'fts-limit' documents
               (when (cdr uid+score)
                 (set! out (tiptop (fts-limit fts) (cons uid+score out)))))
             (lambda (uid) (engine-in-transaction (fts-engine fts) okvs
                             (lambda (tx) (score/transaction tx fts uid))))
             candidates)
            out)))))
