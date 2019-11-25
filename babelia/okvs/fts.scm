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

(define (query-parse string)
  (let loop ((keywords (filter (compose not string-null?)
                               (string-split (string-downcase string) #\space)))
             (positives '())
             (negatives '()))
    (if (null? keywords)
        (values positives negatives)
        (if (char=? (string-ref (car keywords) 0) #\-)
            (loop (cdr keywords)
                  positives
                  (cons (string-downcase (substring (car keywords) 1)) negatives))
            (loop (cdr keywords)
                  (cons (string-downcase (car keywords)) positives)
                  negatives)))))

;; TODO: return #f when there is no such object
(define maybe-object->ulid object->ulid)

(define (stems->seeds/transaction transaction fts stems)
  (let* ((ulids (map (lambda (stem) (maybe-object->ulid transaction (fts-ustore fts) stem))
                     stems))
         (counter (make-counter (fts-engine fts)
                                (append (fts-prefix fts) %subspace-counter-stem)))
         (multimap (make-multimap (fts-engine fts)
                                  (append (fts-prefix fts) %subspace-multimap-stem)))
         ;; TODO: OPTIM where we only keep the smallest count, and
         ;; early return in case of zero.
         (counters (map (lambda (ulid) (cons ulid (counter-ref transaction counter ulid)))
                        ulids))
         (seed (caar (sort counters (lambda (a b) (<= (cdr a) (cdr b)))))))
    (multimap-ref transaction multimap seed)))

(define (stems->seeds okvs fts stems)
  (engine-in-transaction (fts-engine fts) okvs
    (lambda (transaction) (stems->seeds/transaction transaction fts stems))))

(define (tiptop limit uid+score) ;; TODO optimize with a scheme mapping
  (define (maybe-take lst limit)
    (if (<= (length lst) limit)
            lst
            (take lst limit)))
  (maybe-take (sort uid+score (lambda (a b) (>= (cdr a) (cdr b)))) limit))

(define (assoc* key alist)
  (assoc key alist bytevector=?))

(define (score/transaction transaction fts uid positives negatives)
  ;; TODO: improve it to support TF-IDF.  To be able to support TF-IDF
  ;; in a performant manner, I guess that TF-IDF requires to cache
  ;; TF(word), TF(stem), DF(word), DF(stem) in the current processus
  ;; shared between all the thread pool workers.  In fact, that cache
  ;; datastructures is a mapping between an ulid and an integer, and
  ;; the total of the associated counter (that should be fetched once
  ;; per fts-query or updated regularly).  That is, it could be some
  ;; kind of hash-table with read and write lock and a condition
  ;; variable per key-value association (pessimistic locking).  I
  ;; guess it requires benchmarks.  It might require a in-memory
  ;; thread-safe SRFI-167, that rely on pessimistic locking.  There is
  ;; no need for transactions spanning multiple keys, still it could
  ;; dead-lock.  Exposing the bytevector-to-bytevector interface of
  ;; SRFI-167, will require packing and unpacking for no good reason?
  ;; Note that there is no need for ordered keys. It looks fun.

  ;; TODO: Think about how to handle structured document like html
  ;; where they are fields that are more important than others. It
  ;; might be called "field boosting" or "weight scoring".

  ;; TODO: Research BM25

  ;; TODO: Maybe research how to bonus small texts

  ;; XXX: In any case, make it work, then benchmark, then make it fast
  (let* ((mapping (make-mapping (fts-engine fts)
                                (append (fts-prefix fts) %subspace-mapping-text)))
         (bag (mapping-ref transaction mapping uid))
         (positives (map (lambda (x) (maybe-object->ulid transaction
                                                         (fts-ustore fts)
                                                         x))
                         positives))
         (negatives (map (lambda (x) (maybe-object->ulid transaction
                                                         (fts-ustore fts)
                                                         x))
                         negatives)))
    (if (any (lambda (negative) (member negative (map car bag))) negatives)
        (cons uid #f)
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

(define (score okvs fts seed positives negatives)
  (engine-in-transaction (fts-engine fts) okvs
    (lambda (transaction) (score/transaction transaction fts seed positives negatives))))

(define-public (fts-query okvs fts string)
  ;; XXX: fts-query takes an okvs as argument, instead of a
  ;; transaction.  Workers will spawn their own transactions.  It may
  ;; be possible to pass transaction as argument, and retrieve the
  ;; okvs handle from it but that would be mis-leading regarding the
  ;; guarantees that fts-query provides.

  ;; TODO: The query named STRING, only support minus operator to
  ;; exclude a keyword.  It does not reduce the score of documents
  ;; where there are words that share the same stem.  Eventually, it
  ;; should also support OR operator, exact match, phrase match with
  ;; "double quotes" and synonyms.  It should give a better score to
  ;; documents where the positive keywords appear near each other:
  ;; proximity bonus. This lead me to think that the datastructure
  ;; required to keep track of all those information is not a single
  ;; record or a nested list.  I wonder if this is a usecase for a
  ;; SRFI-168 that does not rely on srfi-167 for a last mile speed up.
  ;; It would not require transactions at all and would not be
  ;; threadsafe. It prolly does not need generators. If the pure
  ;; SRFI-168 route is taken, it will require a nstore->list procedure
  ;; to be able to store in the database in fts-index.

  ;; TODO: Also, see the comment in the procedure score/transaction.

  ;; TODO: query-parse in fts-apply and return multiple values, to
  ;; avoid blocking the main thread. Do not forget to fetch ulid for
  ;; query terms.

  ;; TODO: validate query: a) do not accept only negation b) do not
  ;; accept the exclusion of a positive word.

  ;; TODO: everything before fts-for-each-map must be in a
  ;; procedure fts-prepare that is called with fts-apply. Among other
  ;; things it must return the query terms as ulids.
  (call-with-values (lambda () (query-parse string))
    (lambda (positives negatives)
      (when (null? positives)
        (error 'babelia "invalid query, no positive keyword" string))
      (let* ((stems (map (lambda (word) (stem english word)) (filter sane? positives)))
             ;; XXX: database queries must be done in worker thread pool.
             (seeds (fts-apply fts (lambda () (stems->seeds okvs fts stems)))))
        (let ((out '()))
          (fts-for-each-map
           fts
           (lambda (uid+score)
             ;; keep top 'fts-limit' documents
             (when (cdr uid+score)
               (set! out (tiptop (fts-limit fts) (cons uid+score out)))))
           (lambda (uid) (score okvs fts uid positives negatives))
           seeds)
          out)))))
