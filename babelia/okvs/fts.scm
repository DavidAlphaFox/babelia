(define-module (babelia okvs fts))

(import (srfi srfi-1))
(import (srfi srfi-9))
(import (babelia stemmer))


;; TODO: i18n
(define english (make-stemmer "english"))

(define-record-type <fts>
  (make-fts prefix engine for-each-par-map)
  fts?
  (prefix fts-prefix)
  (engine fts-engine)
  (for-each-par-map %fts-for-each-par-map))

(define (fts-for-each-par-map fts sproc pproc lst)
  ((%fts-for-each-par-map fts) sproc pproc lst))

(define ALPHANUM (string->list "0123456789abcdefghijklmnopqrstuvwxyz"))

(define (maybe-space char)
  (if (member char ALPHANUM)
      char
      #\space))

(define (sane? word)
  (< 2 (string-length word) 64))

(define (uniquify lst)
  (delete-duplicates lst string=?))

(define (text->stems text)
  (uniquify
   (map (lambda (word) (stem english word))
        (filter (compose not sane?)
                (string-split
                 (list->string (map maybe-space (string->list (string-downcase text))))
                 #\space)))))

(define %subspace-counter-word '(0))
(define %subspace-counter-stem '(1))
(define %subspace-multimap-stem '(2))
(define %subspace-mapping-text '(3))

(define (fts-stem-increment transaction fts ulid)
  (let ((counter (counter (append (fts-prefix fts) %subspace-counter-stem)
                          (fts-engine fts))))
    (counter-increment transaction counter ulid)))

(define (fts-stem-add fts stem ulid)
  (let ((multimap (multimap (append (fts-prefix fts) %subspace-multimap-stem)
                            (fts-engine fts))))
    (multimap-add transaction multimap stem ulid)))

(define (fts-index-stem transaction fts text ulid)
  (let ((stems (map (lambda (stem) (object->ulid transaction object stem))
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
        ;; than count the number of keys in a okvs/multimap that might
        ;; be bigger that 2^16.  All that explains, the duplicate work
        ;; in the following code:
        (fts-stem-increment transaction fts (car stems))
        (fts-stem-add fts (car stems) ulid)
        (loop (cdr stems))))
    (not (null? stems))))

(define (text->words text)
  (uniquify
   (filter (compose not sane?)
           (string-split
            (list->string (map maybe-space (string->list (string-downcase text))))
            #\space))))

(define (fts-text-store transaction ulid text)
  (let ((mapping (mapping (append (fts-prefix fts) %subspace-mapping-text)
                          (fts-engine fts))))
    (mapping-set transaction mapping ulid (object->ulid transaction object text))))

(define (fts-word-increment transaction fts ulid)
  (let ((counter (counter (append (fts-prefix fts) %subspace-counter-word)
                          (fts-engine fts))))
    (counter-increment transaction counter ulid)))

(define (fts-index-text transaction text ulid)
  ;; store raw TEXT
  (fts-text-store transaction ulid text)
  ;; store WORDS count to compute IDF
  (let ((words (map (lambda (word) (object->ulid transaction object word))
                    (text->words text))))
    (for-each (lambda (word) (fts-word-increment transaction fts word) words))))

(define-public (fts-index transaction fts ulid text)
  (when (fts-index-stem transaction fts text ulid)
    (fts-index-text transaction fts text ulid)))

(define-public (fts-query fts query)
  (when #f #f))
