(define-module (babelia okvs fts))

(import (srfi srfi-1))
(import (srfi srfi-9))
(import (babelia stemmer))


(define english (make-stemmer "english"))

(define-record-type <fts>
  (make-fts prefix engine thread-index for-each-par-map)
  fts?
  (prefix fts-prefix)
  (engine fts-engine)
  (thread-index %fts-thread-index)
  (for-each-par-map %fts-for-each-par-map))

(define-public fts make-fts)

(define (fts-thread-index fts)
  ((%fts-thread-index fts)))

(define (fts-for-each-par-map fts sproc pproc lst)
  ((%fts-pool-apply fts) thunk))

(define ALPHANUM (string->list "qwertyuiopasdfghjklzxcvbnm0123456789"))

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
                 (list->string
                  (map maybe-space (string->list text))) #\space)))))

(define %word-counter-subspace 0)
(define %stem-counter-subspace 1)
(define %stem-multimap-subspace 2)
(define %text-map-subspace 3)

(define (fts-stem-increment transaction fts ulid)
  (let ((counter (counter (append (fts-prefix fts) (list %stem-counter-subspace))
                          (fts-engine fts))))
    (counter-increment transaction counter ulid)))

(define (fts-stem-append fts stem ulid)
  (let ((multimap (multimap (append (fts-prefix fts) (list %stem-multimap-subspace))
                            (fts-engine fts))))
    (multimap-append transaction multimap stem ulid)))

(define (fts-index-stem transaction fts text ulid)
  (let ((stems (map (lambda (stem) (object->ulid transaction object stem))
                    (text->stems text))))
    (let loop ((stems stems))
      (unless (null? stems)
        (fts-stem-increment transaction fts (car stems))
        (fts-stem-append fts (car stems) ulid)
        (loop (cdr stems))))
    (not (null? stems))))

(define (text->words text)
  (uniquify
   (filter (compose not sane?)
           (string-split
            (list->string
             (map maybe-space (string->list text))) #\space))))

(define (fts-word-increment transaction fts ulid)
  (let ((counter (counter (append (fts-prefix fts) (list %word-counter-subspace))
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
