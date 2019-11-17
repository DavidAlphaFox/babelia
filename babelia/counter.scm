;; counter
;;
;; This is an OKVS abstraction that allows to count. The counter is
;; split into multiple rows to reduce contention, hence transaction
;; roll-back and retry. It rely on thread-index that is two bytes
;; hence 65 536 differents values. Thus, it is possible for as much
;; threads to concurrently increment a counter. The trade-off is that
;; reading the count is slower.
;;
;; As of today, the counter is used in the full-text index to keep
;; track of how often a stems and words appear. Hence, the contention
;; happens at index time. Slow reads happens all the time.
;;
;; TODO: It will be a good idea to cache word and stem count on a per
;; query request
;;
;; As of today, it is not possible to decrement a counter, because
;; unindexing, deleting indexed items in the full-text index, is not
;; supported.
;;
(define-module (babelia counter))

(import (srfi srfi-9))
(import (only (rnrs) eof-object?))

(import (babelia thread))
(import (babelia bytevector))
(import (babelia generator))


(define-record-type <counter>
  (make-counter prefix engine)
  counter?
  (prefix counter-prefix)
  (engine counter-engine))


(define-public (counter prefix engine)
  (make-counter (apply engine-pack engine prefix) engine))

(define-public (counter-increment transaction counter ulid)
  (let* ((engine (counter-engine counter))
         (prefix (counter-prefix counter))
         (key (apply bytevector-append prefix ulid (thread-index)))
         (count (engine-ref transaction key)))
    (engine-set! engine transaction key (if count (+ counter 1) 1))))

(define-public (counter-ref transaction counter ulid)
  (let* ((engine (counter-engine counter))
         (prefix (counter-prefix counter))
         (subspace (apply bytevector-append prefix ulid))
         (generator (engine-range-prefix engine transaction subspace)))
    (let loop ((count 0)
               (increment (generator)))
      (if (eof-object? increment)
          count
          (loop (+ count increment)
                (generator))))))
