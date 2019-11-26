;; counter
;;
;; The counter is split into multiple rows to reduce contention, hence
;; reduce the chanced of transaction roll-back and retry. It rely on
;; thread-index that is two bytes hence 65 536 differents
;; values. Thus, it is possible for as much threads to concurrently
;; increment a counter when thread-index is chosen wisely. The
;; trade-off is that reading the count is slower.
;;
;; Note: FoundationDB has atomic operations. And sqlite nested
;; transactions.
;;
;; The counter is used in the full-text index to keep track of how
;; often stems and words appear. Hence, the contention happens at
;; index time. Slow reads happens all the time.
;;
;; TODO: It will be a good idea to cache word and stem count on a per
;; query request
;;
;; TODO: Implement counter-decrement
(define-module (babelia okvs counter))

(import (srfi srfi-9))
(import (only (rnrs) eof-object?))

(import (ice-9 match))

(import (babelia thread))
(import (babelia bytevector))
(import (babelia generator))
(import (babelia okvs engine))


(define-record-type <counter>
  (make-counter engine prefix)
  counter?
  (engine counter-engine)
  (prefix counter-prefix))

(export make-counter)

(define-public (counter-increment transaction counter object)
  "Increment count for OBJECT"
  (let* ((engine (counter-engine counter))
         (prefix (counter-prefix counter))
         (key (apply engine-pack engine prefix object (list (thread-index))))
         (count (and=> (engine-ref engine transaction key)
                       (lambda (key) (car (engine-unpack engine key)))))
         (new-count (engine-pack engine (if count (+ count 1) 1))))
    (engine-set! engine transaction key new-count)))

(define-public (counter-ref transaction counter object)
  (let* ((engine (counter-engine counter))
         (prefix (apply engine-pack
                        engine
                        (counter-prefix counter)
                        (list object)))
         (generator (engine-prefix-range engine transaction prefix)))
    (let loop ((count 0)
               (key+value (generator)))
      (if (eof-object? key+value)
          count
          (loop (+ count (car (engine-unpack engine (cdr key+value))))
                (generator))))))

(define-public (counter-total transaction counter)
  (let* ((engine (counter-engine counter))
         (prefix (engine-pack engine (counter-prefix counter)))
         (generator (engine-prefix-range engine transaction prefix)))
    (let loop ((count 0)
               (key+value (generator)))
      (if (eof-object? key+value)
          count
          (loop (+ count (car (engine-unpack engine (cdr key+value))))
                (generator))))))


(define %null '(null))

(define (unpack engine x)
  (cons (engine-unpack engine (car x)) (car (engine-unpack engine (cdr x)))))

(define-public (counter-fold transaction kons knil counter)
  (let* ((engine (counter-engine counter))
         (prefix (engine-pack engine (counter-prefix counter)))
         (generator (gmap (lambda (x) (unpack engine x))
                          (engine-prefix-range engine transaction prefix))))
    (let loop ((key %null)
               (count 0)
               (key+value (generator))
               (knil knil))
      (if (eof-object? key+value)
          (kons (cons key count) knil)
          (match key+value
            (((prefix other _0) . value)
             (if (eq? key other)
                 (loop key (+ count value) (generator) knil)
                 (begin
                   (if (eq? key %null)
                       (loop other value (generator) knil)
                       (loop other value (generator) (kons (cons key count) knil)))))))))))
