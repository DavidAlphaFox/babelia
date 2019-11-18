;; pool of workers that can be used to execute blocking operation in a
;; fibers application.
;;
;; TODO: maybe it will be better to re-base this module on `future` or
;; `promise` object.
(define-module (babelia pool))

(import (ice-9 match))
(import (ice-9 threads))
(import (srfi srfi-9))
(import (fibers channels))
(import (fibers operations))
(import (babelia thread))
(import (babelia ulid))


(define %channel #f)

(define (worker channel)
  (parameterize ((thread-index (random-bytes 2)))
    (let loop ((message (get-message channel)))
      (let ((thunk (car message))
            (return (cdr message)))
        ;; Execute thunk and send the returned value.  XXX: To be able
        ;; to keep track of jobs, the channel called `return`, is put
        ;; in itself.  See procedure pool-for-each-par-map.
        (let ((out (thunk)))
          (put-message return (cons return out))))
      (loop (get-message channel)))))

(define-public (pool-init)
  (if %channel
      (error 'babelia "pool can not be initialized more than once")
      (let ((channel (make-channel)))
        (let loop ((index (- (current-processor-count) 1)))
          (unless (zero? index)
            (call-with-new-thread (lambda () (worker channel)))
            (loop (- index 1))))
        (set! %channel channel))))

(define (publish thunk)
  (let ((return (make-channel)))
    (put-message %channel (cons thunk return))
    return))

(define-public (pool-apply thunk)
  "Execute THUNK in a worker thread.

   Pause the calling fiber until the result is available."
  (cdr (get-message (publish thunk))))

(define (select channels)
  (perform-operation
   (apply choice-operation (map get-operation channels))))

;; TODO: Maybe add a timeout argument, in order to be able to display
;; a nicer error.
(define-public (pool-for-each-par-map sproc pproc lst)
  "For each item of LST execute (PPROC item) in a worker thread, and
   gather returned value with SPROC. SPROC is executed in the calling
   fiber.

   This a POSIX thread pool based n-for-each-par-map for fibers. It is
   somewhat equivalent to:

     (for-each SSPROC (map PPROC LST))

   But applications of PPROC happens in parallel and waiting for new
   values is not blocking the main thread."
  (let loop ((channels (map (lambda (item) (publish (lambda () (pproc item)))) lst)))
    (unless (null? channels)
      (match (select channels)
        ((channel . value)
         (sproc value)
         (loop (remove (lambda (x) (eq? x channel)) channels)))))))
