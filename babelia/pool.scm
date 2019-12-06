;; pool of workers that can be used to execute blocking operation in a
;; fibers application.
;;
;; TODO: maybe it will be better to re-base this module on `future` or
;; `promise` object.
(define-module (babelia pool))

(import (ice-9 match))
(import (ice-9 q))
(import (ice-9 threads))
(import (srfi srfi-9))
(import (srfi srfi-1))
(import (fibers))
(import (fibers channels))
(import (fibers operations))
(import (babelia thread))
(import (babelia okvs ulid))
(import (babelia log))


(define %channel #f)

(define worker-count (- (current-processor-count) 1))

(define (worker channel)
  (parameterize ((thread-index (random-bytes 2)))
    (let ((worker (make-channel)))
      (let loop ()
        (put-message channel (cons 'worker worker))
        (let* ((work (get-message worker))
               (thunk (car work))
               (return (cdr work))
               ;; Execute thunk and send the returned value.  XXX: To be able
               ;; to keep track of jobs, the channel called `return`, is put
               ;; in itself.  See procedure pool-for-each-par-map.

               ;; TODO: add a call-with-values
               (out (thunk)))
          (put-message return (cons return out)))
        (loop)))))

(define (arbiter channel)
  (let ((works (make-q))
        (workers (make-q)))
    (let loop ((message (get-message channel)))
      (match message
        (('worker . worker)
         (if (q-empty? works)
             (enq! workers worker)
             (let ((work (deq! works)))
               (put-message worker work))))
        (('work . work)
         (if (q-empty? workers)
             (enq! works work)
             (let ((worker (deq! workers)))
               (put-message worker work))))
        (_ (raise (cons 'fuu message))))
      (loop (get-message channel)))))

(define-public (pool-init)
  (if %channel
      (error 'babelia "pool can not be initialized more than once")
      (let ((channel (make-channel)))
        (log-debug "pool init")
        (set! %channel channel)
        (let loop ((index worker-count))
          (unless (zero? index)
            (call-with-new-thread (lambda () (worker channel)))
            (loop (- index 1))))
        (arbiter channel))))

(define (publish thunk)
  (let ((return (make-channel)))
    (put-message %channel (cons 'work (cons thunk return)))
    return))

(define-public (pool-apply thunk)
  "Execute THUNK in a worker thread.

   Pause the calling fiber until the result is available."
  (cdr (get-message (publish thunk))))

(define (select channels)
  (log-trace "select")
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
  (let loop ((channels (map (lambda (item) (publish (lambda () (pproc item))))
                            lst)))
    (unless (null? channels)
      (match (select channels)
        ((channel . value)
         (sproc value)
         (loop (remove (lambda (x) (eq? x channel)) channels)))
        (else (raise 'fuuubar))))))
