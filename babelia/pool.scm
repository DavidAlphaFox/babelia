;; pool of workers that can be used to execute blocking operation in a
;; fibers application.
(define-module (babelia pool))

(import (ice-9 threads))
(import (srfi srfi-9))

(import (fibers channels))

(import (babelia thread))
(import (babelia ulid))


(define %channel #f)

(define (worker channel)
  (parameterize ((thread-index (random-bytes 2)))
    (let loop ((message (get-message channel)))
      (let ((thunk (car message))
            (return (cdr message)))
        ;; execute thunk and send the returned value
        (let ((out (thunk)))
          (put-message return out)))
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

(define-public (pool-apply thunk)
  (let ((return (make-channel)))
    (put-message %channel (cons thunk return))
    (get-message return)))
