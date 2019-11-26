(define-module (babelia okvs counter tests))

(import (babelia testing))
(import (babelia generator))
(import (babelia okvs counter))
(import (babelia okvs engine))
(import (babelia okvs wiredtiger))


(define engine (make-default-engine))

(define counter (make-counter engine 'test-counter))

(define-public test-00
  (test  ;; ref unknown key
   0
   (with-directory "wt"
     (let ((okvs (engine-open engine "wt")))
       (let ((out (okvs-in-transaction okvs
                    (lambda (transaction)
                      (counter-ref transaction counter 'unknown)))))
         (engine-close engine okvs)
         out)))))

(define-public test-01
  (test  ;; set and ref
   42
   (with-directory "wt"
     (let ((okvs (engine-open engine "wt")))
       (let ((out (okvs-in-transaction okvs
                    (lambda (transaction)
                      (let loop ((index 42))
                        (unless (zero? index)
                          (counter-increment transaction counter 'magic)
                          (loop (- index 1))))
                      (counter-increment transaction counter 'one)
                      (counter-increment transaction counter 'two)
                      (counter-increment transaction counter 'two)
                      (counter-ref transaction counter 'magic)))))
         (engine-close engine okvs)
         out)))))


(define-public test-02
  (test  ;; total
   45
   (with-directory "wt"
     (let ((okvs (engine-open engine "wt")))
       (let ((out (okvs-in-transaction okvs
                    (lambda (transaction)
                      (let loop ((index 42))
                        (unless (zero? index)
                          (counter-increment transaction counter 'magic)
                          (loop (- index 1))))
                      (counter-increment transaction counter 'one)
                      (counter-increment transaction counter 'two)
                      (counter-increment transaction counter 'two)
                      (counter-total transaction counter)))))
         (engine-close engine okvs)
         out)))))

(define-public test-03
  (test  ;; fold
   '((two . 2) (one . 1) (magic . 42))
   (with-directory "wt"
     (let ((okvs (engine-open engine "wt")))
       (let ((out (okvs-in-transaction okvs
                    (lambda (transaction)
                      (let loop ((index 42))
                        (unless (zero? index)
                          (counter-increment transaction counter 'magic)
                          (loop (- index 1))))
                      (counter-increment transaction counter 'one)
                      (counter-increment transaction counter 'two)
                      (counter-increment transaction counter 'two)
                      (counter-fold transaction cons '() counter)))))
         (engine-close engine okvs)
         out)))))
