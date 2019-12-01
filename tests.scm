(define-module (tests))

(import (scheme base))
(import (babelia testing))



(export test-test)
(export test-test-raise)


(define test-test
  (test #t #t))

(define test-test-raise
  (test-raise (lambda (x) (eq? x 'test-raise))
              (lambda () (raise 'test-raise))))
