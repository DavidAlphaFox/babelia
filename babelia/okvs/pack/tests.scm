(define-module (babelia okvs pack tests))

(import (babelia testing))
(import (babelia okvs pack))


(define expected
  (list %null
        #t
        #f
        0
        #vu8(42 101 255)
        "hello world"
        'symbol
        42
        (expt 2 64)
        -42
        (- (expt 2 64))))

(define-public test-000
  (test expected (unpack (apply pack expected))))
