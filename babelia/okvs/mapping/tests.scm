(define-module (babelia okvs mapping tests))

(import (babelia testing))
(import (babelia generator))
(import (babelia okvs mapping))
(import (babelia okvs engine))
(import (babelia okvs wiredtiger))


(define engine (make-default-engine))

(define mapping (make-mapping engine 'test-mapping))

(define-public test-00
  (test  ;; ref unknown key
   #f
   (with-directory "wt"
     (let ((okvs (engine-open engine "wt")))
       (let ((out (okvs-in-transaction okvs
                    (lambda (transaction)
                      (mapping-ref transaction mapping 'unknown)))))
         (engine-close engine okvs)
         out)))))

(define-public test-01
  (test  ;; set and ref
   42
   (with-directory "wt"
     (let ((okvs (engine-open engine "wt")))
       (let ((out (okvs-in-transaction okvs
                    (lambda (transaction)
                      (mapping-set transaction mapping 'magic 42)
                      (mapping-ref transaction mapping 'magic)))))
         (engine-close engine okvs)
         out)))))

(define-public test-02
  (test  ;; generator
   '((1 . one) (2 . two) (3 . three))
   (with-directory "wt"
     (let ((okvs (engine-open engine "wt")))
       (let ((out (okvs-in-transaction okvs
                    (lambda (transaction)
                      (mapping-set transaction mapping 1 'one)
                      (mapping-set transaction mapping 2 'two)
                      (mapping-set transaction mapping 3 'three)
                      (generator-map->list values (mapping-generator transaction mapping))))))
         (engine-close engine okvs)
         out)))))
