(define-module (babelia okvs multimap tests))

(import (babelia testing))
(import (babelia generator))
(import (babelia okvs multimap))
(import (babelia okvs engine))
(import (babelia okvs wiredtiger))


(define engine (make-default-engine))

(define multimap (make-multimap engine 'test-multimap))

(define-public test-00
  (test  ;; ref empty database
   '()
   (with-directory "wt"
     (let ((okvs (engine-open engine "wt")))
       (let ((out (okvs-in-transaction okvs
                    (lambda (transaction)
                      (multimap-ref transaction multimap 'unknown)))))
         (engine-close engine okvs)
         out)))))

(define-public test-01
  (test  ;; ref unknown key
   '()
   (with-directory "wt"
     (let ((okvs (engine-open engine "wt")))
       (let ((out (okvs-in-transaction okvs
                    (lambda (transaction)
                      (multimap-add transaction multimap 'key 1)
                      (multimap-add transaction multimap 'key 2)
                      (multimap-add transaction multimap 'key 3)
                      (multimap-add transaction multimap 'magic 42)
                      (multimap-ref transaction multimap 'unknown)))))
         (engine-close engine okvs)
         out)))))

(define-public test-02
  (test  ;; ref 'magic
   '(42)
   (with-directory "wt"
     (let ((okvs (engine-open engine "wt")))
       (let ((out (okvs-in-transaction okvs
                    (lambda (transaction)
                      (multimap-add transaction multimap 'key 1)
                      (multimap-add transaction multimap 'key 2)
                      (multimap-add transaction multimap 'key 3)
                      (multimap-add transaction multimap 'magic 42)
                      (multimap-ref transaction multimap 'magic)))))
         (engine-close engine okvs)
         out)))))

(define-public test-03
  (test  ;; ref 'key
   '(1 2 3)
   (with-directory "wt"
     (let ((okvs (engine-open engine "wt")))
       (let ((out (okvs-in-transaction okvs
                    (lambda (transaction)
                      (multimap-add transaction multimap 'key 1)
                      (multimap-add transaction multimap 'key 2)
                      (multimap-add transaction multimap 'key 3)
                      (multimap-add transaction multimap 'magic 42)
                      (multimap-ref transaction multimap 'key)))))
         (engine-close engine okvs)
         out)))))
