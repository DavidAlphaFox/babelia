(define-module (babelia okvs mapping))

(import (srfi srfi-9))
(import (babelia generator))
(import (babelia okvs engine))


(define-record-type <mapping>
  (make-mapping engine prefix)
  mapping?
  (engine mapping-engine)
  (prefix mapping-prefix))

(export make-mapping)

(define-public (mapping-set transaction mapping key value)
  (let* ((engine (mapping-engine mapping))
         (key* (engine-pack engine (mapping-prefix mapping) key))
         (value* (engine-pack engine value)))
    (engine-set! engine transaction key* value*)))

(define-public (mapping-ref transaction mapping key)
  (let* ((engine (mapping-engine mapping))
         (key* (engine-pack engine (mapping-prefix mapping) key))
         (value (engine-ref engine transaction key*)))
    (and=> value (lambda (value) (car (engine-unpack engine value))))))

(define-public (mapping-generator transaction mapping)
  (let ((engine (mapping-engine mapping)))
    (gmap (lambda (key+value) (cons (cadr (engine-unpack engine (car key+value)))
                                    (car (engine-unpack engine (cdr key+value)))))
          (engine-prefix-range engine transaction (engine-pack engine (mapping-prefix mapping))))))

(define-public (mapping-clear transaction mapping)
  (let* ((engine (mapping-engine mapping)))
    (engine-prefix-range-remove! engine
                                 transaction
                                 (engine-pack engine (mapping-prefix mapping)))))
