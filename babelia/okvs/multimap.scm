;; mutlimap is mapping where a key can be associated with a set of
;; values.
(define-module (babelia okvs multimap))

(import (srfi srfi-9))
(import (babelia generator))
(import (babelia okvs engine))


(define-record-type <mutlimap>
  (make-multimap engine prefix)
  multimap?
  (engine multimap-engine)
  (prefix multimap-prefix))

(export make-multimap)

(define-public (multimap-add transaction multimap key value)
  (let* ((engine (multimap-engine multimap))
         (key* (engine-pack engine (multimap-prefix multimap) key value))
         (value* #vu8(0)))
    (engine-set! engine transaction key* value*)))

(define-public (multimap-generator transaction multimap key)
  (let ((engine (multimap-engine multimap)))
    (gmap (lambda (key+value) (caddr (engine-unpack engine (car key+value))))
          (engine-prefix-range engine transaction (engine-pack engine (multimap-prefix multimap) key)))))

(define-public (multimap-ref transaction multimap key)
  "Return the associated values as a list"
  (generator-map->list values (multimap-generator transaction multimap key)))
