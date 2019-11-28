;; rstore for record (or row) store is meant to store records of known
;; fields.  It is used to retrieve the title, url and preview of
;; something that is indexed using fts.scm.  It is overkill to use the
;; generic tuple store (nstore.scm) because the fields are all known
;; in advance.

;; XXX: rstore rely on r6rs procedural records API to inspect records
;; type.  Since guile has unified records, you can pass a srfi-9
;; record type to rstore constructor.
(define-module (babelia okvs rstore))

(import (only (rnrs) error))
(import (rnrs records inspection (6)))

(import (srfi srfi-1))
(import (srfi srfi-9))
(import (babelia okvs engine))
(import (babelia okvs ulid))


;; helpers

(define (instance->list rtd instance)
  (let loop ((ks (vector->list (record-type-field-names rtd)))
             (out '()))
    (if (null? ks)
        (reverse out)
        (loop (cdr ks) (cons ((record-accessor rtd (car ks)) instance) out)))))

(define (list->instance rtd lst)
  (apply (record-constructor rtd) lst))

;; rstore

(define-record-type <rstore>
  (make-rstore engine prefix rtd)
  rstore?
  (engine rstore-engine)
  (prefix rstore-prefix)
  (rtd rstore-rtd))

(export make-rstore)

(define-public (rstore-insert transaction rstore instance)
  (let ((engine (rstore-engine rstore))
        (out (ulid)))
    (engine-set! engine
                 transaction
                 (apply engine-pack engine (append (rstore-prefix rstore)
                                                   (list out)))
                 (apply engine-pack engine (instance->list (rstore-rtd rstore) instance)))
    out))

(define-public (rstore-update transaction rstore uid instance)
  (let ((engine (rstore-engine rstore)))
    (engine-set! engine
                 transaction
                 (apply engine-pack engine (append (rstore-prefix rstore)
                                                   (list uid)))
                 (apply engine-pack engine (instance->list (rstore-rtd rstore) instance)))))

(define-public (rstore-delete transaction rstore uid)
  (let ((engine (rstore-engine rstore)))
    (engine-delete! engine
                    transaction
                    (apply engine-pack engine (append (rstore-prefix rstore)
                                                      (list uid))))))

(define-public (rstore-ref transaction rstore uid)
  (let ((engine (rstore-engine rstore)))
    (and=> (engine-ref engine
                       transaction
                       (apply engine-pack engine (append (rstore-prefix rstore)
                                                         (list uid))))
           (lambda (value)
             (list->instance (rstore-rtd rstore)
                             (engine-unpack engine value))))))
