(define-module (babelia object))

(import (srfi srfi-9))
(import (rnrs bytevectors))

(import (gcrypt hash))

(import (babelia ulid))
(import (babelia okvs engine))


;; helper

(define (bytevector-append . bvs)
  (let* ((total (let loop ((bvs bvs)
                           (out 0))
                  (if (null? bvs)
                      out
                      (loop (cdr bvs) (+ (bytevector-length (car bvs)) out)))))
         (out (make-bytevector total)))
    (let loop ((bvs bvs)
               (index 0))
      (unless (null? bvs)
        (bytevector-copy! (car bvs) 0 out index (bytevector-length (car bvs)))
        (loop (cdr bvs) (+ index (bytevector-length (car bvs))))))
    out))

;;

(define %sha256->value #vu8(0))
(define %sha256->ulid #vu8(1))
(define %ulid->sha256 #vu8(2))

;; XXX: `prefix` must a list (like in nstore)
(define-record-type <object>
  (%object engine prefix)
  object?
  (engine object-engine)
  (prefix object-prefix))

(define-public (object engine prefix)
  (%object engine (engine-pack engine prefix)))

(define (object->ulid transaction object value)
  (let ((engine (object-engine object)))
    (let* ((value (engine-pack engine value))
           (hash (sha256 value))
           (key (bytevector-append (object-prefix object)
                                    %sha256->ulid
                                    hash)))
      ;; try to get ulid from sha256->ulid subspace
      (let ((out (engine-ref engine transaction key)))
        (if out
            out
            ;; otherwise, create a new identifier and store it.
            (let ((out (ulid)))
              (engine-set! engine transaction
                           (bytevector-append (object-prefix object)
                                               %sha256->value
                                               hash)
                           value)

              (engine-set! engine transaction
                           (bytevector-append (object-prefix object)
                                               %sha256->ulid
                                               hash)
                           out)

              (engine-set! engine transaction
                           (bytevector-append (object-prefix object)
                                               %ulid->sha256
                                               out)
                           hash)

              out))))))

(define (ulid->object transaction object ulid)
  "Retrieve the object with the given ULID. Otherwise return #f"
  (let ((engine (object-engine object)))
    ;; try to find the hash
    (and=>
     (engine-ref engine
                 transaction
                 (bytevector-append (object-prefix object)
                                     %ulid->sha256
                                     ulid))
     (lambda (hash)
       ;; there is a hash, there must be a value
       (let ((value (engine-ref engine transaction
                                (bytevector-append (object-prefix object)
                                                    %sha256->value
                                                    hash))))
         (car (engine-unpack engine value)))))))
