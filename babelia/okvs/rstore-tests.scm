(define-module (babelia okvs rstore-tests))

(import (babelia testing))
(import (srfi srfi-9))
(import (babelia okvs engine))
(import (babelia okvs rstore))
(import (babelia okvs wiredtiger))


(define engine (make-default-engine))

(define-record-type <test-rtd>
  (make-test-rtd value)
  test-rtd?
  (value test-rtd-value))

(define rstore (make-rstore engine '(13 37) <test-rtd>))

(define-public test-00
  (test  ;; insert, and ref
   #t
   (with-directory "wt"
     (let ((okvs (engine-open engine "wt")))
       (let* ((expected (make-test-rtd 42))
              (uid (rstore-insert okvs rstore expected))
              (actual (rstore-ref okvs rstore uid))
              (out (equal? expected actual)))
         (engine-close engine okvs)
         out)))))

(define-public test-01
  (test  ;; insert, update, and ref
   #t
   (with-directory "wt"
     (let ((okvs (engine-open engine "wt")))
       (let* ((initial (make-test-rtd 42))
              (uid (rstore-insert okvs rstore initial))
              (expected (make-test-rtd 43))
              (_ (rstore-update okvs rstore uid expected))
              (actual (rstore-ref okvs rstore uid))
              (out (equal? expected actual)))
         (engine-close engine okvs)
         out)))))

(define-public test-02
  (test  ;; ref unknown uid
   #f
   (with-directory "wt"
     (let* ((okvs (engine-open engine "wt"))
            (out (rstore-ref okvs rstore #vu8(#xBA #xBE))))
       (engine-close engine okvs)
       out))))


(define-public test-03
  (test  ;; insert, delete, and ref
   #f
   (with-directory "wt"
     (let ((okvs (engine-open engine "wt")))
       (let* ((expected (make-test-rtd 42))
              (uid (rstore-insert okvs rstore expected))
              (_ (rstore-delete okvs rstore uid))
              (out (rstore-ref okvs rstore uid)))
         (engine-close engine okvs)
         out)))))
