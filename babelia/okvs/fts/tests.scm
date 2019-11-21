(define-module (babelia okvs fts tests))

(import (babelia testing))
(import (babelia okvs fts))
(import (babelia okvs engine))
(import (babelia okvs ustore))
(import (babelia okvs wiredtiger))


(define engine (make-default-engine))

(define ustore (make-ustore engine '(test-ustore-prefix)))

(define (for-each-map sproc pproc lst)
  (for-each sproc (map pproc lst)))

(define fts (make-fts engine
                      ustore
                      '(test-fts-prefix)
                      1
                      (lambda (thunk) (apply thunk '()))
                      for-each-map))

(define %documents '((1 . "After the second world war, the industrial
era has taken earth by storm.  Production of all sorts have
increased. Knowledge worker have increased. Knowledge industry has
grown amazingly.")
                     (2 . "The risk to see the industrial age collapse
because of too much production of gas is possible. This is a knowledge
we have and must act upon") ;; minus risk
                     (3 . "Industrial production of knowledge will
strive and prosper once people acknowledge that the vnstore is the
correct tool for that line of work")
                     (4 . "There is no place of hatered speech, racism
and other lack of tolerance.  Earth can only prosper by recognizing
that diversity is a strength."))) ;; keywords not found.

(define-public test-00
  (test
   '((1 . 4))
   (with-directory "wt"
     (let ((okvs (engine-open engine "wt")))
       ;; prepare
       (okvs-in-transaction okvs
         (lambda (transaction)
           (let loop ((documents %documents))
             (unless (null? documents)
               (fts-index transaction fts (caar documents) (cdar documents))
               (loop (cdr documents))))))
       ;; exec
       (let ((out (fts-query okvs fts "industrial production knowledge -risk")))
         (engine-close engine okvs)
         ;; check
         out)))))
