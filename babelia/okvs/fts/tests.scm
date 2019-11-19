(define-module (babelia okvs counter tests))

(import (babelia testing))
(import (babelia okvs fts))
(import (babelia okvs engine))
(import (babelia okvs wiredtiger))


(define engine (make-default-engine))

(define fts (make-fts engine 'test-fts 3))


(define %documents '((1 . "After the second world war, the industrial
era has taken earth by storm.  Production of all sorts have
increased. Knowledge worker have much increased in number.")
                     (2 . "The risk to see the industrial age collapse
because of too much production of gas is possible. This is a knowledge
we have and must act upon") ;; minus risk
                     (3 . "Industrial production of knowledge will
strive and prosper once people acknowledge that the vnstore is the
correct tool for that line of work")
                     (4 . "There is no place of hatered speech, racism
and other lack of tolerance.  Earth can only prosper by recognizing
that diversity is a strength.")))

(define-public test-00
  (test
   '()
   (with-directory "wt"
     (let ((okvs (engine-open engine "wt")))
       ;; prepare
       (okvs-in-transaction okvs
         (lambda (transaction)
           (let loop ((documents %documents))
             (fts-index transaction fts (caar documents) (cdar documents))
             (loop (cdr documents)))))
       ;; exec
       (let ((out (fts-query okvs fts "industrial production of knowledge -risk")))
         (engine-close engine okvs)
         ;; check
         out)))))
