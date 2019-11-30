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
                      10
                      (lambda (thunk) (apply thunk '()))
                      for-each-map))

(define %documents '((1 . "<html><head><title>Knowledge industry</title></head><body>After the second world war, the industrial
era has taken earth by storm.  Production of all sorts have
increased. Knowledge worker have increased. Knowledge industry has
grown amazingly. After the second world war, the industrial
era has taken earth by storm.  Production of all sorts have
increased. Knowledge worker have increased. Knowledge industry has
grown amazingly.</body></html>")
                     (2 . "<html><head><title>Apocalypse</title></head><body>The risk to see the industrial age collapse
because of too much production of gas is possible. This is a knowledge
we have and must act upon. The risk to see the industrial age collapse
because of too much production of gas is possible. This is a knowledge
we have and must act upon. The risk to see the industrial age collapse
because of too much production of gas is possible. This is a knowledge
we have and must act upon.</body></html>") ;; minus risk
                     (3 . "<html><head><title>vnstore</vnstore></head><body>Industrial production of knowledge will
strive and prosper once people acknowledge that the vnstore is the
correct tool for that line of work. Industrial production of knowledge will
strive and prosper once people acknowledge that the vnstore is the
correct tool for that line of work. Industrial production of knowledge will
strive and prosper once people acknowledge that the vnstore is the
correct tool for that line of work. </body></html>")
                     (4 . "<html><head><title>diversity</title></head><body>There is no place of hatered speech, racism
and other lack of tolerance.  One can not be tolerant with intolerance.  Earth can only prosper by recognizing
that diversity is a strength. There is no place of hatered speech, racism
and other lack of tolerance.  One can not be tolerant with intolerance.  Earth can only prosper by recognizing
that diversity is a strength.There is no place of hatered speech, racism
and other lack of tolerance.  One can not be tolerant with intolerance.  Earth can only prosper by recognizing
that diversity is a strength.</body></html>") ;; keywords not found.
                     (5 . "<html><head><title>Man of knowledge</title></head><body>“The man of knowledge must be able not only to love his enemies but also to hate his friends.” ― Friedrich Nietzsche “The man of knowledge must be able not only to love his enemies but also to hate his friends.” ― Friedrich Nietzsche “The man of knowledge must be able not only to love his enemies but also to hate his friends.” ― Friedrich Nietzsche</body></html>")
                     ;; the following items are added to bump the
                     ;; frequency of industrial and production to make
                     ;; sure knowledge stem is the most discriminant.
                     (6 . "<html><head><title>industrial production</title></head><body>industrial production industrial production industrial production industrial production industrial production industrial production industrial production industrial production industrial production industrial production industrial production industrial production industrial production industrial production industrial production industrial production industrial production</body></html>")
                     (7 . "<html><head><title>industrial production</title></head><body>industrial production industrial production industrial production industrial production industrial production industrial production industrial production industrial production industrial production industrial production industrial production industrial production industrial production industrial production industrial production industrial production industrial production industrial production industrial production</body></html>")))

(define-public test-00
  (test
   '(3 1)
   (with-directory "wt"
     (let ((okvs (engine-open engine "wt")))
       (let ((alist ;; prepare
              (okvs-in-transaction okvs
                (lambda (transaction)
                  (let loop ((documents %documents)
                             (out '()))
                    (if (null? documents)
                        out
                        (loop (cdr documents)
                              (cons (cons (fts-index transaction fts (cdar documents))
                                          (caar documents))
                                    out))))))))

         ;; exec
         (let ((out (fts-query okvs fts "industrial production knowledge -risk")))
           (engine-close engine okvs)
           ;; check
           (map (lambda (x) (cdr (assoc (car x) alist))) out)))))))
