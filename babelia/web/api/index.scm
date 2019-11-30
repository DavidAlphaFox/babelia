(define-module (babelia web api index))

(import (scheme base))

(import (babelia app))
(import (babelia log))
(import (babelia pool))
(import (babelia okvs engine))
(import (babelia okvs fts))
(import (babelia web helpers))

(import (ice-9 threads))


(define-public (route/api/index app body)
  (guard (ex ((and (pair? ex) (eq? (car ex) 'babelia/index))
              (bad-request (cdr ex)))
             (else (pk ex)))
    (let* ((body (utf8->string body))
           (url+html (call-with-input-string body read))
           (url (car url+html))
           (html (cdr url+html)))
      (log-trace "indexing" `((url . ,url)))
      (engine-in-transaction (app-engine app) (app-okvs app)
        (lambda (transaction)
          (call-with-values (lambda () (fts-index transaction (app-fts app) body))
            (lambda (uid title preview)
              (rstore-update transaction
                             (app-rstore app)
                             uid
                             (make-document url title preview))))))))
  (scheme->response 'OK))
