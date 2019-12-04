(define-module (babelia web api index))

(import (scheme base))

(import (babelia app))
(import (babelia log))
(import (babelia pool))
(import (babelia okvs engine))
(import (babelia okvs fts))
(import (babelia okvs rstore))
(import (babelia web helpers))

(import (ice-9 threads))
(import (babelia okvs wiredtiger))


(define-public (route/api/index app body)
  ((pool-apply
    (lambda ()
      (guard (ex ((and (pair? ex) (eq? (car ex) 'babelia/index))
                  (lambda () (bad-request (cdr ex))))
                 (else (lambda () (log-error "internal error" ex) (internal-error))))
        (let* ((body (utf8->string body))
               (url+html (call-with-input-string body read))
               (url (car url+html))
               (html (cdr url+html)))
          (log-debug "indexing" `((url . ,url)))
          (engine-in-transaction (app-engine app) (app-okvs app)
            (lambda (transaction)
              (call-with-values (lambda () (fts-index transaction (app-fts app) body))
                (lambda (uid title preview)
                  (rstore-update transaction
                                 (app-rstore app)
                                 uid
                                 (make-document url title preview))))))
          (lambda () (scheme->response 'OK))))))))
