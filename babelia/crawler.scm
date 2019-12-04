(define-module (babelia crawler))


(import (scheme base))

(import (web request))
(import (web uri))
(import (ice-9 match))

(import (babelia pool))
(import (babelia log))
(import (babelia web server))
(import (babelia web helpers))
(import (babelia web static))


(define (template class body)
  `((doctype "html")
    (html
     (head
      (meta (@ (charset "utf-8")))
      (title "babelia")
      (link (@ (rel "stylesheet") (href "/static/normalize.css")))
      (link (@ (rel "stylesheet") (href "/static/main.css"))))
     (body
      (div (@ (id "header")))
      (div (@ (id "wrapper"))
           (div (@ (id "container")
                   (class ,class))
                ,body))
      (div (@ (id "footer")))))))

(define (route/index)
  (sxml->response
   (template "index" "Hello, world!")))

(define (route/api/status)
  (scheme->response 'OK))

(define (router app request body)
  (log-debug "new request" `((method . ,(request-method request))
                             (path . ,(uri->string (request-uri request)))))
  (match (cons (request-method request) (request-path-components request))
    ('(GET) (route/index))
    ('(GET "api" "status") (route/api/status))
    (('GET "static" path ...) (render-static-asset path))
    (_ (not-found (uri-path (request-uri request))))))

(define (router/guard app request body)
  (guard (ex (else
              (log-error "internal error" ex)
              (internal-error)))
    (router app request body)))

(define-public (subcommand-crawler-run app port)
  (pool-init)
  (log-debug "running crawler server on PORT" port)
  (run-server (lambda (request body) (router/guard app request body))
              #:port port))
