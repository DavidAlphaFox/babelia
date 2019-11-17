(define-module (babelia web))

(import (web request))
(import (web uri))
(import (ice-9 match))

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

(define (router request body)
  (match (cons (request-method request) (request-path-components request))
    ('(GET) (route/index))
    (('GET "static" path ...) (render-static-asset path))
    (_ (not-found (uri-path (request-uri request))))))

(define-public (run)
  (run-server router))
