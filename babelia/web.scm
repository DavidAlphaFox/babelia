(define-module (babelia web))

(import (scheme base))

(import (web request))
(import (web uri))
(import (ice-9 match))

(import (babelia log))
(import (babelia app))
(import (babelia web server))
(import (babelia web helpers))
(import (babelia web static))
(import (babelia web decode))
(import (babelia web api index))
(import (babelia web api search))
(import (babelia pool))
(import (babelia okvs rstore))
(import (babelia okvs engine))
(import (babelia okvs fts))


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

(define (route/api/status)
  (scheme->response 'OK))

(define (route/index/transaction transaction app query)
  (let loop ((hits (fts-query (app-okvs app) (app-fts app) query)))
    (map (lambda (hit) (cons (rstore-ref transaction (app-rstore app) (car hit))
                             (cdr hit)))
         hits)))

(define (route/index/template hits)
  (template
   "index"
   (map (lambda (hit) `(a (@ (href ,(document-url (car hit))))
                          (p ,(document-title (car hit)))
                          (p ,(document-preview (car hit)))))
        hits)))

(define (route/index app query)
  (if (not query)
      (sxml->response (route/index/template '()))
      (let ((query (cadr (assoc "query" (decode query)))))
        (log-debug "web search" query)
        (sxml->response
         (route/index/template (pool-apply
                                (lambda ()
                                  (engine-in-transaction (app-engine app) (app-okvs app)
                                    (lambda (tx)
                                      (route/index/transaction tx app query))))))))))

(define (router app request body)
  (log-debug "new request" `((method . ,(request-method request))
                             (path . ,(uri->string (request-uri request)))))
  (match (cons (request-method request) (request-path-components request))
    ('(GET) (route/index app (uri-query (request-uri request))))
    ('(GET "api" "status") (route/api/status))
    ('(POST "api" "index") (route/api/index app body))
    ('(GET "api" "search") (route/api/search app (uri-query (request-uri request))))
    (('GET "static" path ...) (render-static-asset path))
    (_ (not-found (uri-path (request-uri request))))))

(define (router/guard app request body)
  (guard (ex (else
              (log-error "internal error" ex)
              (internal-error)))
    (router app request body)))

(define-public (subcommand-web-run app)
  (pool-init)
  (log-info "web server starting at port 8080...")
  (run-server (lambda (request body) (router/guard app request body))))
