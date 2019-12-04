(define-module (babelia crawler))


(import (scheme base))
(import (scheme list))

(import (web request))
(import (web client))
(import (web response))
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

;; add url

(define (head url)
  (call-with-values (lambda () (http-get url  #:decode-body? #f #:streaming? #t))
    (lambda (response _) response)))

(define (valid? url)
  (let ((response (head url)))
    (and (= (response-code response) 200)
         ;; somekind of html content
         (and=> (assq 'content-type (response-headers response))
                (lambda (content-type)
                  (any (lambda (item)
                         (let ((item (if (symbol? item) (symbol->string item) item)))
                           (string-contains item "text/html")))
                       (cdr content-type))))
         ;; at most 5MB
         (and=> (assq 'content-length (response-headers response))
                (lambda (content-length)
                  (< (cdr content-length) (* 5 1024 1024)))))))

(define (get url)
  (call-with-values (lambda () (http-get url  #:decode-body? #t #:streaming? #f))
    (lambda (_ body) body)))

(define (index remote url document)
  (http-post (string-append remote "/api/index")
             #:body (call-with-output-string (lambda (p) (write (cons (uri->string url)
                                                                      document)
                                                                p)))))

(define (add-single-page remote url)
  (let ((body (get url)))
    (index remote url body)))


(define-public (subcommand-crawler-add remote url)
  (if (not (valid? url))
      (log-error "This is not a valid URL...")
      (let ((url (string->uri url)))
        (if (or (string=? (uri-path url) "/")
                (string-null? (uri-path url)))
            (add-single-page remote url)
            (add-domain remote url)))))
