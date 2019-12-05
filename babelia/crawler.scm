(define-module (babelia crawler))

(import (scheme assume))
(import (scheme base))
(import (scheme list))
(import (scheme generator))

(import (web request))
(import (web client))
(import (web response))
(import (web uri))
(import (ice-9 match))
(import (only (sxml xpath) sxpath))

(import (babelia fash))
(import (babelia app))
(import (babelia pool))
(import (babelia log))
(import (babelia htmlprag))
(import (babelia okvs engine))
(import (babelia okvs nstore))
(import (babelia okvs ulid))
(import (babelia web server))
(import (babelia web helpers))
(import (babelia web static))

(import (babelia crawler uri-join))


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
  (call-with-values (lambda ()
                      (http-post (string-append remote "/api/index")
                                 #:body (call-with-output-string
                                         (lambda (p) (write (cons (uri->string url)
                                                                  document)
                                                            p)))))
    (lambda (response _)
      (assume (= (response-code response) 200)))))

(define (add-single-page app remote url)
  (log-debug "add single page URL at REMOTE" `((url . ,(uri->string url))
                                               (remote . ,remote)))
  (let ((body (get url)))
    (index remote url body)
    (done app (uri->string url))
    body))

(define extract-href (sxpath '(// a @ href *text*)))

(define (unsupported-href href)
  (not (or (string-prefix? "#" href)
           (string-prefix? "mailto:" href))))

(define (done/transaction transaction nstore url)
  ;; move URL to done in the todo.
  (log-debug "move URL to done" `((url . ,url)))
  (let ((uid (generator->list
              (nstore-select transaction nstore (list (nstore-var 'uid)
                                                      'todo/url
                                                      url)))))
    (when (not (null? uid))
      (let ((uid (fash-ref (car uid) 'uid)))
        (nstore-delete! transaction nstore (list uid
                                                 'todo/url
                                                 url))
        (nstore-delete! transaction nstore (list uid
                                                 'todo/done
                                                 #f))))
    (let ((uid (ulid)))
      (nstore-add! transaction nstore (list uid
                                            'todo/url
                                            url))
      (nstore-add! transaction nstore (list uid
                                            'todo/done
                                            #t)))))

(define (done app url)
  (engine-in-transaction (app-engine app) (app-okvs app)
    (lambda (transaction)
      (done/transaction transaction (app-nstore app) url))))

(define (todo/transaction transaction nstore url)
  ;; add URL to the todo only if it is not already there.
  (let ((todo/url (generator->list
                   (nstore-select transaction nstore (list (nstore-var 'uid)
                                                           'todo/url
                                                           url)))))
    (when (null? todo/url)
      (let ((uid (ulid)))
        (log-debug "adding URL to todo" `((url . ,url)))
        (nstore-add! transaction nstore (list uid
                                              'todo/url
                                              url))
        (nstore-add! transaction nstore (list uid
                                              'todo/done
                                              #f))))))

(define (todo app url)
  (engine-in-transaction (app-engine app) (app-okvs app)
    (lambda (transaction)
      (todo/transaction transaction (app-nstore app) url))))

(define (add-domain app remote domain)
  (log-info "add DOMAIN at REMOTE" `((domain . ,domain)
                                     (remote . ,remote)))
  (let* ((body (add-single-page app remote (string->uri domain)))
         (html (html->sxml body))
         (hrefs (extract-href html))
         (hrefs (filter unsupported-href hrefs))
         (urls (map (lambda (href) (uri-join domain href)) hrefs)))
    (let loop ((urls urls))
      (unless (null? urls)
        (when (string-prefix? domain (car urls))
          (todo app (car urls)))
        (loop (cdr urls))))))

(define-public (subcommand-crawler-add app remote url)
  (log-debug "subcommand-crawler-add")
  (if (not (valid? url))
      (log-error "This is not a valid URL...")
      (let ((url (string->uri url)))
        (if (or (string=? (uri-path url) "/")
                (string-null? (uri-path url)))
            (add-domain app remote (uri->string url))
            (add-single-page app remote url)))))
