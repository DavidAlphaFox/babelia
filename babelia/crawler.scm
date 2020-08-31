(define-module (babelia crawler))

(import (scheme assume))
(import (scheme base))
(import (scheme list))
(import (scheme generator))
(import (fibers))

(import (web request))
(import (web client))
(import (web response))
(import (web uri))
(import (ice-9 match))
(import (ice-9 iconv))
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

(define (todo-ref transaction nstore domain)
  (let ((urls
         (generator-map->list
          (lambda (x) (fash-ref x 'url))
          (nstore-query
           (nstore-select transaction nstore (list (nstore-var 'url)
                                                   'domain
                                                   domain))
           (nstore-where transaction nstore (list (nstore-var 'url)
                                                  'todo?
                                                  #t))))))
    (if (null? urls)
        #f
        (let ((url (car urls)))
          (domain-touch/transaction transaction nstore (uri-domain (string->uri url)))
          (done/transaction transaction nstore url)
          url))))

(define (crawler-todo-ref/transaction transaction nstore)
  (let ((domains (generator-map->list
                  (lambda (x) (cons (fash-ref x 'domain)
                                    (fash-ref x 'timestamp)))
                  (nstore-select transaction nstore (list (nstore-var 'domain)
                                                          'timestamp
                                                          (nstore-var 'timestamp))))))
    (let loop ((domains domains)
               (out '()))
      (if (null? domains)
          out
          (if (< 1000 (- (current-milliseconds) (cdar domains)))
              (let ((url (todo-ref transaction nstore (caar domains))))
                (if url
                    (loop (cdr domains) (cons url out))
                    (loop (cdr domains) out)))
              (begin
                (log-debug "waiting for DOMAIN" (caar domains))
                (loop (cdr domains) out)))))))

(define (crawler-todo-ref app)
  (engine-in-transaction (app-engine app) (app-okvs app)
    (lambda (transaction)
      (crawler-todo-ref/transaction transaction (app-nstore app)))))

(define (crawl app url remote)
  (add-single-page app remote (string->uri url))
  (domain-touch app (uri-domain (string->uri url))))

(define (crawler-run app remote)
  (let loop0 ()
    (let ((todos (crawler-todo-ref app)))
      (let loop1 ((todos todos))
        (unless (null? todos)
          (spawn-fiber (lambda () (crawl app (car todos) remote)))
          (loop1 (cdr todos)))))
    (sleep 0.1)
    (loop0)))

(define-public (subcommand-crawler-run app port remote)
  (log-debug "running crawler server on PORT" port)
  (run-server (lambda (request body) (router/guard app request body))
              #:port port
              #:init (lambda () (crawler-run app remote))))

;; add url

(define (head url)
  (call-with-values (lambda () (http-head url))
    (lambda (response _) response)))

(define (valid? url)
  (let ((response (head url)))
    (= (response-code response)) 200))

(define (get url)
  (call-with-values (lambda () (http-get url #:decode-body? #f))
    (lambda (_ body) (decode body))))

(define (decode bytevector)
  (bytevector->string bytevector "UTF-8" 'substitute))

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
  (log-debug "move URL to done" `((url . ,url)))
  (let ((todo? (generator->list
                  (nstore-select transaction nstore (list url
                                                          'todo?
                                                          (nstore-var 'todo?))))))
    (if (null? todo?)
        (nstore-add! transaction nstore (list url
                                              'todo?
                                              #f))
        (let ((todo? (fash-ref (car todo?) 'todo?)))
          (if todo?
              (begin
                (nstore-delete! transaction nstore (list url
                                                         'todo?
                                                         #t))
                (nstore-add! transaction nstore (list url
                                                      'todo?
                                                      #f)))
              (log-warn "URL was already done" `((url . ,url))))))))


(define (done app url)
  (engine-in-transaction (app-engine app) (app-okvs app)
    (lambda (transaction)
      (done/transaction transaction (app-nstore app) url))))

(define (todo/transaction transaction nstore url)
  ;; add URL to the todo only if it is not already there.
  (log-debug "todo URL" `((url . ,url)))
  (let ((todo? (generator->list
                (nstore-select transaction nstore (list url
                                                        'todo?
                                                        (nstore-var 'todo?))))))
    (if (null? todo?)
        (begin
          (nstore-add! transaction nstore (list url
                                                'todo?
                                                #t))
          (nstore-add! transaction nstore (list url
                                                'domain
                                                (uri-domain (string->uri url)))))
        (let ((todo? (fash-ref (car todo?) 'todo?)))
          (if todo?
              (log-debug "URL was already todo" `((url . ,url)))
              (log-debug "URL is done, do nothing"))))))



(define (todo app url)
  (engine-in-transaction (app-engine app) (app-okvs app)
    (lambda (transaction)
      (todo/transaction transaction (app-nstore app) url))))

(define (current-milliseconds)
  (let ((seconds+microseconds (gettimeofday)))
    (+ (* (car seconds+microseconds) (expt 10 3))
       (round (/ (cdr seconds+microseconds) (expt 10 3))))))

(define (domain-touch/transaction transaction nstore domain)
  (log-debug "touch DOMAIN" `((domain . ,domain)))
  (let ((timestamp (generator->list
                    (nstore-select transaction nstore (list domain
                                                            'timestamp
                                                            (nstore-var 'timestamp))))))
    (if (null? timestamp)
        (nstore-add! transaction nstore (list domain
                                             'timestamp
                                             (current-milliseconds)))
        ;; else, refresh timestamp
        (let ((timestamp (fash-ref (car timestamp) 'timestamp)))
          (nstore-delete! transaction nstore (list domain
                                                   'timestamp
                                                   timestamp))
          (nstore-add! transaction nstore (list domain
                                               'timestamp
                                               (current-milliseconds)))))))

(define (domain-touch app domain)
  (engine-in-transaction (app-engine app) (app-okvs app)
    (lambda (transaction)
      (domain-touch/transaction transaction (app-nstore app) domain))))

(define (add-domain app remote domain)
  (log-info "add DOMAIN at REMOTE" `((domain . ,domain)
                                     (remote . ,remote)))
  (domain-touch app (uri-domain (string->uri domain)))
  (let* ((body (add-single-page app remote (string->uri domain)))
         (html (html->sxml body))
         (hrefs (extract-href html))
         (hrefs (filter unsupported-href hrefs))
         (urls (map (lambda (href) (uri-join (string->uri domain) href)) hrefs)))
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
