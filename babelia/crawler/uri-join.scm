(define-module (babelia crawler uri-join))

(import (web uri))
(import (scheme base))


(define-public (uri-domain uri)
  (if (uri-port uri)
      (string-append (symbol->string (uri-scheme uri))
                     "://"
                     (uri-host uri)
                     ":"
                     (number->string (uri-port uri)))
      (string-append (symbol->string (uri-scheme uri))
                     "://"
                     (uri-host uri))))

(define (uri-canonical-path url)
  (let ((path* (string-split (uri-path (string->uri url)) #\/)))
    (let loop ((path path*)
               (out '()))
      (cond
       ((null? path) (string-append (uri-domain (string->uri url))
                                    "/"
                                    (string-join (reverse out) "/")
                                    (if (string-suffix? "/" url)
                                        "/"
                                        "")))
       ;; ignore empty string component
       ((string-null? (car path)) (loop (cdr path) out))
       ((string=? (car path) ".") (loop (cdr path) out))
       ;; ignore double dots if the OUT is emtpy. This is a faulty
       ;; href...
       ((and (string=? (car path) "..")
             (null? out))
        (loop (cdr path) out))
       ;; Otherwise, remove one component from out
       ((string=? (car path) "..")
        (loop (cdr path) (cdr out)))
       ;; fail!
       (else (loop (cdr path) (cons (car path) out)))))))

(define-public (uri-join url href)
  (uri-canonical-path (cond
                       ;; it is a full url, return it as-is.
                       ((string-prefix? "http" href) href)
                       ;; same protocol url, prefix the correct protocol
                       ((string-prefix? "//" href)
                        (if (string-prefix? "http://" url)
                            (string-append "http:" href)
                            (string-append "https:" href)))
                       ;; HREF is absolute, suffix domain from URL with HREF
                       ((string-prefix? "/" href)
                        (string-append (uri-domain url) href))
                       ;; ./foo/bar/baz and foo/bar/baz
                       ((string-suffix? "/" (uri->string url))
                        (string-append (uri->string url) href))
                       ;; similar case where URL does not end with a
                       ;; slash, go one step up the hierarchy with
                       ;; dirname.
                       (else (string-append (uri-domain url)
                                            "/"
                                            (dirname (uri-path url))
                                            "/"
                                            href)))))
