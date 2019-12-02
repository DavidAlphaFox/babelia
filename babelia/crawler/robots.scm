;; Wanna be robots.txt parser. See the specification at:
;;
;;  https://www.robotstxt.org/orig.html
;;
(define-module (babelia crawler robots))

(import (scheme base))
(import (scheme list))


(define (field-split string)
  (let loop ((string (string->list string))
             (name '()))
    (if (null? string)
        (values #f #f)
        (if (char=? (car string) #\:)
            (list (list->string (reverse name)) (list->string (cdr string)))
            (loop (cdr string) (cons (car string) name))))))

(define-public (robots.txt-sitemaps string)
  "Parse a robots.txt return sitemaps.xml"
  (let loop ((lines (string-split string #\newline))
             (out '()))
    (if (null? lines)
        out
        (let* ((line (car lines))
               (line (string-trim-both line))
               (index (or (string-index line #\#) (string-length line)))
               (line (substring line 0 index)))
          (if (zero? (string-length line))
              ;; ignore whitespace or comments lines
              (loop (cdr lines) out)
              (let* ((field (map string-trim-both (field-split line)))
                     (name (string-downcase (car field)))
                     (value (cadr field)))
                (cond
                 ((string=? name "sitemap")
                  (loop (cdr lines) (cons value out)))
                 (else ;; ignore everything else
                  (loop (cdr lines) out)))))))))

(define (robots.txt->scm string)
  "Parse a robots.txt file given as STRING"
  ;; This will try to not loose information, but (most?) robots.txt do
  ;; not conform to the original specification.  In particular, in the
  ;; wild there is wild stuff like 'request-rate' or robot specific
  ;; sitemaps.
  ;;
  ;; The idea of robots.txt format as parsed by this procedure is the
  ;; following:
  ;;
  ;; - Line based
  ;;
  ;; - field names are not sensitive to case.
  ;;
  ;; - user-agent field name will start a specific crawler
  ;;   configuration.
  ;;
  ;; - Keep track of 'crawl-delay' as an integer of seconds
  ;;
  ;; - Keep track of 'disallow' as a prefix of a path
  ;;
  (let loop ((lines (string-split string #\newline))
             (current '(default #f ()))
             (out '()))
    (if (null? lines)
        (delete-duplicates (cons current out)
                           (lambda (x y) (equal? (car x) (car y))))
        (let* ((line (car lines))
               (line (string-trim-both line))
               (index (or (string-index line #\#) (string-length line)))
               (line (substring line 0 index)))
          (if (zero? (string-length line))
              ;; ignore whitespace or comments lines
              (loop (cdr lines) current out)
              (let* ((field (map string-trim-both (field-split line)))
                     (name (string-downcase (car field)))
                     (value (cadr field)))
                (cond
                 ((string=? name "user-agent")
                  (loop (cdr lines)
                        (list value #f '())
                        (cons current out)))
                 ((string=? name "disallow")
                  (let* ((index (or (string-index value #\*) (string-length value)))
                         (value (substring value 0 index)))
                    (loop (cdr lines)
                          (list (car current)
                                (cadr current)
                                (cons value (caddr current)))
                          out)))
                 ((string=? name "crawl-delay")
                  (loop (cdr lines)
                        (list (car current)
                              (string->number value)
                              (caddr current))
                        out))
                 (else ;; ignore everything else
                  (loop (cdr lines) current out)))))))))

(define-record-type <robots.txt>
  (%make-robots.txt delay disallow)
  robots.txt?
  (delay robots.txt-delay)
  (disallow robots.txt-disallow))

(export robots.txt-delay)

(define-public (make-robots.txt user-agent string)
  (let loop ((lst (robots.txt->scm string)))
    (if (null? lst)
        (%make-robots.txt #f '())
        (let ((item (car lst)))
          (if (or
               (eq? (car item) 'default)
               (string=? (car item) user-agent)
               (string=? (car item) "*"))
              (%make-robots.txt (cadr item) (caddr item))
              (loop (cdr lst)))))))

(define-public (robots.txt-allow? robots.txt path)
  (let loop ((disallow (robots.txt-disallow robots.txt)))
    (if (null? disallow)
        #t
        (if (string-prefix? (car disallow) path)
            #f
            (loop (cdr disallow))))))
