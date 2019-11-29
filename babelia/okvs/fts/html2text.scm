(define-module (babelia okvs fts html2text))

(import (babelia htmlprag))


(define (element->string tag attr elements port)
  (cond
   ((eq? tag 'script) *unspecified*)
   ((eq? tag 'head) *unspecified*)
   (else (map (lambda (element) (%sxml->text element port)) elements))))

(define (%sxml->text tree port)
  (cond
   ((pair? tree)
    (if (symbol? (car tree))
        ;; An element.
        (let ((tag (car tree)))
          (case tag
            ((*TOP*)
             (%sxml->text (cdr tree) port))
            ((*ENTITY*) *unspecified*)
            ((*DECL*) *unspecified*)
            ((*PI*) *unspecified*)
            (else
             (let* ((elems (cdr tree))
                    (attrs (and (pair? elems) (pair? (car elems))
                                (eq? '@ (caar elems))
                                (cdar elems))))
               (element->string tag attrs (if attrs (cdr elems) elems) port)))))
        ;; A nodelist.
        (for-each (lambda (x) (%sxml->text x port)) tree)))
   ((string? tree)
    (display tree port) (display " " port))
   ((null? tree) *unspecified*)
   ((not tree) *unspecified*)
   ((eqv? tree #t) *unspecified*)
   ((procedure? tree)
    (with-output-to-port port tree))
   (else
    (display tree port))))

(define-public html->text
  (case-lambda
    ((string) (html->text string (current-output-port)))
    ((string port) (%sxml->text (html->sxml string) port))))
