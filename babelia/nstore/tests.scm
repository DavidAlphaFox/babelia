;; Copyright © 2019 Amirouche BOUBEKKI <amirouche at hyper dev>
;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use,
;;; copy, modify, merge, publish, distribute, sublicense, and/or
;;; sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following
;;; conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;; OTHER DEALINGS IN THE SOFTWARE.
(define-module (babelia nstore tests))

(import (only (rnrs) guard error))

(import (babelia fash))
(import (babelia generator))
(import (babelia nstore))
(import (babelia okvs engine))
(import (babelia okvs wiredtiger))
(import (babelia testing))



;; helper: (with-directory path body ...)

(define (path-exists? path)
  "Return #true if path is a file or directory.
   #false if it doesn't exists"
  (access? path F_OK))

(define (path-join . rest)
  "Return the absolute path made of REST. If the first item
   of REST is not absolute the current working directory
   will be prepended"
  (let ((path (string-join rest "/")))
    (if (string-prefix? "/" path)
        path
        (string-append (getcwd) "/" path))))

(define (path-split path)
  (let ((parts (string-split path #\/)))
    (if (equal? (car parts) "")
        (cons (string-append "/" (cadr parts)) (cddr parts))
        parts)))

(define (path-mkdir dirpath parents)
  "Create DIRPATH directory and its parents if PARENTS is true"
  (if parents
      (let* ((parts (path-split dirpath))
             (paths (let loop ((dirs (cdr parts))
                               (out (list (car parts))))
                      (if (null? dirs)
                          (reverse out)
                          (loop (cdr dirs) (cons (apply path-join (list (car out) (car dirs))) out))))))
        (and (map (lambda (p) (if (not (path-exists? p)) (mkdir p))) paths) #true))
      (if (not (path-exists? dirpath)) (and (mkdir dirpath) #true))))

(define (path-walk dirpath proc)
  (define dir (opendir dirpath))
  (let loop ()
    (let ((entry (readdir dir)))
      (cond
       ((eof-object? entry))
       ((or (equal? entry ".") (equal? entry "..")) (loop))
       (else (let ((path (path-join dirpath entry)))
               (if (equal? (stat:type (stat path)) 'directory)
                   (begin (path-walk path proc) (loop))
                   (begin (proc path) (loop))))))))
  (closedir dir)
  (proc (path-join dirpath)))

(define (rmtree path)
  (path-walk path (lambda (path)
                    (if (equal? (stat:type (stat path)) 'directory)
                        (rmdir path)
                        (delete-file path)))))

(define-syntax-rule (with-directory path e ...)
  ;; TODO: use guard to delete the directory in case of exception
  (begin
    (when (access? path F_OK)
      (rmtree path))
    (mkdir path)
    (let ((out (begin e ...)))
      (rmtree path)
      out)))

;; tests

(define engine (make-default-engine))

(define (triplestore)
  (nstore engine (list 42 1337) '(uid key value)))

(define-public test-000
  (test ;; empty database
   #f
   (with-directory "wt"
     (let ((okvs (engine-open engine "wt"))
           (triplestore (triplestore)))
       ;; ask
       (let ((out (engine-in-transaction
                   engine okvs
                   (lambda (transaction)
                     (nstore-ask? transaction triplestore '("P4X432" blog/title "hyper.dev"))))))
         (engine-close engine okvs)
         out)))))

(define-public test-001
  (test ;; add and ask triplestore"
   #t
   (with-directory "wt"
     (let ((okvs (engine-open engine "wt"))
           (triplestore (triplestore)))
       ;; add
       (engine-in-transaction
        engine okvs
        (lambda (transaction)
          (nstore-add! transaction triplestore '("P4X432" blog/title "hyper.dev"))))
       ;; ask
       (let ((out
              (engine-in-transaction
               engine okvs
               (lambda (transaction)
                 (nstore-ask? transaction triplestore '("P4X432" blog/title "hyper.dev"))))))
         (engine-close engine okvs)
         out)))))

(define-public test-002
  (test ;; "add, rm and ask triplestore"
   #f
   (with-directory "wt"
     (let ((okvs (engine-open engine "wt"))
           (triplestore (triplestore)))
       (let ((out
              (engine-in-transaction
               engine okvs
               (lambda (transaction)
                 ;; add!
                 (nstore-add! transaction triplestore '("P4X432" blog/title "hyper.dev"))
                 ;; remove!
                 (nstore-delete! transaction triplestore '("P4X432" blog/title "hyper.dev"))
                 ;; ask
                 (nstore-ask? transaction triplestore '("P4X432" blog/title "hyper.dev"))))))
         (engine-close engine okvs)
         out)))))

(define-public test-003
  (test ;; "blog query post titles"
   '("DIY a database" "DIY a full-text search engine")
   (with-directory "wt"
     (let ((okvs (engine-open engine "wt"))
           (triplestore (triplestore)))
       (engine-in-transaction
        engine okvs
        (lambda (transaction)
          ;; add hyper.dev blog posts
          (nstore-add! transaction triplestore '("P4X432" blog/title "hyper.dev"))
          (nstore-add! transaction triplestore '("123456" post/title "DIY a database"))
          (nstore-add! transaction triplestore '("123456" post/blog "P4X432"))
          (nstore-add! transaction triplestore '("654321" post/title "DIY a full-text search engine"))
          (nstore-add! transaction triplestore '("654321" post/blog "P4X432"))
          ;; add dthompson.us blog posts
          (nstore-add! transaction triplestore '("1" blog/title "dthompson.us"))
          (nstore-add! transaction triplestore '("2" post/title "Haunt 0.2.4 released"))
          (nstore-add! transaction triplestore '("2" post/blog "1"))
          (nstore-add! transaction triplestore '("3" post/title "Haunt 0.2.3 released"))
          (nstore-add! transaction triplestore '("3" post/blog "1"))))
       ;; query
       (let ()
         (define query
           (lambda (transaction blog/title)
             (generator->list (nstore-query
                               (nstore-select transaction triplestore
                                              (list (nstore-var 'blog/uid)
                                                    'blog/title
                                                    blog/title))
                               (nstore-where transaction triplestore
                                             (list (nstore-var 'post/uid)
                                                   'post/blog
                                                   (nstore-var 'blog/uid)))
                               (nstore-where transaction triplestore
                                             (list (nstore-var 'post/uid)
                                                   'post/title
                                                   (nstore-var 'post/title)))))))
         (let* ((out (engine-in-transaction engine okvs (lambda (transaction) (query transaction "hyper.dev"))))
                (out (map (lambda (x) (fash-ref x 'post/title)) out)))
           (engine-close engine okvs)
           out))))))

(define-public test-004
  (test ;; "nstore-select limit and offset"
   '("hyperdev.fr")
   (with-directory "wt"
     (let ((okvs (engine-open engine "wt"))
           (triplestore (triplestore)))
       ;; add!
       (nstore-add! okvs triplestore '("P4X432" blog/title "hyper.dev"))
       (nstore-add! okvs triplestore '("P4X433" blog/title "hyperdev.fr"))
       (nstore-add! okvs triplestore '("P4X434" blog/title "hypermove.net"))
       (let ((out (engine-in-transaction
                   engine okvs
                   (lambda (transaction)
                     (generator-map->list
                      (lambda (item) (fash-ref item 'title))
                      (nstore-select transaction triplestore (list (nstore-var 'uid)
                                                                   'blog/title
                                                                   (nstore-var 'title))
                                   `((limit . 1) (offset . 1))))))))
         (engine-close engine okvs)
         out)))))

(define-public test-005
  (test ;; "nstore validation add via hooks"
   #t
   (with-directory "wt"
     (let* ((okvs (engine-open engine "wt"))
            (triplestore (triplestore))
            (hook (nstore-hook-on-add triplestore)))
       (add-hook! hook (lambda (nstore items)
                         (when (string=? (car items) "private")
                           (error 'nstore-hook "private is private" items))))
       ;; replace with babelia testing's test-raise
       (guard (ex (else (engine-close engine okvs) #t))
         (nstore-add! okvs triplestore '("private" private "private"))
         #f)))))

(define-public test-006
  (test ;; "nstore validation delete via hooks"
   #t
   (with-directory "wt"
     (let* ((okvs (engine-open engine "wt"))
            (triplestore (triplestore))
            (hook (nstore-hook-on-delete triplestore)))
       (add-hook! hook (lambda (nstore items)
                         (when (string=? (car items) "private")
                           (error 'nstore-hook "private is private" items))))
       ;; replace with babelia testing's test-raise
       (guard (ex (else (engine-close engine okvs) #t))
         (nstore-delete! okvs triplestore '("private" private "private"))
         #f)))))
