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
(define-module (babelia okvs wiredtiger tests))

(export test-00
        test-01
        test-02
        test-03
        test-04
        test-05
        test-06
        test-07
        )

(import (babelia testing)
        (babelia generator)
        (babelia okvs wiredtiger)
        (babelia okvs engine))

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

(define test-00
  (test
   #t
   (with-directory "wt"
     (let ((okvs (engine-open engine "wt")))
       (engine-close engine okvs)
       #t))))

(define test-01
  (test
   #vu8(1 2 3 42)
   (with-directory "wt"
     (let ((okvs (engine-open engine "wt")))
       ;; set
       (engine-in-transaction engine okvs
         (lambda (transaction)
           (engine-set! engine transaction #vu8(13 37) #vu8(1 2 3 42))))
       ;; get
       (let ((out (engine-in-transaction engine okvs
                    (lambda (transaction)
                      (engine-ref engine transaction #vu8(13 37))))))
         (engine-close engine okvs)
         out)))))

(define test-02
  (test
   #vu8(42)
   (with-directory "wt"
     (let ((okvs (engine-open engine "wt")))
       ;; set
       (engine-in-transaction engine okvs
         (lambda (transaction)
           (engine-set! engine transaction #vu8(13 37) #vu8(1 2 3 42))))
       ;; overwrite
       (engine-in-transaction engine okvs
         (lambda (transaction)
           (engine-set! engine transaction #vu8(13 37) #vu8(42))))
       ;; get
       (let ((out (engine-in-transaction engine okvs
                    (lambda (transaction)
                      (engine-ref engine transaction #vu8(13 37))))))
         (engine-close engine okvs)
         out)))))

(define test-03
  (test
   (list (cons #vu8(20 16) #vu8(2)) (cons #vu8(20 17) #vu8(3)))
   (with-directory "wt"
     (let ((okvs (engine-open engine "wt")))
       ;; set
       (engine-in-transaction engine okvs
         (lambda (transaction)
           (engine-set! engine transaction #vu8(20 18) #vu8(4))
           (engine-set! engine transaction #vu8(20 16) #vu8(2))
           (engine-set! engine transaction #vu8(20 15) #vu8(1))
           (engine-set! engine transaction #vu8(20 19) #vu8(5))
           (engine-set! engine transaction #vu8(20 17) #vu8(3))))
       ;; get
       (let ((out (engine-in-transaction engine okvs
                    (lambda (transaction)
                      (generator->list
                       (engine-range engine transaction #vu8(20 16) #t #vu8(20 18) #f))))))
         (engine-close engine okvs)
         out)))))

(define test-04
  (test
   (list (cons #vu8(20 16) #vu8(2)) (cons #vu8(20 17 01) #vu8(3)))
   (with-directory "wt"
     (let ((okvs (engine-open engine "wt")))
       ;; set
       (engine-in-transaction engine okvs
         (lambda (transaction)
           (engine-set! engine transaction #vu8(20 18) #vu8(4))
           (engine-set! engine transaction #vu8(20 16) #vu8(2))
           (engine-set! engine transaction #vu8(20 15) #vu8(1))
           (engine-set! engine transaction #vu8(20 19) #vu8(5))
           ;; #vu8(20 17 01) lexicographically less than #vu8(20 18)
           (engine-set! engine transaction #vu8(20 17 01) #vu8(3))))
       ;; get
       (let ((out (engine-in-transaction engine okvs
                    (lambda (transaction)
                      (generator->list
                       (engine-range engine transaction #vu8(20 16) #t #vu8(20 18) #f))))))
         (engine-close engine okvs)
         out)))))

(define test-05
  (test
   '((#vu8(20 16) . #vu8(2))
     (#vu8(20 16 1) . #vu8(2))
     (#vu8(20 17) . #vu8(3))
     (#vu8(20 17 1) . #vu8(2)))
   (with-directory "wt"
     (let ((okvs (engine-open engine "wt")))
       ;; set
       (engine-in-transaction engine okvs
         (lambda (transaction)
           (engine-set! engine transaction #vu8(20 17 01) #vu8(2))
           (engine-set! engine transaction #vu8(20 17) #vu8(3))
           (engine-set! engine transaction #vu8(42 42) #vu8(5))
           (engine-set! engine transaction #vu8(01 02) #vu8(1))
           (engine-set! engine transaction #vu8(20 16) #vu8(2))
           (engine-set! engine transaction #vu8(20 16 01) #vu8(2))))
       ;; get
       (let ((out (engine-in-transaction engine okvs
                    (lambda (transaction)
                      (generator->list (engine-prefix-range engine transaction #vu8(20)))))))
         (engine-close engine okvs)
         out)))))

(define test-06
  (test
   '((#vu8(20 17) . #vu8(4))
     (#vu8(20 16 1) . #vu8(3)))
   (with-directory "wt"
     (let ((okvs (engine-open engine "wt")))
       ;; set
       (engine-in-transaction engine okvs
         (lambda (transaction)
           (engine-set! engine transaction #vu8(20 17 01) #vu8(5))
           (engine-set! engine transaction #vu8(20 17) #vu8(4))
           (engine-set! engine transaction #vu8(42 42) #vu8(6))
           (engine-set! engine transaction #vu8(01 02) #vu8(1))
           (engine-set! engine transaction #vu8(20 16) #vu8(2))
           (engine-set! engine transaction #vu8(20 16 01) #vu8(3))))
       ;; get
       (let ((out (engine-in-transaction engine okvs
                    (lambda (transaction)
                      (generator->list (engine-prefix-range engine transaction
                                                            #vu8(20)
                                                            '((offset . 1)
                                                              (limit . 2)
                                                              (reverse? #t))))))))
         (engine-close engine okvs)
         out)))))

(define test-07
  (test
   '()
   (let ((keys '(#vu8(1 42 0 20 2 55 97 98 53 118 54 110 103 113 119 49 117 53 121 111 57 50 104 110 107 105 109 112 105 104 0 21 102 21 103)
                     #vu8(1 42 0 21 1 21 102 21 103 2 55 97 98 53 118 54 110 103 113 119 49 117 53 121 111 57 50 104 110 107 105 109 112 105 104 0)
                     #vu8(1 42 0 21 2 21 103 2 55 97 98 53 118 54 110 103 113 119 49 117 53 121 111 57 50 104 110 107 105 109 112 105 104 0 21 102))))
     (with-directory "wt"
       (let ((okvs (engine-open engine "wt")))
         ;; set
         (engine-in-transaction engine okvs
           (lambda (transaction)
             (let loop ((keys keys))
               (unless (null? keys)
                 (engine-set! engine transaction (car keys) #vu8(2))
                 (loop (cdr keys))))))
         ;; get
         (let* ((prefix #vu8(1 42 0 20 2 57 98 57 55 54 97 104 97 104 50 51 113 110 52 102 121 97 99 49 53 120 99 118 48 100 0))
                (out (engine-in-transaction engine okvs
                       (lambda (transaction)
                         (generator->list (engine-prefix-range engine transaction prefix))))))
           (engine-close engine okvs)
           out))))))
