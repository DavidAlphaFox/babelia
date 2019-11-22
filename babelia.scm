#!/bin/sh
# -*- scheme -*-
exec guile -L $(pwd) -e '(@ (babelia) main)' -s "$0" "$@"
!#
(define-module (babelia))

(import (ice-9 ftw))
(import (ice-9 match))
(import (ice-9 rdelim))
(import (ice-9 threads))
(import (babelia html2text))
(import (babelia okvs ustore))
(import (babelia okvs engine))
(import (babelia okvs wiredtiger))
(import (babelia okvs fts))
(import (babelia web))

(define (current-milliseconds)
  (let ((seconds+microseconds (gettimeofday)))
    (+ (* (car seconds+microseconds) (expt 10 3))
       (round (/ (cdr seconds+microseconds) (expt 10 3))))))

(define engine (make-default-engine))

(define ustore (make-ustore engine '(ustore)))

(define (for-each-map sproc pproc lst)
  (n-for-each-par-map 6 sproc pproc lst))

(define fts (make-fts engine
                      ustore
                      '(fts)
                      10 ;; return top 10 results
                      (lambda (thunk) (apply thunk '()))
                      for-each-map))

(define counter 0)

(define (for-each-html proc directory)
  (ftw directory
       (lambda (filename statinfo flag)
         (case flag
           ((regular)
            (when (string-suffix? ".html" filename)
              (set! counter (+ counter 1))
              (when (= (modulo counter 1000) 0)
                (pk counter))
              (proc filename))
            #t)
           ((directory) #t)
           (else #t)))))

(define (index/transaction transaction filename)
  (let* ((html (call-with-input-file filename read-string))
         (text (call-with-output-string (lambda (port) (html->text html port)))))
    (fts-index transaction fts filename text)))

(define %config `((cache . ,(* 5 1024 1024))
                  (wal . ,(* 1 1024 1024))
                  (mmap . #f)))


(define (index directory)
  (define okvs (engine-open engine directory %config))
  (pk "indexing:" directory)
  (for-each-html (lambda (filename)
                   (engine-in-transaction engine okvs
                     (lambda (transaction)
                       (index/transaction transaction filename))))
                   directory)
  (engine-close engine okvs))

(define (search directory query)
  (pk "search for:" query)
  (let* ((okvs (engine-open engine directory %config))
         (start (current-milliseconds))
         (results (fts-query okvs fts query)))
    (pk "query time in milliseconds" (- (current-milliseconds) start))
    (for-each pk results)
    (engine-close engine okvs)))

(define-public (main args)
  (match (cdr args)
    (`("index" ,directory) (index directory))
    (`("search" ,directory . ,keywords) (search directory (string-join keywords " ")))
    (`("web") (run))))
