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

(define (current-milliseconds)
  (let ((seconds+microseconds (gettimeofday)))
    (+ (* (car seconds+microseconds) (expt 10 3))
       (round (/ (cdr seconds+microseconds) (expt 10 3))))))

(define engine (make-default-engine))

(define ustore (make-ustore engine '(ustore)))

(define (for-each-map sproc pproc lst)
  (n-for-each-par-map (- (current-processor-count) 1) sproc pproc lst))

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


;; TODO: rework the config to include eviction trigger and eviction
;; target, max number of thread set to the count of cpu core:
;;
;;   https://source.wiredtiger.com/3.2.0/tune_cache.html
;;
;; Check statistics.
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

(define (benchmark-once okvs fts query)
  (let* ((start (current-milliseconds)))
    (fts-query okvs fts query)
    (- (current-milliseconds) start)))

(define (benchmark directory query)
  (let* ((okvs (engine-open engine directory %config)))
    ;; warm the cache
    (fts-query okvs fts query)
    ;; wait the cache to be setup ready
    (sleep 2)
    ;; benchmark
    (let ((out (let loop ((count 0)
                          (sum 0))
                 (if (= count 10)
                     (exact->inexact (/ sum 10))
                     (loop (+ count 1) (+ sum (benchmark-once okvs fts query)))))))
      (engine-close engine okvs)
      out)))

(define (print obj)
  (display obj) (newline))

(define (stems-ref directory)
  (let* ((okvs (engine-open engine directory %config))
         (out (fts-stem-counter okvs fts)))
      (engine-close engine okvs)
      (reverse out)))

(define (stem-stop-guess directory min)
  (let ((stems (stems-ref directory)))
    (let loop ((stems stems)
               (out '()))
      (if (null? stems)
          (error 'babelia "oops")
          (let ((new (benchmark directory (caar stems))))
            (pk new)
            (if (< new min)
                (for-each (compose print car) out)
                (loop (cdr stems) (cons (car stems) out))))))))

(define-public (main args)
  (match (cdr args)
    (`("index" ,directory) (index directory))
    (`("search" ,directory . ,keywords) (search directory (string-join keywords " ")))
    (`("benchmark" ,directory . ,keywords)
     (pk "average in milliseconds" (benchmark directory (string-join keywords " "))))
    (`("word" "counter" ,directory)
       (let* ((okvs (engine-open engine directory %config)))
         (for-each print (fts-word-counter okvs fts))
         (engine-close engine okvs)))
    (`("stem" "counter" ,directory)
       (let* ((okvs (engine-open engine directory %config)))
         (for-each print (fts-stem-counter okvs fts))
         (engine-close engine okvs)))
    (`("stem" "stop" "guess" ,directory ,milliseconds) (stem-stop-guess directory (string->number milliseconds)))))
