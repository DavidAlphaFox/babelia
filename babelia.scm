#!/bin/sh
# -*- scheme -*-
exec guile -L $(pwd) -e '(@ (babelia) main)' -s "$0" "$@"
!#
(define-module (babelia))

(import (ice-9 ftw))
(import (ice-9 match))
(import (ice-9 rdelim))
(import (ice-9 threads))

(import (babelia app))
(import (babelia html2text))
(import (babelia okvs ustore))
(import (babelia okvs rstore))
(import (babelia okvs engine))
(import (babelia okvs wiredtiger))
(import (babelia okvs fts))
(import (babelia web))
(import (babelia web api secret))


;; TODO: rework the config to include eviction trigger and eviction
;; target, max number of thread set to the count of cpu core:
;;
;;   https://source.wiredtiger.com/3.2.0/tune_cache.html
;;
;; Check statistics.
(define %config `((cache . ,(* 5 1024 1024))
                  (wal . ,(* 1 1024 1024))
                  (mmap . #f)))

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

(define rstore (make-rstore engine '(rstore) <document>))

;; TODO: use app everywhere
(define app (make-app #f engine %config ustore rstore fts))

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
  ;; TODO: speed up the search with divide-and-conquer strategy, that
  ;; is a dichotomy.
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

(define (stem-stop-update directory filename)
  (let ((okvs (engine-open engine directory %config))
        (stems (string-split (call-with-input-file filename read-string) #\newline)))
    (fts-stem-stop-update okvs fts stems)
    (engine-close engine okvs)))

(define (stem-stop-show directory)
  (let ((okvs (engine-open engine directory %config)))
    (for-each print (fts-stem-stop-ref okvs fts))
    (engine-close engine okvs)))

(define-public (main args)
  (match (cdr args)
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
    (`("stem" "stop" "guess" ,directory ,milliseconds) (stem-stop-guess directory (string->number milliseconds)))
    (`("stem" "stop" "show" ,directory) (stem-stop-show directory))
    (`("stem" "stop" "update" ,directory ,filename) (stem-stop-update directory filename))
    ;; TODO: eventually all commands must start with a directory and ends with a rest.
    (`(,directory "web" "api" "secret" "generate" . ,args)
     (subcommand-secret-generate directory args))
    (`(,directory "web" "run") (subcommand-web-run #f))))
