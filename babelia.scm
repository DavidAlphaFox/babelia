(import (ice-9 ftw))
(import (ice-9 match))
(import (ice-9 rdelim))
(import (ice-9 threads))

(import (scheme process-context))

(import (babelia app))
(import (babelia log))
(import (babelia pool))
(import (babelia okvs ustore))
(import (babelia okvs rstore))
(import (babelia okvs nstore))
(import (babelia okvs engine))
(import (babelia okvs wiredtiger))
(import (babelia okvs fts))
(import (babelia web))
(import (babelia web api secret))
(import (babelia crawler))


(log-toggle!)

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

(define rstore (make-rstore engine '(rstore) <document>))

(define nstore (nstore engine '(nstore) '(uid key value)))

(define fts (make-fts engine
                      ustore
                      '(fts)
                      10 ;; return top 10 results
                      pool-apply
                      pool-for-each-par-map))

;; TODO: use app everywhere
(define directory (cadr (program-arguments)))
(define okvs (engine-open engine directory %config))
(define app (make-app #f engine okvs ustore rstore nstore fts))

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
  (reverse (fts-stem-counter okvs fts)))


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
  (let ((stems (string-split (call-with-input-file filename read-string) #\newline)))
    (fts-stem-stop-update okvs fts stems)))

(define (stem-stop-show directory)
  (for-each print (fts-stem-stop-ref okvs fts)))

(match (cddr (program-arguments))
  (`("benchmark" . ,keywords)
   (pk "average in milliseconds" (benchmark directory (string-join keywords " "))))
  (`("word" "counter")
   (for-each print (fts-word-counter okvs fts)))
  (`("stem" "counter")
   (for-each print (fts-stem-counter okvs fts)))
  (`("stem" "stop" "guess" ,milliseconds) (stem-stop-guess directory (string->number milliseconds)))
  (`("stem" "stop" "show") (stem-stop-show directory))
  (`("stem" "stop" "update" ,filename) (stem-stop-update directory filename))
  ;; TODO: eventually all commands must start with a directory and ends with a rest.
  (`("web" "api" "secret" "generate" . ,args) (subcommand-secret-generate directory args))
  (`("web" "run") (subcommand-web-run app))
  (`("crawler" "run" ,port) (subcommand-crawler-run app (string->number port)))
  (`("crawler" "add" ,remote ,url) (subcommand-crawler-add app remote url))
  (`("index" ,filename)
   (let ((body (call-with-input-file filename read-string)))
     (engine-in-transaction (app-engine app) (app-okvs app)
       (lambda (transaction)
         (call-with-values (lambda () (fts-index transaction (app-fts app) body))
           (lambda (uid title preview)
             (rstore-update transaction
                            (app-rstore app)
                            uid
                            (make-document filename title preview)))))))))

(engine-close engine okvs)
