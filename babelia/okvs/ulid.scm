;;
;; Universally Unique Lexicographically Sortable Identifier
;;
;; See https://github.com/ulid/spec.
;;
;; The specification prescribe 48 bits of milliseconds since epoch and
;; 80 bits of randomness. This implementation makes the first 16 bits
;; of randomness configureable on thread by thread basis using a
;; parameter called `thread-index`. It allows to reduce fragmentation
;; for writes that happens in the same thread at the same millisecond.
;;
;; That is very likely to happen when the application starts for the
;; first time and the empty object-store must be populated in a batch.
;;
;; When creating a new POSIX thread, the user must set the
;; `thread-index` parameter to a bytevector of length 2 e.g. using
;; (random-bytes 2).
;;
;; AFAIU, it is not useful to share thread-index value across reboots.
;;
(define-module (babelia okvs ulid))

(import (ice-9 binary-ports))
(import (rnrs bytevectors))
(import (rnrs arithmetic bitwise))

(import (babelia thread))


(define /dev/urandom (open-input-file "/dev/urandom" #:binary #t))

(define-public (random-bytes count)
  "Return a bytevector of length COUNT generated by /dev/urandom"
  (let ((bv (make-bytevector count)))
    (let loop ((index 0))
      (unless (= index count)
        (let ((byte (get-u8 /dev/urandom)))
              (bytevector-u8-set! bv index byte)
              (loop (+ index 1)))))
    bv))

(define (current-milliseconds)
  (let ((seconds+microseconds (gettimeofday)))
    (+ (* (car seconds+microseconds) (expt 10 3))
       (round (/ (cdr seconds+microseconds) (expt 10 3))))))

(define (pack integer)
  "pack INTEGER into a 6 bytes big endian"
  (define count-bytes 6)
  (let ((out (make-bytevector count-bytes)))
    (let loop ((index 0))
      (unless (= index count-bytes)
        (bytevector-u8-set! out
                            index (bitwise-and
                                   (ash integer (- (* (- count-bytes index 1) 8)))
                                   #xFF))
        (loop (+ index 1))))
    out))

(define (bytevector-append . bvs)
  (let* ((total (let loop ((bvs bvs)
                           (out 0))
                  (if (null? bvs)
                      out
                      (loop (cdr bvs) (+ (bytevector-length (car bvs)) out)))))
         (out (make-bytevector total)))
    (let loop ((bvs bvs)
               (index 0))
      (unless (null? bvs)
        (bytevector-copy! (car bvs) 0 out index (bytevector-length (car bvs)))
        (loop (cdr bvs) (+ index (bytevector-length (car bvs))))))
    out))

(define-public (ulid)
  (let ((epoch (pack (current-milliseconds)))
        (index (thread-index))
        (randomness (random-bytes 8)))
    ;; XXX: if you see the error:
    ;;
    ;;  Wrong type argument in position 1 (expecting bytevector): #f
    ;;
    ;; thread-index is #f instead of bytevector of length two.
    (bytevector-append epoch index randomness)))
