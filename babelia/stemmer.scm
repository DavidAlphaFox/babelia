;; Copyright (C) 2019 Amirouche Boubekki <amirouche@hyper.dev>

;; This library is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.

;; This library is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.

;; You should have received a copy of the GNU Lesser General Public License
;; along with this library; if not, write to the Free Software Foundation,
;; Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
(define-module (babelia stemmer))

(import (system foreign))
(import (only (rnrs bytevectors)
              bytevector-length
              string->utf8
              utf8->string))

(export stemmers make-stemmer stem)

;;; ffi helpers

(define NULL %null-pointer)
(define POINTER '*)

;; XXX: only use that procedure in your project if you don't need to
;; access static variables
(define (dynamic-link* library-name)
  (let ((shared-object (dynamic-link library-name)))
    (lambda (return-value function-name . arguments)
      (let ((function (dynamic-func function-name shared-object)))
        (pointer->procedure return-value function arguments)))))

;; bindings

(define snowball-stemmer
  (dynamic-link* "/gnu/store/rzvlish3vsidfmvv74f74s2854wn8yii-stemmer-0.0.0/lib/libstemmer.so"))

(define stemmers
  (let ((proc (snowball-stemmer POINTER "sb_stemmer_list")))
    (lambda ()
      (let ((array (pointer-address (proc))))
        (let loop ((out '())
                   (index 0))
          (let ((pointer (dereference-pointer (make-pointer (+ array (* 8 index))))))
            (if (eq? pointer NULL)
                out
                (loop (cons (pointer->string pointer) out)
                      (+ index 1)))))))))

(define %stemmer-delete
  (let ((proc (snowball-stemmer void "sb_stemmer_delete" POINTER)))
    (lambda (stemmer)
      (proc stemmer))))

(define stemmers-guardian (make-guardian))

;; TODO: return a stem procedure
(define make-stemmer
  (let ((proc (snowball-stemmer POINTER "sb_stemmer_new" POINTER POINTER)))
    (lambda (algorithm)
      (let ((out (proc (string->pointer algorithm) NULL)))
        (when (eq? out NULL)
          (error 'snowball-stemmer "Oops! Stemmer not found" algorithm))
        (stemmers-guardian out)
        out))))

(define (reap-stemmers)
  (let loop ()
    (let ((stemmer (stemmers-guardian)))
      (when stemmer
        (%stemmer-delete stemmer)
        (loop)))))

(add-hook! after-gc-hook reap-stemmers)

(define %stemmer-length
  (let ((proc (snowball-stemmer int "sb_stemmer_length" POINTER)))
    (lambda (stemmer)
      (proc stemmer))))

(define stem
  (lambda (stemmer word)
    (let ((proc (snowball-stemmer POINTER "sb_stemmer_stem" POINTER POINTER int)))
      (let ((bv (string->utf8 word)))
        (let ((pointer (proc stemmer (bytevector->pointer bv) (bytevector-length bv))))
          (utf8->string (pointer->bytevector pointer (%stemmer-length stemmer))))))))
