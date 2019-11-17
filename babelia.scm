#!/bin/sh
# -*- scheme -*-
exec guile -L $(pwd) -e '(@ (babelia) main)' -s "$0" "$@"
!#
(define-module (babelia))

(import (babelia web))


(define-public (main _)
  (run))
