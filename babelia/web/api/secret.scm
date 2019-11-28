(define-module (babelia web api secret))

(import (only (rnrs) error))
(import (only (babelia okvs ulid) random-bytes))
(import (only (rnrs bytevectors) bytevector->u8-list))
(import (only (rnrs io ports) put-string))


(define (path-exists? path)
  "Return #true if path is a file or directory.
   #false if it doesn't exists"
  (access? path F_OK))

(define (byte->hex number)
  (if (< number 16)
      (string-append "0" (number->string number 16))
      (number->string number 16)))

(define (hexify bytevector)
  ;; convert a bytevector into a string made of hexadecimal
  ;; representation of the bytes
  (string-upcase
   (string-concatenate (map byte->hex (bytevector->u8-list bytevector)))))

(define (make-secret)
  (hexify (random-bytes 256)))

(define (generate filename)
  (call-with-output-file filename
    (lambda (port)
      (put-string port (make-secret))))
  (pk "secret written to" filename))

(define-public (subcommand-secret-generate directory args)
  (unless (path-exists? directory)
    (error 'babelia "directory does not exists" directory))
  (let ((force? (and (not (null? args)) (string=? (car args) "--force")))
        (filename (string-append directory "/secret")))
    (if (path-exists? filename)
        (if force?
            (generate filename)
            (error 'babelia "there is an existing secret pass --force to overwrite" filename))
        (generate filename))))
