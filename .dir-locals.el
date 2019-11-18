;; The 'nil' configuration applies to all modes.
((scheme-mode . ((eval . (progn
                           ;; example
                           (put 'okvs-in-transaction 'scheme-indent-function 1)
                           (put 'with-directory 'scheme-indent-function 1)
                           (put 'match 'scheme-indent-function 1)
                           (put 'with-connection 'scheme-indent-function 2))))))
