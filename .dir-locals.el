;; The 'nil' configuration applies to all modes.
((scheme-mode . ((eval . (progn
                           ;; example
                           (put 'with-directory 'scheme-indent-function 1)
                           (put 'match 'scheme-indent-function 0)
                           (put 'with-connection 'scheme-indent-function 2))))))
