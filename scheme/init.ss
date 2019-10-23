#!chezscheme

(let ([libdir (car (command-line-arguments))])
  (source-directories (cons libdir (source-directories)))

  (printf "libdir: ~s\n" (source-directories))
  (load "misc.ss")
  (load "cc-helper.ss")
  (load "prims.ss"))

(import (misc))
(import (cc-helper))
(import (prims))

;; Local Variables:
;; mode: scheme
;; eval: (put 'module 'scheme-indent-function 1)
;; eval: (put 'with-mutex 'scheme-indent-function 1)
;; End:
