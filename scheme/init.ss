#!chezscheme

(let ([libdir (car (command-line-arguments))])
  (source-directories (cons libdir (source-directories)))

  (printf "libdir: ~s\n" (source-directories))
  (load "prims.ss")
  (load "misc.ss")
  (load "cc-helper.ss"))

(import (prims))
(import (misc))
(import (cc-helper))
