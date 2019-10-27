#!chezscheme

(let ([libdir (car (command-line-arguments))])
  (source-directories (cons libdir (source-directories)))

  (printf "libdir: ~s\n" (source-directories))
  (load "prims.ss")
  (load "misc.ss")
  (load "cc-helper.ss")
  (load "eval.ss"))

(import (prefix (elisp prims) emacs-))
(import (elisp misc))
(import (elisp cc-helper))
(import (elisp eval))
