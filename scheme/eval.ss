(library (elisp eval)
  (export eval-lisp-sub)
  (import (chezscheme))
  (import (prefix (elisp prims) emacs-))

(define eval-lisp-sub
  (lambda (form)
    (printf "lisp-eval-sub: ~s\n" form)
    #f))

)
