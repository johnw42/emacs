;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((nil
  (tab-width . 8)
  (sentence-end-double-space . t)
  (fill-column . 70))
 (c-mode
  (c-file-style . "GNU")
  (c-noise-macro-names "UNINIT" "CALLBACK" "ALIGN_STACK"))
 (change-log-mode
  (add-log-time-zone-rule . t)
  (fill-column . 74)
  (bug-reference-url-format . "https://debbugs.gnu.org/%s")
  (mode . bug-reference))
 (diff-mode
  (mode . whitespace))
 (emacs-lisp-mode
  (indent-tabs-mode))
 (log-edit-mode
  (log-edit-font-lock-gnu-style . t)
  (log-edit-setup-add-author . t))
 (objc-mode
  (c-file-style . "GNU"))
 (scheme-mode
  (eval . (progn
            (font-lock-add-keywords 'scheme-mode
                            (list
                             (regexp-opt '("emacs-lambda" "module")
                                         'symbols)))
            (put 'module 'scheme-indent-function 1)
            (put 'with-mutex 'scheme-indent-function 1)
            (put 'emacs-lambda 'scheme-indent-function 2)))))

