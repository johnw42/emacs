;; -*- lexical-binding: t; -*-


;; (defmacro foo (n)
;;   (bar 1)
;;   nil)

;; (defmacro bar (n)
;;   (foo 1)
;;   nil)

(require 'f)
(require 's)

(defun improper-each (list fn)
  (while (consp list)
    (funcall fn (car list))
    (setq list (cdr list)))
  (when list
    (funcall fn list)))

(defun do-it ()
  (let ((forms-used (make-hash-table :test 'eq)))
    (cl-block tmp
      (dolist (path (f-entries "/home/jrw/git/emacs/lisp/" nil t))
        (when (s-matches-p (rx ".el" eos) path)
          (message "%s" path)
          (let ((exprs
                 (with-temp-buffer
                   (insert "(progn\n")
                   (insert-file-contents path)
                   (goto-char (point-max))
                   (insert ")")
                   (goto-char (point-min))
                   (read (current-buffer)))))
            (cl-labels
                ((remove-qq
                  (form)
                  (and (memq (car-safe form) '(\, \,@))
                       (cdr form)))
                 (analyze-form
                  (form macro-name)
                  (when (and (consp form)
                             (symbolp (car form)))
                    (pcase form
                      (`(quote . ,_) nil)
                      ((or `(quasiquote . ,arg)
                           `(\` . ,arg))
                       (analyze-form (remove-qq arg) macro-name))
                      (`(defmacro ,name ,_ . ,body)
                       (improper-each
                        body
                        (lambda (it) (analyze-form it name))))
                      (`(,(or 'defun 'defsubst) ,_ ,_ . ,body)
                       (improper-each
                        body
                        (lambda (it)
                          (analyze-form it macro-name))))
                      (`(,head . ,args)
                       (unless (or (not macro-name)
                                   (functionp head))
                         (puthash
                          head t
                          (or (gethash macro-name forms-used)
                              (puthash macro-name
                                       (make-hash-table :test 'eq)
                                       forms-used))))
                       (improper-each
                        args
                        (lambda (it)
                          (analyze-form it macro-name))))))))
              (analyze-form exprs nil)
              ;; (when (> (hash-table-count forms-used) 20)
              ;;   (cl-return-from tmp))
              )))))
    (let (done)
      (while (not done)
        (message "finding closure...")
        (setq done t)
        (maphash (lambda (form subforms)
                   (dolist (subform (hash-table-keys subforms))
                     (when-let ((other-subforms (gethash subform forms-used)))
                       (dolist (other-subform (hash-table-keys other-subforms))
                         (when (eq form other-subform)
                           (message "found cycle: %S %S" form subform))
                         (unless (gethash other-subform subforms)
                           (puthash other-subform t subforms)
                           (setq done nil))))))
                 forms-used))
      (message "analyzed %S" (hash-table-keys forms-used))
      (message "done!"))
    ;; (maphash (lambda (form subforms)
    ;;            (message "%S %S" form (hash-table-keys subforms)))
    ;;          forms-used)
    ))

(byte-compile 'do-it)
(do-it)
