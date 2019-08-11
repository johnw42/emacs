(debug-level 3)
(optimize-level 0)
(import (elisp eval)
        ;;(elisp atoms)
        )

;; Gets the a list of at most `max-depth` strings representing the
;; stack trace for the contiuation `k`.
(define (get-stack-trace k max-depth)
  (do ([cur (inspect/object k) (cur 'link)]
       [i 0 (+ i 1)]
       [stack '()
              (cons
               (call-with-values (lambda () (cur 'source-path))
                 (case-lambda
                  [(file line . _)
                   (format "at ~a (~a:~a)"
                           ((cur 'code) 'name) file line)]
                  [else (format "no source at ~a" i)]))
               stack)])
      [(or (not cur)
           (<= (cur 'depth) 1)
           (> i max-depth))
       (reverse stack)]))

;; Prints a stack trace for continuation `k`, preceeded by a label
;; `label`, containing at most `max-depth` items.  If `k` is omitted,
;; the current continutation is used.
(define print-stack-trace
  (case-lambda
   [(label k max-depth)
    (printf "backtrace of [~a] as following:\n" label)
    (for-each (lambda (line)
                (printf "\t~a\n" line))
              (get-stack-trace k max-depth))]
   [(label max-depth)
    (call/cc (lambda (k)
               (print-stack-trace label k max-depth)))]))

(unless (debug-on-exception)
  (let ([old-handler (base-exception-handler)])
    (base-exception-handler
     (lambda (err)
       (when (continuation-condition? err)
         (print-stack-trace "exception" (condition-continuation err) 20))
       (old-handler err)))))

(define-syntax and-let*
  (syntax-rules ()
    [(_ () body ...)
     (let () body ...)]
    [(_ ([var expr] [vars exprs] ...) body ...)
     (let ([var expr])
       (and var
            (and-let* ([vars exprs] ...) body ...)))]))

;; (define (elisp-read-file path)
;;   (with-input-from-file path
;;     (lambda ()
;;       (let loop ([sexps '()])
;;         (or (and-let* ([sexp (elisp-read)])
;;                       (loop (cons sexp sexps)))
;;             (reverse sexps))))))

;; (for-each (lambda (path)
;;             (for-each pretty-print
;;                       (elisp-read-file path)))
;;           '(;;"lisp/emacs-lisp/macroexp.el"
;;             "lisp/loadup.el"))

(elisp-load "lisp/loadup.el")
