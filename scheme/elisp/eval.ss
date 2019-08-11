#!chezscheme
(library (elisp eval)
  (export elisp-eval
          elisp-load)
  (import (elisp reader)
          (scheme))

  ;; (define-record-type lexical-env
  ;;   (fields parent table)
  ;;   (protocol (lambda (new)
  ;;               (lambda (parent)
  ;;                 (new parent (make-eq-hashtable))))))

  ;; (define (lexical-env-ref env symbol)
  ;;   (let ([table (lexical-env-table env)])
  ;;     (or (hashtable-ref env symbol #f)
  ;;         (let ([box (lexical-env-ref (lexical-env-parent env) symbol)])
  ;;           ))))

  (define (elisp-load filename)
    (let ([sexps (with-input-from-file filename
                   (lambda ()
                     (let loop ([sexps '()])
                       (let ([sexp (elisp-read)])
                         (if sexp
                             (loop (cons sexp sexps))
                             (reverse sexps))))))])
      (for-each elisp-eval sexps)))

  ;; (define eval-func-table
  ;;   (let ([table (make-eq-hashtable)])
  ;;     (define-syntax define-special-forms
  ;;       (syntax-rules ()
  ;;         [(_ [name def] ...)
  ;;          (begin
  ;;            (hashtable-set! table 'name def) ...)]))
  ;;     (define-special-forms
  ;;       [setq (lambda args
  ;;               (let loop ([symbol (car args)]
  ;;                          [expr (cadr args)]
  ;;                          [rest (cddr args)])
  ;;                 (hashtable-set! symbol-value-table
  ;;                                 symbol
  ;;                                 (elisp-eval form))))]
  ;;       [if ()])
  ;;     table))
  
  ;; (define symbol-value-table
  ;;   (let ([table (make-eq-hashtable)])
  ;;     (define-syntax define-vars
  ;;       (syntax-rules ()
  ;;         [(_ [name value] ...)
  ;;          (hashtable-set! table 'name value)]))
  ;;     table))

  ;; (define-record-type elisp-symbol
  ;;   (fields symbol
  ;;           value
  ;;           function
  ;;           plist
  ;;           readonly?))

  ;; (define symbol-table (make-eq-hashtable))

  ;; (define elisp-compile
  ;;   (lambda (form)
  ;;     (cond
  ;;      [(pair? form)
  ;;       (let* ([sym (car form)]
  ;;              [fail (lambda _ (errorf 'elisp-eval
  ;;                                      "Not a function: ~s"
  ;;                                      sym))]
  ;;              [func (hashtable-ref eval-func-table sym fail)])
  ;;         (apply func (cdr form)))]
  ;;      [else 
  ;;       (errorf 'elisp-eval "Can't compile: ~s" form)])))

  ;; (define elisp-eval
  ;;   (lambda (form)
  ;;     (eval (elisp-compile form)
  ;;           (environment '(elisp env)))))

  (define (elisp-eval form lexical-env)
    (when (eq? lexical-env #t)
      (set! lexical-env '()))
    (cond
     [(symbolp? form)
      (let ([pair (assq form lexical-env)])
        (if pair
            (cdr pair)
            (symbol-value form)))]))
  )
