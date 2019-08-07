(library (elisp symbol)

  (export intern
          intern-soft
          make-symbol
          symbol-name
          symbolp)
  (import (rnrs (6))
          (elisp atoms))

  (define symbol-table (make-hashtable string=? string-hash))

  (define (make-symbol name)
    (make-elisp-symbol name #f))

  (define (intern name)
    (or (hashtable-ref symbol-table name #f)
        (let* ([s-symbol (string->symbol name)]
               [e-symbol (make-elisp-symbol name s-symbol)])
          (hashtable-set! symbol-table name e-symbol)
          e-symbol)))

  (define (intern-soft name)
    (hashtable-ref symbol-table name nil))

  (define (symbol-name sym)
    (cond
     [(elisp-symbol? sym) (elisp-symbol-name sym)]
     [(symbol? sym) (symbol->string sym)]
     [(or (null? sym) (not sym)) "nil"]
     [else (error 'symbol-name "Argument is not a symbol" sym)]))

  (define (symbolp x)
    (or (elisp-symbol? x)
        (symbol? x)
        (null? x)
        (not x)
        nil)))
