;; -*- mode: scheme; eval: (put 'elisp-lambda 'scheme-indent-function 1) -*-
(library (elisp atoms)
  (export ~nil
          ~null
          ~t
          ~type-of

          elisp->boolean
          make-symbol-record)
  (import (rnrs (6)))

  (define ~nil '())
  (define ~t #t)

  ;; (define-syntax elisp-lambda
  ;;   (syntax-rules ()
  ;;     [(_ argspec body ...)
  ;;      (lambda argspec
  ;;        (let ([result (begin nil body ...)])
  ;;          ))]))

  (define-syntax elisp->boolean
    (syntax-rules ()
      [(_ x) (not (or (null? x) (not x)))]))

  (define-syntax boolean->elisp
    (syntax-rules ()
      [(_ x) (or x ~nil)]))

  (define (~consp x)
    (if (pair? x) ~t ~nil))

  (define (~atomp x)
    (if (pair? x) ~nil ~t))

  (define (~listp x)
    (if (or (pair? x) (null? x)) ~t ~nil))

  (define (~nlistp x)
    (if (or (pair? x) (null? x)) ~nil ~t))

  (define (~null x)
    (if (or (not x) (null? x)) ~t ~nil))

  (define (~arrayp x)
    (boolean->elisp
     (or (vector? x)
         (bytevector? x)
         (string? x)
         (and (record? x)
              (or (string-record? x)
                  (bool-vector-record? x)
                  (char-table-record? x))))))

  (define (~bool-vector-p x)
    (boolean->elisp
     (bool-vector-record? x)))

  (define-record-type bool-vector-record)
  (define-record-type char-table-record)

  (define-record-type symbol-record
    (fields name symbol))

  (define-record-type string-record
    (fields value plist))

  ;; (define-syntax pred-case
  ;;   (syntax-rules (else)
  ;;     [(_ arg)
  ;;      (void)]
  ;;     [(_ arg [else body ...])
  ;;      (begin body ...)]
  ;;     [(_ arg [(pred ...) body ...] clause ...)
  ;;      (if (or (pred arg) ...)
  ;;          (begin body ...)
  ;;          (pred-case arg clause ...))]))

  (define record-type-table (make-eq-hashtable))

  (define (register-record-type rtd symbol)
    (hashtable-set! record-type-table rtd symbol))

  (define (~type-of x)
    (cond
     [(record? x)
      (let ([rtd (record-rtd x)])
        (or (hashtable-ref record-type-table rtd #f)
            (record-type-name rtd)))]
     [(boolean? x) 'symbol]
     [(null? x) 'symbol]
     [(pair? x) 'cons]
     [(integer? x) 'integer]
     [(real? x) 'float]
     [(char? x) 'integer]
     [(string? x) 'string]
     [(vector? x) 'vector]
     [(symbol? x) 'symbol]
     [(procedure? x) 'subr]
     [(bytevector? x) 'vector]
     [(hashtable? x) 'hash-table]
     ;; TODO
     [else ~nil]))

  (register-record-type (record-rtd 'symbol-record) 'symbol))
