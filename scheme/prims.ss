(library (prims)
  (export
   emacs-nil
   emacs-t
   emacs-nil?
   emacs-truthy?
   emacs-eq
   emacs-null
   emacs-cons
   emacs-car
   emacs-car-safe
   emacs-cdr
   emacs-cdr-safe
   emacs-setcar
   emacs-setcdr)
  (import (chezscheme))

  ;; The value used to represent nil in Emacs Lisp.
  (define emacs-nil '())

  ;; The value used to represent t in Emacs Lisp.
  (define emacs-t #t)

  ;; Returns #t if the argument is the Emacs nil value, #f otherwise.
  (define emacs-nil? null?)

  ;; Returns #f if the argument is the Emacs nil value, #t otherwise.
  (define emacs-truthy?
    (lambda (x) (not (null? x))))

  ;; Convert an Emacs nil value to #f and return any other value
  ;; unchanged.
  (define nil->false
    (lambda (x)
      (if (null? x) #f x)))

  ;; Convert #f to an the Emacs nil value and return any other value
  ;; unchanged.
  (define false->nil
    (lambda (x)
      (or x '())))

  (define-syntax emacs-lambda-result
    (syntax-rules (&obj &list &bool &nil)
      [(_ &obj expr1 expr2 ...)
       (let ([x (begin expr1 expr2 ...)])
         (if (eq? x (void)) emacs-nil x))]
      [(_ &list expr1 expr2 ...)
       (emacs-lambda-result &obj expr1 expr2 ...)]
      [(_ &bool expr1 expr2 ...)
       (let ([x (begin expr1 expr2 ...)])
         (if (or (not x) (eq? x (void))) emacs-nil emacs-t))]
      [(_ &nil expr1 expr2 ...)
       (begin expr1 expr2 ... emacs-nil)]))

  ;; Variant of lambda for functions that may be called from Emacs Lisp.
  (define-syntax emacs-lambda
    (lambda (x)
      (syntax-case x (&obj &bool &list)
        [(k type () expr1 expr2 ...)
         #'(lambda () (emacs-lambda-result type expr1 expr2 ...))]
        [(k type (arg ...) expr1 expr2 ...)
         (let loop ([args #'(arg ...)]
                    [arg-names '()]
                    [bindings '()])
           (if (null? args)
               #`(lambda #,(reverse arg-names)
                   (let #,(reverse bindings)
                     (emacs-lambda-result type expr1 expr2 ...)))
               (let ([arg-type (car args)]
                     [arg-name (cadr args)])
                 (loop (cddr args)
                       (cons arg-name arg-names)
                       (case (syntax->datum arg-type)
                         [(&obj) bindings]
                         [(&bool)
                          (cons #`[#,arg-name (nil->false #,arg-name)]
                                bindings)]
                         [(&list)
                          (cons #`[#,arg-name (false->nil #,arg-name)]
                                bindings)]
                         [else
                          (syntax-error
                           arg-type
                           "invalid argument type")])))))])))

  (define wrong-type-argument
    (foreign-procedure "wrong_type_argument"
                       (scheme-object scheme-object)
                       scheme-object))

  (define check-type
    (lambda (ok? pred x)
      (unless ok?
        (wrong-type-argument pred x))))

  (define check-cons
    (lambda (cell)
      (check-type (pair? cell) 'consp cell)))

  (define emacs-eq
    (emacs-lambda &bool (&obj x &obj y)
      (eq? x y)))

  (define emacs-null
    (emacs-lambda &bool (&obj x)
      (eq? x emacs-nil)))

  (define emacs-consp
    (emacs-lambda &bool (&obj x)
      (pair? x)))

  (define emacs-atom
    (emacs-lambda &bool (&obj x)
      (atom? x)))

  (define emacs-listp
    (emacs-lambda &bool (&obj x)
      (or (pair? x) (emacs-nil? x))))

  (define emacs-nlistp
    (emacs-lambda &bool (&obj x)
      (not (or (pair? x) (emacs-nil? x)))))

  (define emacs-symbolp
    (emacs-lambda &bool (&obj x)
      (or (symbol? x)
          (eq? x emacs-nil)
          (eq? x emacs-t))))

  (define emacs-keywordp
    (emacs-lambda &bool (&obj x)
      (and (symbol? x)
           (let ([str (symbol->string x)])
             (and (> (string-length str) 0)
                  (eqv? #\: (string-ref str 0)))))))

  (define emacs-cons cons)

  (define emacs-car
    (emacs-lambda &obj (&obj pair)
      (cond
       [(pair? pair) (car pair)]
       [(null? pair) emacs-nil]
       [else (wrong-type-argument 'listp pair)])))

  (define emacs-car-safe
    (emacs-lambda &obj (&obj pair)
      (if (pair? pair)
          (car pair)
          emacs-nil)))

  (define emacs-cdr
    (emacs-lambda &obj (&obj pair)
      (cond
       [(pair? pair) (cdr  pair)]
       [(null? pair) emacs-nil]
       [else (wrong-type-argument 'listp pair)])))

  (define emacs-cdr-safe
    (emacs-lambda &obj (&obj pair)
      (if (pair? pair)
          (cdr pair)
          emacs-nil)))

  (define emacs-setcar
    (emacs-lambda &nil (&obj pair &obj new-car)
      (check-cons pair)
      (set-car! pair new-car)))

  (define emacs-setcdr
    (emacs-lambda &nil (&obj pair &obj new-cdr)
      (check-cons pair)
      (set-cdr! pair new-cdr))))
