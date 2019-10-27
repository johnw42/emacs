(library (elisp prims)
  (export
   (rename (emacs-lambda lambda)
           (emacs-car car)
           (emacs-cdr cdr))
   nil
   t
   nil?
   truthy?
   nil->f
   f->nil
   eq
   null
   cons
   consp
   atom
   listp
   nlistp
   symbolp
   keywordp
   car-safe
   cdr-safe
   setcar
   setcdr)
  (import (chezscheme))

;; The value used to represent nil in Emacs Lisp.
(define nil '())

;; The value used to represent t in Emacs Lisp.
(define t #t)

;; Returns #t if the argument is the Emacs nil value, #f otherwise.
(define nil? null?)

;; Returns #f if the argument is the Emacs nil value, #t otherwise.
(define truthy?
  (lambda (x) (not (null? x))))

;; Convert an Emacs nil value to #f and return any other value
;; unchanged.
(define nil->f
  (lambda (x)
    (if (null? x) #f x)))

;; Convert #f to an the Emacs nil value and return any other value
;; unchanged.
(define f->nil
  (lambda (x)
    (or x '())))

(define-syntax emacs-lambda-result
  (syntax-rules (&obj &list &bool &nil)
    [(_ &obj expr1 expr2 ...)
     (let ([x (begin expr1 expr2 ...)])
       (if (eq? x (void)) nil x))]
    [(_ &list expr1 expr2 ...)
     (emacs-lambda-result &obj expr1 expr2 ...)]
    [(_ &bool expr1 expr2 ...)
     (let ([x (begin expr1 expr2 ...)])
       (if (or (not x) (eq? x (void))) nil t))]
    [(_ &nil expr1 expr2 ...)
     (begin expr1 expr2 ... nil)]))

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

(define eq
  (emacs-lambda &bool (&obj x &obj y)
    (eq? x y)))

(define null
  (emacs-lambda &bool (&obj x)
    (eq? x nil)))

(define consp
  (emacs-lambda &bool (&obj x)
    (pair? x)))

(define atom
  (emacs-lambda &bool (&obj x)
    (atom? x)))

(define listp
  (emacs-lambda &bool (&obj x)
    (or (pair? x) (nil? x))))

(define nlistp
  (emacs-lambda &bool (&obj x)
    (not (or (pair? x) (nil? x)))))

(define symbolp
  (emacs-lambda &bool (&obj x)
    (or (symbol? x)
        (eq? x nil)
        (eq? x t))))

(define keywordp
  (emacs-lambda &bool (&obj x)
    (and (symbol? x)
         (let ([str (symbol->string x)])
           (and (> (string-length str) 0)
                (eqv? #\: (string-ref str 0)))))))

(define emacs-car
  (emacs-lambda &obj (&obj pair)
    (cond
     [(pair? pair) (car pair)]
     [(null? pair) nil]
     [else (wrong-type-argument 'listp pair)])))

(define car-safe
  (emacs-lambda &obj (&obj pair)
    (if (pair? pair)
        (car pair)
        nil)))

(define emacs-cdr
  (emacs-lambda &obj (&obj pair)
    (cond
     [(pair? pair) (cdr  pair)]
     [(null? pair) nil]
     [else (wrong-type-argument 'listp pair)])))

(define cdr-safe
  (emacs-lambda &obj (&obj pair)
    (if (pair? pair)
        (cdr pair)
        nil)))

(define setcar
  (emacs-lambda &nil (&obj pair &obj new-car)
    (check-cons pair)
    (set-car! pair new-car)))

(define setcdr
  (emacs-lambda &nil (&obj pair &obj new-cdr)
    (check-cons pair)
    (set-cdr! pair new-cdr))))
