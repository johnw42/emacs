(library (prims)
  (export
   emacs-eq
   emacs-null
   emacs-cons
   emacs-car

   emacs-car-safe
   emacs-cdr
   emacs-cdr-safe
   emacs-setcar
   emacs-setcdr
   )
  (import (chezscheme))

  (define nil #f)
  (define t #t)

  (define emacs-false?
    (lambda (x)
      (not x)))

  (define emacs-true?
    (lambda (x)
      (not (emacs-false? x))))

  (define ->emacs-boolean
    (lambda (x)
      (if x #t #f)))

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

  (define encode-cdr
    (lambda (x)
      (if (emacs-false? x)
          '()
          x)))

  (define decode-cdr
    (lambda (x)
      (if (null? x) nil x)))

  (define emacs-eq
    (lambda (x y)
      (->emacs-boolean (eq? x y))))

  (define emacs-null
    (lambda (x)
      (->emacs-boolean
       (memq x '(#f nil ())))))

  (define emacs-consp
    (lambda (x)
      (->emacs-boolean (pair? x))))

  (define emacs-atom
    (lambda (x)
      (->emacs-boolean (atom? x))))

  (define emacs-listp
    (lambda (x)
      (->emacs-boolean
       (or (pair? x) (emacs-false? x)))))

  (define emacs-nlistp
    (lambda (x)
      (->emacs-boolean
       (not (emacs-listp x)))))

  (define emacs-symbolp
    (lambda (x)
      (->emacs-boolean
       (or (symbol? x) (boolean? x)))))

  (define emacs-keywordp
    (lambda (x)
      (->emacs-boolean
       (and (symbol? x)
            (let ([str (symbol->string x)])
              (and (> (string-length str) 0)
                   (eqv? #\: (string-ref str 0))))))))

  (define emacs-cons
    (lambda (car cdr)
      (cons car (encode-cdr cdr))))

  (define emacs-car
    (lambda (pair)
      (cond
       [(pair? pair)
        (car pair)]
       [(emacs-false? pair) nil]
       [else (wrong-type-argument 'listp pair)])))

  (define emacs-car-safe
    (lambda (pair)
      (if (pair? pair)
          (car pair)
          nil)))

  (define emacs-cdr
    (lambda (pair)
      (cond
       [(pair? pair) (decode-cdr (cdr pair))]
       [(emacs-false? pair) nil]
       [else (wrong-type-argument 'listp pair)])))

  (define emacs-cdr-safe
    (lambda (pair)
      (if (pair? pair)
          (decode-cdr (cdr pair))
          nil)))

  (define emacs-setcar
    (lambda (pair new-car)
      (check-cons pair)
      (set-car! pair new-car)
      nil))

  (define emacs-setcdr
    (lambda (pair new-cdr)
      (check-cons pair)
      (set-cdr! pair (encode-cdr new-cdr))
      nil))
  )
