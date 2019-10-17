(library (cc-helper)
  (export lisp-condition-case-helper
          condition-case-helper
          catch-helper)
  (import (chezscheme))

  (define (lisp-condition-case-helper trampoline-ptr
                                      body-form
                                      num-handlers)
    (call/cc
     (lambda (k)
       (cons
        #f
        ((foreign-procedure trampoline-ptr
                            (scheme-object iptr scheme-object)
                            scheme-object)
         body-form num-handlers k)))))

  (define condition-case-helper
    (case-lambda
     [(trampoline-ptr bfun)
      (call/cc
       (lambda (k)
         (cons
          #f
          ((foreign-procedure trampoline-ptr
                              (void*
                               scheme-object)
                              scheme-object)
           bfun k))))]
     [(trampoline-ptr bfun arg)
      (call/cc
       (lambda (k)
         (cons
          #f
          ((foreign-procedure trampoline-ptr
                              (void*
                               scheme-object
                               scheme-object)
                              scheme-object)
           bfun arg k))))]
     [(trampoline-ptr bfun arg1 arg2)
      (call/cc
       (lambda (k)
         (cons
          #f
          ((foreign-procedure trampoline-ptr
                              (void*
                               scheme-object
                               scheme-object
                               scheme-object)
                              scheme-object)
           bfun arg1 arg2 k))))]))

  (define condition-case-helper-n
    (lambda (trampoline-ptr bfun nargs args)
      (call/cc
       (lambda (k)
         (cons
          #f
          ((foreign-procedure trampoline-ptr
                              (void*
                               iptr
                               void*
                               scheme-object)
                              scheme-object)
           bfun nargs args k))))))


  (define catch-helper
    (lambda (trampoline-ptr func arg)
      (call/cc
       (lambda (k)
         (cons
          #f
          ((foreign-procedure trampoline-ptr
                              (void*
                               scheme-object
                               scheme-object)
                              scheme-object)
           func arg k)))))))
