(define subr-max-args 3)

(define nil 'nil)

(define-syntax wrap-function-impl
  (lambda (x)
    (syntax-case x ()
      [(_ func-ptr-form min-args-form max-args-form)
       (let* ([lambda-case
               (lambda (num-args max-args)
                 (let ([params (generate-temporaries
                                (iota num-args))]
                       [filler (make-list (- max-args num-args)
                                          #'nil)])
                   #`[#,params
                      (proc #,@params #,@filler)]))
               ]
              [lambda-cases
               (lambda (min-args max-args)
                 (map (lambda (num-args)
                        (lambda-case num-args max-args))
                      (map (lambda (i) (+ i min-args))
                           (iota (+ 1 (- max-args min-args))))))]
              [min-clause
               (lambda (min-args max-args)
                 #`(#,min-args
                    #,(if (= min-args max-args)
                          #'proc
                          #`(case-lambda
                             #,@(lambda-cases min-args max-args)))))]
              [min-clauses
               (lambda (max-args)
                 (map (lambda (min-args) (min-clause min-args max-args))
                      (iota (+ 1 max-args))))]
              [max-clause
               (lambda (max-args)
                 (let ([arg-types
                        (map (lambda (_) #'scheme-object)
                             (iota max-args))])
                   #`(#,max-args
                      (let ([proc (foreign-procedure func-ptr-form
                                                     #,arg-types
                                                     scheme-object)])
                        (case min-args-form
                          #,@(min-clauses max-args))))))]
              [max-clauses
               (map max-clause
                    (iota (+ 1 subr-max-args)))])
         #`'(case max-args-form
              #,@max-clauses))
       ])))

(pretty-print (sc-expand '(wrap-function-impl fp min-a max-a)))
