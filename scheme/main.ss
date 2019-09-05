#!chezscheme
(import (chezscheme))

(printf "main.ss running\n")

(module (emacs-init
         ensure-scheme-object-id
         ensure-lisp-object-ref
         extract-lisp-object
         scheme-object-for-id
         forget-scheme-object
         after-idle
         identity
         make-obarray-table

         c-hashtablep
         c-save_pointer
         c-check_pointer
         c-print_to_bytevector
         c-save_origin
         c-print_origin
         c-eq_hash
         c-hashtable_values
         c-hashtable_ref
         c-symbol_is
         c-trivial
         c-ephemeron_cons
         )

  (define elisp-do-scheme-gc
    (foreign-procedure __collect_safe "do_scheme_gc" () void))

  (define elisp-before-scheme-gc
    (foreign-procedure "before_scheme_gc" () boolean))

  (define elisp-after-scheme-gc
    (foreign-procedure "after_scheme_gc" () void))

  (define elisp-equal
    (foreign-procedure "Fequal" (scheme-object scheme-object) scheme-object))

  (define elisp-sxhash-equal
    (foreign-procedure "Fsxhash_equal" (scheme-object) scheme-object))

  (define (make-obarray-table)
    (make-hashtable elisp-sxhash-equal
                    (lambda (a b)
                      (not (eq? 'nil (elisp-equal a b))))))

  ;; A hash function for foreign pointers.
  (define pointer-hash
    (begin
      (for-each (lambda (i)
                  (assert (fxlogbit? i (greatest-fixnum))))
                (iota (- (fixnum-width) 1)))
      (lambda (ptr)
        ;; If ptr is not already a fixnum, chop off enough high-order
        ;; bits to make it a fixnum.
        (unless (fixnum? ptr)
          (set! ptr (bitwise-and ptr (least-fixnum))))
        ;; Due to alignment requirements, the low-order bits of most
        ;; pointers tend to be zero.  This makes for a terrible hash
        ;; function, especially given that Chez Scheme's hashtable
        ;; type uses power-of-two table sizes, which effectively
        ;; ignores the high-order bits of the hash code.  In practice
        ;; this means each pointer will probably have 3 or 4 junk bits
        ;; per pointer, but we try to accomodate up to 8 junk bits.
        (fxlogxor ptr
                  (fxsrl ptr 4)
                  (fxsrl ptr 8)))))

  (define pointer-table
    (make-hashtable pointer-hash =))

  (define (strlen ptr)
    (if (fxzero? ptr)
        0
        (do ([i 0 (+ 1 i)])
            ((fxzero? (foreign-ref 'unsigned-8 ptr i))
             i))))

  (define (strcmp? ptr1 ptr2)
    (or (fx= ptr1 ptr2)
        (and (not (fxzero? ptr1))
             (not (fxzero? ptr2))
             (let loop ([i 0])
               (let ([c1 (foreign-ref 'unsigned-8 ptr1 i)]
                     [c2 (foreign-ref 'unsigned-8 ptr2 i)])
                 (and (eqv? c1 c2)
                      (or (zero? c1)
                          (loop (+ i 1)))))))))

  (define decode-char*
    (let ([memo-ptr #f]
          [memo-result #f])
      (lambda (ptr)
        (if (fxzero? ptr)
            "<null>"
            (unless (eqv? ptr memo-ptr)
              (set! memo-result
                    (let* ([count (strlen ptr)]
                           [str (make-string count)])
                      (do ([i 0 (+ 1 i)])
                          ((>= i count) str)
                        (string-set!
                         str i (integer->char
                                (foreign-ref 'unsigned-8 ptr i))))))))
        memo-result)))


  (define-syntax locked-foreign-callable
    (syntax-rules ()
      [(_ func (arg-type ...) result-type)
       (let ([x (foreign-callable func (arg-type ...) result-type)])
         (lock-object x)
         x)]))

  (define-syntax define-for-c
    (lambda (x)
      (syntax-case x ()
        [(k (name result-type (arg ...)) form ...)
         #'(k (name result-type (arg ...) #f)
              form ...)]
        [(k (name result-type
                  ((arg-type arg) ...)
                  min-count)
            form ...)
         (let ([c-file-arg (datum->syntax #'k 'c-file)]
               [c-line-arg (datum->syntax #'k 'c-line)])
           #`(define name
               (locked-foreign-callable
                (let ([counter 0])
                  (lambda (#,c-file-arg #,c-line-arg arg ...)
                    (when min-count
                      (set! counter (fx+ 1 counter))
                      (when (fx>= counter min-count)
                        (printf "~a ~a ~a:~a\n" 'name counter (decode-char* #,c-file-arg) #,c-line-arg)))
                    (let ([result (begin form ...)])
                      (when (and min-count (fx>= counter min-count))
                        (printf "returning from ~a\n" 'name))
                      result)))
                (void* int arg-type ...)
                result-type)))])))

  (define-for-c (c-ephemeron_cons
                 scheme-object ((scheme-object the-car)
                                (scheme-object the-cdr)))
    (ephemeron-cons the-car the-cdr))

  (define-for-c (c-trivial int ((int arg))) arg)

  (define-for-c (c-symbol_is
                 boolean
                 ((scheme-object sym)
                  (void* name-ptr)))
    (let* ([name-str (if (string? sym)
                         sym
                         (symbol->string sym))]
           [n (string-length name-str)])
      (let loop ([i 0])
        (let ([c1 (foreign-ref 'unsigned-8 name-ptr i)])
          (or (and (fx= i n)
                   (zero? c1))
              (and (fx< i n)
                   (let ([c2 (char->integer (string-ref name-str i))])
                     (and (fx= c1 c2)
                          (loop (fx+ i 1))))))))))

  (define eq-hash-table (make-weak-eq-hashtable))

  (define-for-c (c-eq_hash
                 unsigned-32
                 ((scheme-object x)))
    (or (hashtable-ref eq-hash-table x #f)
        (let ([hash (random #x100000000)])
          (hashtable-set! eq-hash-table x hash)
          hash)))

  (define-for-c (c-hashtablep boolean
                              ((scheme-object x)))
    (hashtable? x))

  (define-for-c (c-hashtable_ref
                 scheme-object
                 ((scheme-object table)
                  (scheme-object key)
                  (scheme-object default)))
    (hashtable-ref table key default))

  (define-record-type def-info
    (fields (immutable type-str)
            (immutable c-file)
            (immutable c-line)))

  (define (print-def-info info)
    (printf "original definition at ~a:~a\n"
            (decode-char* (def-info-c-file info))
            (def-info-c-line info)))

  (define-for-c (c-save_pointer boolean
                                ((void* ptr)
                                 (void* type-str)))
    (let ([def-info (hashtable-ref pointer-table ptr #f)])
      (if def-info
          (let ([old-type (def-info-type-str def-info)])
            ;; Separate printfs in case the second one dies before
            ;; completing.
            (printf "duplicate registration of ~x\n" ptr)
            (print-def-info def-info)
            (printf "~a => ~a\n"
                    (decode-char* old-type)
                    (decode-char* type-str))
            #f)
          (begin
            (hashtable-set! pointer-table ptr
                            (make-def-info type-str c-file c-line))
            #t))))

  (define-for-c (c-check_pointer boolean
                                 ((void* ptr)
                                  (void* type-str)))
    (let* ([def-info (hashtable-ref pointer-table ptr #f)]
           [old-type (if def-info (def-info-type-str def-info) 0)])
      (or (strcmp? old-type type-str)
          (begin
            ;; Separate printfs in case the second one dies before
            ;; completing.
            (printf "wrong registration of ~x\n" ptr)
            (print-def-info def-info)
            (printf "expected ~a, got ~a\n"
                    (decode-char* type-str)
                    (decode-char* old-type))
            #f))))

  (define origin-table (make-eq-hashtable))

  (define-for-c (c-save_origin
                 void ((scheme-object obj)))
    (hashtable-set! origin-table obj (cons c-file c-line)))

  (define-for-c (c-print_origin
                 void ((scheme-object obj)))
    (let ([origin (hashtable-ref origin-table obj #f)])
      (if origin
          (printf "~a:~a\n" (decode-char* (car origin)) (cdr origin))
          (printf "unknown\n"))))

  (define-for-c (c-print_to_bytevector
                 scheme-object ((scheme-object obj)))
    (critical-section
     (string->utf8
      (call-with-string-output-port
       (lambda (port)
         (put-datum port obj)
         (put-char port #\x00))))))

  (define-for-c (c-hashtable_values
                 scheme-object ((scheme-object obj)))
    (let-values ([(keys values) (hashtable-entries obj)])
      values))

  (define elisp-funcall
    (case-lambda
     [(fun)
      (elisp-call0 fun)]
     [(fun arg)
      (elisp-call1 fun arg)]
     [(fun . args)
      (elisp-apply fun args)]))

  (define guardian (make-guardian))

  (define-record-type lisp-object-ref
    (fields hi lo))

  (define-syntax with-mutex
    (syntax-rules ()
      [(_ m form1 form2 ...)
       (begin form1 form2 ...)]))

  (define mutex (and (threaded?) (make-mutex)))
  (define id-to-obj-table (make-eqv-hashtable))
  (define obj-to-id-table (make-eq-hashtable))
  (define next-obj-id 0)

  ;; Returns the object ID.
  (define (ensure-scheme-object-id obj)
    (with-mutex mutex
      (or (hashtable-ref obj-to-id-table obj #f)
          (let ([id next-obj-id])
            ;; (printf "assigning scheme object ~a = ~s\n" id obj)
            (hashtable-set! id-to-obj-table id obj)
            (hashtable-set! obj-to-id-table obj id)
            (set! next-obj-id (+ 1 next-obj-id))
            id))))

  (define (forget-scheme-object id)
    ;; (printf "forgetting scheme object ~a\n" id)
    (with-mutex mutex
      (hashtable-delete! id-to-obj-table id)))

  (define (scheme-object-for-id id)
    (with-mutex mutex
      (hashtable-ref id-to-obj-table id #f)))

  ;; If x is a lisp-object-ref, extract the Lisp_Object value as a
  ;; pair of fixnums.
  (define (extract-lisp-object x)
    ;; (printf "in extract-lisp-object\n")
    (and (lisp-object-ref? x)
         (cons (lisp-object-ref-hi x)
               (lisp-object-ref-lo x))))

  ;; Given a Lisp_Object reference represented as a pair (hi, lo) of
  ;; fixnums, and an ID number, create a lisp-object-ref and reigster
  ;; it so Emacs will be notified when it is garbage collected.
  (define (ensure-lisp-object-ref hi lo id)
    ;;(printf "lisp->scheme: ~a ~a" lisp-object lisp-object-id)
    (purge-lisp-object-refs)
    (let ([ref (make-lisp-object-ref hi lo)])
      (guardian ref id)
      ref))

  (define (purge-lisp-object-refs)
    (let loop ()
      (let ([id (guardian)])
        (when id
          (elisp-funcall 'forget-lisp-object id)
          (loop)))))

  (define (identity x) x)

  (define (gc)
    (elisp-funcall 'garbage-collect))

  (define (after-idle from-lisp)
    (collect-notify #t)

    (elisp-funcall 'set 'dummy-var (cons #f #f))

    (gc)
    (set! from-lisp #f)
    (elisp-funcall 'set 'dummy-var 'nil)

    (gc)
    'nil)

  (define abort (foreign-procedure "abort" () void))

  (define (emacs-init)
    (collect-request-handler
     #;elisp-do-scheme-gc
     (lambda ()
       (parameterize ([collect-request-handler list])
         (when (elisp-before-scheme-gc)
           (collect)
           (elisp-after-scheme-gc)))))
    (let (
          #;[stderr (transcoded-port (standard-error-port)
          (native-transcoder))])
      (base-exception-handler
       (lambda (x)
         (base-exception-handler default-exception-handler)
         (display-condition x)
         (newline)
         (abort)
         )))

    #;(when #t
    (printf "running alloc_test\n")     ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ;
    (collect-notify #t)                 ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ;
    ((foreign-procedure __collect_safe "alloc_test" () void)) ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ;
    (exit 1))

    ;; (elisp-funcall 'load "scheme-internal")
    #;(when (elisp-fboundp 'message)
    (elisp-apply 'message '("Hello, world: %S %S\n" 42 1.0)))

    (printf "emacs-init done\n")))

;; Local Variables:
;; mode: scheme
;; eval: (put 'module 'scheme-indent-function 1)
;; eval: (put 'with-mutex 'scheme-indent-function 1)
;; End:
