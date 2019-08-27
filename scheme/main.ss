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

         c-hashtablep
         c-save_pointer
         c-check_pointer
         print-to-bytevector
         )

  (define elisp-boundp
    (foreign-procedure "scheme_elisp_boundp" (scheme-object) boolean)) 

  (define elisp-fboundp
    (foreign-procedure "scheme_elisp_fboundp" (scheme-object) boolean)) 

  (define elisp-call0
    (foreign-procedure "scheme_elisp_call0"
                       (scheme-object)
                       scheme-object))

  (define elisp-call1
    (foreign-procedure "scheme_elisp_call1"
                       (scheme-object scheme-object)
                       scheme-object))

  (define elisp-apply
    (foreign-procedure "scheme_elisp_apply"
                       (scheme-object scheme-object)
                       scheme-object))

  (define-syntax locked-foreign-callable
    (syntax-rules ()
      [(_ func (arg-type ...) result-type)
       (let ([x (foreign-callable func (arg-type ...) result-type)])
         (lock-object x)
         x)]))

  (define decode-char*
    (let ([memo-ptr #f]
          [memo-result #f])
      (lambda (ptr)
        (unless (eqv? ptr memo-ptr)
          (let* ([count (do ([i 0 (+ 1 i)])
                            ((fxzero? (foreign-ref 'integer-8 ptr i))
                             i))]
                 [str (make-string count)])
            (set! memo-result
                  (do ([i 0 (+ 1 i)])
                      ((>= i count) str)
                    (string-set!
                     str i (integer->char
                            (foreign-ref 'integer-8 ptr i)))))))
        memo-result)))

  (define-syntax define-for-c
    (syntax-rules ()
      [(_ (name min-count
                ((arg-type arg) ...)
                result-type)
          form ...)
       (define name
         (let ([counter 0])
           (locked-foreign-callable
            (lambda (file line arg ...)
              (when min-count
                (set! counter (fx+ 1 counter))
                (when (fx>= counter min-count)
                  (printf "~a ~a ~a:~a\n" 'name counter (decode-char* file) line)))
              (let ([result (begin form ...)])
                (when (and min-count (fx>= counter min-count))
                  (printf "returning from ~a\n" 'name))
                result))
            (void* int arg-type ...)
            result-type)))]))

  (define-for-c (c-hashtablep #f
                              ((scheme-object x))
                              boolean)
    (hashtable? x))

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

  (define-for-c (c-save_pointer #f
                                ((void* ptr)
                                 (void* type-str))
                                boolean)
    #;(printf "in save-pointer ~a ~a\n" ptr type-str)
    (let ([old-type (hashtable-ref pointer-table ptr #f)])
      (if old-type
          (begin
            (printf "duplicate registration of ~x: ~a => ~a\n"
                    ptr (decode-char* old-type) type-str)
            #f)
          (begin
            (hashtable-set! pointer-table ptr type-str)
            #t))))

  (define-for-c (c-check_pointer #f
                                 ((void* ptr)
                                  (void* type-str))
                                 boolean)
    ;;(printf "in check-pointer ~a ~a\n" ptr type-str)
    (let ([old-type (hashtable-ref pointer-table ptr #f)])
      (if (equal? old-type type-str)
          #t
          (begin
            (printf "wrong registration of ~x; expected ~a, got ~a\n"
                    ptr type-str old-type)
            #f))))

  (define-for-c (print-to-bytevector #f
                                     ((scheme-object obj))
                                     scheme-object)
    (string->utf8
     (call-with-string-output-port
      (lambda (port)
        (put-datum port obj)
        (put-char port #\x00)))))
  
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
    (printf "called scheme emacs-init\n")
    (collect-request-handler
     (lambda ()
       (printf "skipping scheme garbage collection\n")))
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
    (printf "running alloc_test\n")     ; ; ; ; ; ; ; ; ; ; ; ; ; ;
    (collect-notify #t)                 ; ; ; ; ; ; ; ; ; ; ; ; ; ;
    ((foreign-procedure __collect_safe "alloc_test" () void)) ; ; ; ; ; ; ; ; ; ; ; ; ; ;
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
