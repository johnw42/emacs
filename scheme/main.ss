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
         identity)

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

  (define (emacs-init)
    (printf "called scheme emacs-init\n")

    ;; (elisp-funcall 'load "scheme-internal")
    #;(when (elisp-fboundp 'message)
    (elisp-apply 'message '("Hello, world: %S %S\n" 42 1.0)))))

;; Local Variables:
;; mode: scheme
;; eval: (put 'module 'scheme-indent-function 1)
;; eval: (put 'with-mutex 'scheme-indent-function 1)
;; End:
