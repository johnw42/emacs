#!chezscheme
(import (chezscheme))

(printf "main.ss running\n")

(module (emacs-init
         ;; alloc-id
         ;; release-id
         obj->id
         id->obj
         lisp->scheme
         extract-lisp-object
         )

  (define elisp-boundp
    (foreign-procedure "scheme_elisp_boundp" (scheme-object) boolean)) 

  (define elisp-fboundp
    (foreign-procedure "scheme_elisp_fboundp" (scheme-object) boolean)) 
  
  (define elisp-intern
    (foreign-procedure "scheme_elisp_intern" (utf-8) scheme-object)) 

  (define elisp-apply
    (foreign-procedure "scheme_elisp_apply"
                       (scheme-object scheme-object)
                       scheme-object)) 

  (define guardian (make-guardian))

  (define-record-type lisp-object-ref
    (fields lisp-object))

  (define-syntax with-mutex
    (syntax-rules ()
      [(_ m form1 form2 ...)
       (begin form1 form2 ...)]))

  (define mutex (and (threaded?) (make-mutex)))
  (define id-to-obj-table (make-eqv-hashtable))
  (define obj-to-id-table (make-eq-hashtable))
  (define next-obj-id 0)

  (define (obj->id obj)
    (with-mutex mutex
      (hashtable-set! id-to-obj-table next-obj-id obj)
      (set! next-obj-id (+ 1 next-obj-id))))

  (define (release-id id)
    (with-mutex mutex
      (hashtable-delete id-to-obj-table id)))

  (define (id->obj id)
    (with-mutex mutex
      (hashtable-ref id-to-obj-table id #f)))

  (define (extract-lisp-object x)
    (and (lisp-object-ref? x)
         (lisp-object-ref-lisp-object x)))

  (define (lisp->scheme lisp-object lisp-object-id)
    (printf "lisp->scheme: ~a ~a" lisp-object lisp-object-id)
    (purge-lisp-object-refs)
    (let ([ref (make-lisp-object-ref lisp-object)])
      (guardian ref lisp-object-id)
      ref))

  (define (purge-lisp-object-refs)
    (let loop ()
      (let ([id (guardian)])
        (when id
          ;; TODO: tell emacs to forget the ID
          (loop)))))
  
  (define (emacs-init)
    (printf "called defined-in-scheme\n")
    (when (elisp-fboundp 'message)
      (elisp-apply 'message '("Hello, world: %S %S\n" 42 1.0)))))

;; Local Variables:
;; mode: scheme
;; eval: (put 'module 'scheme-indent-function 1)
;; eval: (put 'with-mutex 'scheme-indent-function 1)
;; End:
