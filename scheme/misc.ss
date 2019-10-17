(library (misc)
  (export make-obarray-table
          pointer-hash
          pointer-table
          object-is-str?
          eq-hash
          print-to-bytevector
          wrap-function
          emacs-init)
  (import (chezscheme))

(define (emacs-init)
  (collect-request-handler
   (lambda ()
     (parameterize ([collect-request-handler
                     (lambda ()
                       (printf "skipping gc from scheme\n"))])
       (when (elisp-before-scheme-gc)
         (printf "invoking (collect) from scheme\n")
         (collect)
         (elisp-after-scheme-gc)))))
  (base-exception-handler
   (lambda (x)
     (base-exception-handler default-exception-handler)
     (display-condition x)
     (newline)
     (abort))))

(define elisp-do-scheme-gc
  (foreign-procedure "do_scheme_gc" () void))

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

(define (object-is-str? obj name-ptr)
  (let* ([name-str (if (string? obj)
                       obj
                       (symbol->string obj))]
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

(define (eq-hash x)
  (or (hashtable-ref eq-hash-table x #f)
      (let ([hash (random #x100000000)])
        (hashtable-set! eq-hash-table x hash)
        hash)))

(define (print-to-bytevector obj)
  (critical-section
   (string->utf8
    (call-with-string-output-port
     (lambda (port)
       (parameterize ([print-graph #t])
         (put-datum port obj)
         (put-char port #\x00)))))))

;; (define-syntax subr-invoker
;;   (lambda (x)
;;     (syntax-case x ()
;;       [(_ f max-args)
;;        (subr-invoker f )
;;        ])))

;; (define-syntax wrap-function-cases
;;   (lambda (x)
;;     (syntax-case x ()
;;       [(x f ...)
;;        (cond)])))

(define (wrap-function func-ptr min-args max-args)
  (let ([proc (case max-args
                (0 (foreign-procedure func-ptr () scheme-object))
                (1 (foreign-procedure func-ptr (scheme-object) scheme-object))
                (2 (foreign-procedure func-ptr (scheme-object scheme-object) scheme-object))
                (3 (foreign-procedure func-ptr (scheme-object scheme-object scheme-object) scheme-object))
                (4 (foreign-procedure func-ptr (scheme-object scheme-object scheme-object scheme-object) scheme-object))
                (5 (foreign-procedure func-ptr (scheme-object scheme-object scheme-object scheme-object scheme-object) scheme-object))
                (6 (foreign-procedure func-ptr (scheme-object scheme-object scheme-object scheme-object scheme-object scheme-object) scheme-object))
                (7 (foreign-procedure func-ptr (scheme-object scheme-object scheme-object scheme-object scheme-object scheme-object scheme-object) scheme-object))
                (8 (foreign-procedure func-ptr (scheme-object scheme-object scheme-object scheme-object scheme-object scheme-object scheme-object scheme-object) scheme-object))
                ;; MANY
                (-2 (foreign-procedure func-ptr (iptr void*) scheme-object))
                ;; UNEVALLED
                (-1 (foreign-procedure func-ptr (scheme-object) scheme-object)))])
    proc)))
