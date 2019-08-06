(define non-symbol-chars (string->list "\"';()[]#`,"))

(define (symbol-char? c)
  (and (char? c)
       (char>? c #\x20)
       (not (eqv? c #\xA0))
       (or (char>=? c #\x80)
           (not (memv c non-symbol-chars)))))

(define line 1)
(define column 1)

(define-syntax and-let*
  (syntax-rules ()
    [(_ () body ...)
     (let () body ...)]
    [(_ ([var expr] [vars exprs] ...) body ...)
     (let ([var expr])
       (and var
            (and-let* ([vars exprs] ...) body ...)))]))

(define (emacs-string->number str)
  (let ([n (string-length str)])
    (string->number
     (if (and (> n 2)
              (eqv? #\0 (string-ref str 0))
              (not (char-numeric? (string-ref str 1))))
         (string-append
          "#" (substring str 1 n))
         str))))

;; (define-syntax push!
;;   (syntax-rules ()
;;     [(_ value stack)
;;      (set! stack (cons value stack))]))

;; (define-syntax pop!
;;   (syntax-rules ()
;;     [(_ stack)
;;      (let ([value (car stack)])
;;        (set! stack (cdr stack))
;;        value)]))

(define (elisp-read)
  (letrec
      ([char-buf #f]
       [last-read-char #f]
       [next-char
        (lambda ()
          (let ([c (or char-buf
                       (read-char))])
            (set! char-buf #f)
            (set! last-read-char c)
            (set! column (+ 1 column))
            (when (eqv? #\newline c)
              (set! line (+ 1 line))
              (set! column 1))
            c))]
       [replace-char
        (lambda ()
          (assert last-read-char)
          (assert (not char-buf))
          (set! char-buf last-read-char)
          (set! last-read-char #f))]
       [next-sexp
        (lambda ()
          (let ([c (next-char)])
            (let ([sexp
                   (case c
                     [(#\()
                      (replace-char)
                      (read-list)]
                     [(#\')
                      (skip-whitespace)
                      (list 'quote (next-sexp))]
                     [(#\`)
                      (skip-whitespace)
                      (list '|`| (next-sexp))]
                     [(#\,)
                      (if (eqv? #\@ (next-char))
                          (begin
                            (skip-whitespace)
                            (list '|,@| (next-sexp)))
                          (begin
                            (replace-char)
                            (skip-whitespace)
                            (list '|,| (next-sexp))))
                      ]
                     [(#\")
                      (replace-char)
                      (read-string)]
                     [(#\?) (syntax-error "not implemented: ?")]
                     [(#\#)
                      (case (next-char)
                        [(#\')
                         (list 'function (next-sexp))]
                        [else
                         (syntax-error "not implemented: #")])]
                     [else
                      (cond
                       [(symbol-char? c)
                        (replace-char)
                        (read-symbol-or-number)]
                       [(eof-object? c) #f]
                       [else
                        (syntax-error (format "Invalid char: ~s" c))])])])
              ;;(printf ">> ~s\n" sexp)
              sexp)))]
       [syntax-error
        (lambda (message)
          (errorf 'elisp-read "Syntax error at ~a:~a: ~a"
                  line column message))]
       [skip-comment
        (lambda ()
          (do ()
              ((eqv? #\newline (next-char)))))]
       [skip-whitespace
        (lambda ()
          (let loop ()
            (let ([c (next-char)])
              (cond
               [(eqv? c #\;)
                (skip-comment)
                (loop)]
               [(and (char? c)
                     (char-whitespace? c))
                (loop)]
               [else
                (replace-char)]))))]
       [read-symbol-chars
        (lambda ()
          (let loop ([chars '()]
                     [quoted? #f])
            (let ([c (next-char)])
              (cond
               [(eqv? #\\ c)
                (loop chars #t)]
               [(symbol-char? c)
                (loop (cons c chars)
                      quoted?)]
               [else
                (replace-char)
                (values (list->string (reverse chars))
                        quoted?)]))))]
       [read-symbol-or-number
        (lambda ()
          (let-values ([(str quoted?)
                        (read-symbol-chars)])
            (or (and (not quoted?)
                     (emacs-string->number str))
                (string->symbol str))))]
       [read-string
        (lambda ()
          (next-char)
          (let loop ([chars '()])
            (let ([c (next-char)])
              (case c
                [(#\")
                 (list->string (reverse chars))]
                [(#\\)
                 (loop
                  (let ([c1 (read-escape-seq #t)])
                    (if c1
                        (cons c1 chars)
                        chars)))]
                [else
                 (loop (cons c chars))]))))]
       [read-escape-seq
        (lambda (string?)
          (let ([c (next-char)])
            (case c
              [(#\a) #\alarm]
              [(#\b) #\backspace]
              [(#\d) #\delete]
              [(#\e) #\esc]
              [(#\f) #\linefeed]
              [(#\n) #\newline]
              [(#\r) #\return]
              [(#\t) #\tab]
              [(#\v) #\vtab]
              [(#\newline) #f]
              [(#\space) (if string? #f #\space)]
              ;; TODO: Hande escapes as per read_escape()
              [else c])))]
       [read-list
        (lambda ()
          (next-char)
          (let loop ([accum '()])
            (skip-whitespace)
            (case (next-char)
              [(#\))
               (reverse accum)]
              [(#\.)
               (when (null? accum)
                 (syntax-error "expected sexp"))
               (skip-whitespace)
               (let ([result
                      (append (cdr accum)
                              (cons (car accum)
                                    (next-sexp)))])
                 (skip-whitespace)
                 (unless (eqv? #\) (next-char))
                   (syntax-error "expecting )"))
                 result)]
              [else
               (replace-char)
               (loop (cons (next-sexp) accum))])))])
    (skip-whitespace)
    (next-sexp)))

(define (elisp-read-file path)
  (with-input-from-file path
    (lambda ()
      (let loop ([sexps '()])
        (or (and-let* ([sexp (elisp-read)])
                      (loop (cons sexp sexps)))
            (reverse sexps))))))

(for-each pretty-print
          (elisp-read-file "lisp/emacs-lisp/macroexp.el"))

;; (with-input-from-file "lisp/emacs-lisp/macroexp.el"
;;   (lambda ()
;;     (let loop ()
;;       (and-let* ([sexp (elisp-read)])
;;                 (pretty-print sexp)
;;                 (loop)))))
