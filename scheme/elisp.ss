;; Gets the a list of at most `max-depth` strings representing the
;; stack trace for the contiuation `k`.
(define (get-stack-trace k max-depth)
  (do ([cur (inspect/object k) (cur 'link)]
       [i 0 (+ i 1)]
       [stack '()
              (cons
               (call-with-values (lambda () (cur 'source-path))
                 (case-lambda
                  [(file line . _)
                   (format "at ~a (~a:~a)"
                           ((cur 'code) 'name) file line)]
                  [else (format "no source at ~a" i)]))
               stack)])
      [(or (not cur)
           (<= (cur 'depth) 1)
           (> i max-depth))
       (reverse stack)]))

;; Prints a stack trace for continuation `k`, preceeded by a label
;; `label`, containing at most `max-depth` items.  If `k` is omitted,
;; the current continutation is used.
(define print-stack-trace
  (case-lambda
   [(label k max-depth)
    (printf "backtrace of [~a] as following:\n" label)
    (for-each (lambda (line)
                (printf "\t~a\n" line))
              (get-stack-trace k max-depth))]
   [(label max-depth)
    (call/cc (lambda (k)
               (print-stack-trace label k max-depth)))]))

(debug-level 3)
(let ([old-handler (base-exception-handler)])
  (base-exception-handler
   (lambda (err)
     (when (continuation-condition? err)
       (print-stack-trace "exception" (condition-continuation err) 10))
     (old-handler err))))

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

(define (char-ior n c)
  (integer->char (bitwise-ior n (char->integer c))))

(define (translate-ctrl-modifier c mods)
  (assert (eqv? 'ctrl (car mods)))
  (cond
   [(eqv? #\? c)
    (values #\x7F (cdr mods))]
   [(or (char<=? #\@ c #\_)
        (char<=? #\a c #\z))
    (values (integer->char (bitwise-and
                            (char->integer (char-upcase c))
                            #x9F))
            (cdr mods))]
   [else
    (values c mods)]))

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
       [next-sexp
        (lambda ()
          (let ([c (peek-char)])
            (let ([sexp
                   (case c
                     [(#\()
                      (read-list)]
                     [(#\')
                      (next-char)
                      (skip-whitespace)
                      (list 'quote (next-sexp))]
                     [(#\`)
                      (next-char)
                      (skip-whitespace)
                      (list '|`| (next-sexp))]
                     [(#\,)
                      (next-char)
                      (if (eqv? #\@ (peek-char))
                          (begin
                            (next-char)
                            (skip-whitespace)
                            (list '|,@| (next-sexp)))
                          (begin
                            (skip-whitespace)
                            (list '|,| (next-sexp))))
                      ]
                     [(#\")
                      (read-string)]
                     [(#\?) (syntax-error "not implemented: ?")]
                     [(#\#)
                      (next-char)
                      (case (next-char)
                        [(#\')
                         (list 'function (next-sexp))]
                        [else
                         (syntax-error "not implemented: #")])]
                     [else
                      (cond
                       [(symbol-char? c)
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
            (let ([c (peek-char)])
              (cond
               [(eqv? c #\;)
                (next-char)
                (skip-comment)
                (loop)]
               [(and (char? c)
                     (char-whitespace? c))
                (next-char)
                (loop)]))))]
       [read-symbol-chars
        (lambda ()
          (let loop ([chars '()]
                     [quoted? #f])
            (let ([c (peek-char)])
              (cond
               [(eqv? #\\ c)
                (next-char)
                (loop chars #t)]
               [(symbol-char? c)
                (next-char)
                (loop (cons c chars)
                      quoted?)]
               [else
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
          (assert (eqv? #\" (next-char)))
          (let loop ([chars '()])
            (let ([c (next-char)])
              (case c
                [(#\")
                 (list->string (reverse chars))]
                [(#\\)
                 (loop
                  (let-values ([(c mods) (read-escape-seq #t)])
                    (if c
                        (cons c chars)
                        chars)))]
                [else
                 (loop (cons c chars))]))))]
       [read-escape-seq
        ;; Reads escape sequence.  Returns character and modifier
        ;; list, or (values #f '()).
        (lambda (string?)
          (let ([c (peek-char)]
                [just (lambda (c)
                        (next-char)
                        (values c '()))]
                )
            (case c
              [(#\a) (just #\alarm)]
              [(#\b) (just #\backspace)]
              [(#\d) (just #\delete)]
              [(#\e) (just #\esc)]
              [(#\f) (just #\linefeed)]
              [(#\n) (just #\newline)]
              [(#\r) (just #\return)]
              [(#\t) (just #\tab)]
              [(#\v) (just #\vtab)]
              [(#\newline) #f]
              [(#\space) (just (if string? #f #\space))]
              [(#\M #\S #\H #\A #\s)
               (next-char)
               (case c
                 [(#\M) (read-modifier-char 'meta)]
                 [(#\S) (read-modifier-char 'shift)]
                 [(#\H) (read-modifier-char 'hyper)]
                 [(#\A) (read-modifier-char 'alt)]
                 [(#\s)
                  (if (or string?
                          (not (eqv? #\- (peek-char))))
                      (just #\space)
                      (read-modifier-char 'super))])]
              [(#\C #\^)
               (when (eqv? #\C c)
                 (consume-hyphen))
               (call-with-values
                   (read-modifier-char 'ctrl)
                 translate-ctrl-modifier)]
              ;; Octal escape.
              [(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7)
               (do ([accum (char- c #\0)
                           (+ (* 8 accum)
                              (char- (next-char) #\0))])
                   ((not (char<=? #\0 (peek-char) #\7))
                    (integer->char accum)))]
              [(#\x) (just (read-hex-digits 7))]
              [(#\u) (just (read-hex-digits 4))]
              [(#\U) (just (read-hex-digits 8))]
              [(#\N)
               (syntax-error
                (format "Named characters not implemented: ~a"
                        (read-char-name)))]
              [else (just c)])))]
       [read-char-name
        (lambda ()
          (unless (eqv? #\{ (next-char))
            (syntax-error "Expected { after \\N"))
          (let loop ([chars '()])
            (let ([c (next-char)])
              (unless (char<? c #\x80)
                (syntax-error
                 (format "Invalid character U+~4,'0x in character name"
                         c)))
              (if (eqv? #\} (peek-char))
                  (list->string (reverse chars))
                  (loop (if (or (null? chars)
                                (not (char-whitespace? c))
                                (not (char-whitespace? (car chars))))
                            (cons c chars)
                            chars))))))]
       [read-hex-char
        (lambda (max-digits)
          (let loop ([chars '()]
                     [to-read max-digits])
            (let ([c (peek-char)])
              (if (and (not (zero? to-read))
                       (or (char-numeric? c)
                           (char-ci<=? #\a c #\f)))
                  (begin
                    (next-char)
                    (loop (cons c chars) (- to-read 1)))
                  (string->number
                   (integer->char
                    (list->string (reverse chars))) 16)))))]
       [add-modifier
        (lambda (modifier)
          (lambda (c mods)
            (values c (cons modifier mods))))]
       [read-modifier-char
        (lambda (modifier)
          (consume-hyphen)
          (let* ([c (next-char)])
            (call-with-values (if (eqv? #\\ c)
                                  (read-escape)
                                  (values c '()))
              (add-modifier modifier))))]
       [consume-hyphen
        (lambda ()
          (unless (eqv? #\- (next-char))
            (syntax-error "Invalid escape character syntax")))]
       [read-list
        (lambda ()
          (assert (eqv? #\( (next-char)))
          (let loop ([accum '()])
            (skip-whitespace)
            (case (peek-char)
              [(#\))
               (next-char)
               (reverse accum)]
              [(#\.)
               (next-char)
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
