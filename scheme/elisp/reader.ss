#!chezscheme
(library (elisp reader)
  (export elisp-read)
  (import (rnrs (6))
          (only (chezscheme)
                char-
                errorf
                format))

  (define non-symbol-chars (string->list "\"';()[]#`,"))

  (define (symbol-char? c)
    (and (char? c)
         (char>? c #\x20)
         (not (eqv? c #\xA0))
         (or (char>=? c #\x80)
             (not (memv c non-symbol-chars)))))

  (define line 1)
  (define column 1)

  (define (emacs-string->number str)
    (let ([n (string-length str)])
      (string->number
       (if (and (> n 2)
                (eqv? #\0 (string-ref str 0))
                (not (char-numeric? (string-ref str 1))))
           (string-append
            "#" (substring str 1 n))
           str))))

  (define modifier-alist
    '((alt . #x0400000)
      (super . #x0800000)
      (hyper . #x1000000)
      (shift . #x2000000)
      (ctrl . #x4000000)
      (meta . #x8000000)))

  (define (modifier->bit mod)
    (cdr (assq mod modifier-alist)))

  (define modifier-mask
    (fold-left
     (lambda (accum pair)
       (bitwise-ior accum (cdr pair)))
     0 modifier-alist))

  (define ctrl-modifier-bit (modifier->bit 'ctrl))

  (define (translate-ctrl-modifier n)
    (let* ([mod-bits (bitwise-and n
                                  (bitwise-not modifier-mask)
                                  (bitwise-not ctrl-modifier-bit))]
           [char-bits (bitwise-and n modifier-mask)]
           [c (integer->char char-bits)])
      (cond
       [(eqv? #\? c)
        (bitwise-ior #x7F mod-bits)]
       [(or (char<=? #\@ c #\_)
            (char<=? #\a c #\z))
        (bitwise-and char-bits #x1F)]
       [else n])))

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

  ;; Converts an emacs code point to a character and a list of modifier
  ;; symbols.
  (define (analyze-code-point n)
    (let loop ([bits n]
               [mods '()]
               [pairs modifier->bit])
      (if (null? pairs)
          (values (integer->char bits)
                  mods)
          (let ([mod (car pairs)]
                [bit (cdr pairs)])
            (if (zero? (bitwise-and bits bit))
                (loop bits mods (cdr pairs))
                (loop (- bits bit)
                      (cons mod mods)
                      (cdr pairs)))))))

  (define (next-char)
    (let ([c (read-char)])
      (set! column (+ 1 column))
      (when (eqv? #\newline c)
        (set! line (+ 1 line))
        (set! column 1))
      c))

  (define (skip-char c)
    (unless (eqv? c (next-char))
      (syntax-error (format "Expected ~a" c))))

  (define (next-sexp)
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
               [(#\") (read-string)]
               [(#\?) (read-char-literal)]
               [(#\#) (read-sharp)]
               [else
                (cond
                 [(symbol-char? c)
                  (read-symbol-or-number)]
                 [(eof-object? c) #f]
                 [else
                  (syntax-error (format "Invalid char: ~s" c))])])])
        ;;(printf ">> ~s\n" sexp)
        sexp)))

  (define (read-sharp)
    (skip-char #\#)
    (case (next-char)
      [(#\s)
       (let ([data (read-list)])
         (syntax-error "TODO: implement hash tables from read1()"))]
      [(#\^)
       (case (peek-char)
         [(#\[)
          (let ([v (read-vector)])
            (syntax-error "TODO: implement char tables from read1()"))]
         [(#\^)
          (let ([v (read-vector)])
            (syntax-error "TODO: implement sub char tables from read1()"))])]
      [(#\&)
       (syntax-error "TODO: implement bool vectors from read1()")]
      [(#\[)
       (syntax-error "TODO: implement reading compiled functions from read1()")]
      [(#\()
       (read-list)
       (syntax-error "TODO: implement string properties from read1()")]
      [(#\@)
       (syntax-error "TODO: implement skipping bytes from read1()")]
      [(#\!)
       (let loop ()
         (unless (eqv? #\newline (next-char))
           (loop)))
       (next-sexp)]
      [(#\$)
       (syntax-error "TODO: Vload_file_name from read1()")]
      [(#\')
       (list 'function (next-sexp))]
      [(#\:)
       (let-values ([(chars quoted?) (read-symbol-chars)])
         (syntax-error "TODO: implement uninterned symbols"))
       (list 'function (next-sexp))]
      [(#\#)
       '||]
      [else
       (syntax-error "not implemented: #")]))

  (define (read-char-literal)
    (skip-char #\?)
    (let ([c (next-char)])
      (if (eqv? #\\ c)
          (read-escape-seq #f)
          c)))

  (define (syntax-error message)
    (errorf 'elisp-read "Syntax error at ~a:~a: ~a"
            line column message))

  (define (skip-comment)
    (do ()
        ((eqv? #\newline (next-char)))))

  (define (skip-whitespace)
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
          (loop)]))))

  (define (read-symbol-chars)
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
                  quoted?)]))))
  (define (read-symbol-or-number)
    (let-values ([(str quoted?)
                  (read-symbol-chars)])
      (or (and (not quoted?)
               (emacs-string->number str))
          (string->symbol str))))

  (define (read-string)
    (skip-char #\")
    (let loop ([chars '()])
      (let ([c (next-char)])
        (case c
          [(#\")
           (list->string (reverse chars))]
          [(#\\)
           (loop
            (let ([c (read-escape-seq #t)])
              (if c
                  (begin
                    (unless (zero? (bitwise-and
                                    c
                                    modifier-mask))
                      (syntax "Invalid character in string"))
                    (cons (integer->char c) chars))
                  chars)))]
          [else
           (loop (cons c chars))]))))

  ;; Reads escape sequence.  Returns the emacs code point,
  ;; including modifier bits, or #f.
  (define (read-escape-seq string?)
    (let ([c (peek-char)]
          [just (lambda (c)
                  (next-char)
                  (char->integer c))]
          [modified
           (lambda (modifier)
             (read-modifier-char string? modifier))])
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
        [(#\space) (if string? #f (just #\space))]
        [(#\M #\S #\H #\A #\s)
         (next-char)
         (case c
           [(#\M) (modified 'meta)]
           [(#\S) (modified 'shift)]
           [(#\H) (modified 'hyper)]
           [(#\A) (modified 'alt)]
           [(#\s)
            (if (or string?
                    (not (eqv? #\- (peek-char))))
                (just #\space)
                (modified 'super))])]
        [(#\C #\^)
         (when (eqv? #\C c)
           (consume-hyphen))
         (translate-ctrl-modifier
          (modified 'ctrl))]
        ;; Octal escape.
        [(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7)
         (do ([accum (char- c #\0)
                     (+ (* 8 accum)
                        (char- (next-char) #\0))])
             ((not (char<=? #\0 (peek-char) #\7))
              accum))]
        [(#\x) (read-hex-char 7)]
        [(#\u) (read-hex-char 4)]
        [(#\U) (read-hex-char 8)]
        [(#\N)
         (syntax-error
          (format "Named characters not implemented: ~a"
                  (read-char-name)))]
        [else (just c)])))

  (define (read-char-name)
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
                      chars))))))

  (define (read-hex-char max-digits)
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
             (list->string (reverse chars)) 16)))))

  (define (read-modifier-char string? modifier)
    (consume-hyphen)
    (let* ([c (next-char)])
      (bitwise-ior
       (if (eqv? #\\ c)
           (read-escape-seq string?)
           (char->integer c))
       (modifier->bit modifier))))

  (define (consume-hyphen)
    (unless (eqv? #\- (next-char))
      (syntax-error "Invalid escape character syntax")))

  (define (read-vector)
    (skip-char #\[)
    (let loop ([accum '()])
      (skip-whitespace)
      (case (peek-char)
        [(#\])
         (next-char)
         (reverse accum)]
        [else
         (loop (cons (next-sexp) accum))])))

  (define (read-list)
    (skip-char #\()
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
           (skip-char #\))
           result)]
        [else
         (loop (cons (next-sexp) accum))])))

  (define (elisp-read)
    (skip-whitespace)
    (next-sexp)))
