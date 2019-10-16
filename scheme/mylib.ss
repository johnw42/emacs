(library (mylib)
  (export in-mylib)
  (import (scheme))

  (define (in-mylib)
    (printf "in-mylib\n"))

  (printf "loading mylib\n"))
