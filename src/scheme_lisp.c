#include <config.h>

#include "lisp.h"
#include <stdio.h>

#ifdef CHEZ_SCHEME

#include <scheme.h>

void scheme_init(void) {
  printf("Starting scheme.\n");
  Sscheme_init(NULL);
  Sregister_boot_file("/usr/local/google/home/jrw/.local/lib/csv9.5.2/a6le/scheme.boot");
  printf("building heap\n");
  Sbuild_heap(NULL, NULL);
  Sscheme_script("/usr/local/google/home/jrw/git/schemacs/scheme/main.ss", 0, NULL);
  Scall0(Stop_level_value(Sstring_to_symbol("defined-in-scheme")));
  /* printf("Scheme shutting down.\n"); */
  /* Sscheme_deinit(); */
  /* printf("Emacs shutting down.\n"); */
  /* exit(0); */
}

#endif
