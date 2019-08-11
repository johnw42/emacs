#include "lisp.h"
#include <stdio.h>

void scheme_init() {
  printf("Starting scheme.\n");
  Sscheme_init(NULL);
  Sregister_boot_file("/usr/local/google/home/jrw/.local/lib/csv9.5.2/a6le/scheme.boot");
  printf("building heap\n");
  Sbuild_heap(NULL, NULL);
  Sscheme_script("/usr/local/google/home/jrw/git/schemacs/scheme/main.ss", 0, argv);
  Scall0(Stop_level_value(Sstring_to_symbol("defined-in-scheme")));
  /* printf("Scheme shutting down.\n"); */
  /* Sscheme_deinit(); */
  /* printf("Emacs shutting down.\n"); */
  /* exit(0); */
}
