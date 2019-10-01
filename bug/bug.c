#include <stdio.h>
#include "scheme.h"

int main() {
  Sscheme_init(0);
  Sregister_boot_file(SCHEME_DIR "/scheme.boot");
  Sbuild_heap(0, 0);
  Sscheme_script("script.scm", 0, 0);
  int (*fun)(int) =
    (void *)Sforeign_callable_entry_point
    (Stop_level_value(Sstring_to_symbol("identity")));
  printf("%d\n", fun(42));
  return 0;
}
