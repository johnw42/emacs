/* Trivial unexec for Solaris.  */

#include <config.h>
#include "unexec.h"

#include <dlfcn.h>

#include "lisp.h"
#include "buffer.h"
#include "coding.h"

void
unexec (const char *new_name, const char *old_name)
{
  ENTER_LISP_FRAME ((), data, errstring);

  if (! dldump (0, new_name, RTLD_MEMORY))
    EXIT_LISP_FRAME_VOID ();

  data = list1 (build_string (new_name));
  synchronize_system_messages_locale ();
  errstring = code_convert_string_norecord (build_string (dlerror ()),
					    Vlocale_coding_system, 0);

  xsignal (Qfile_error,
	   Fcons (build_string ("Cannot unexec"), Fcons (errstring, data)));
  EXIT_LISP_FRAME_VOID ();
}
