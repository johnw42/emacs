#include <config.h>

#ifdef CHEZ_SCHEME

#include "lisp.h"
#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>

#pragma GCC diagnostic ignored "-Wdiscarded-qualifiers"

static EMACS_INT next_lisp_object_id = 0;
static Lisp_Object lisp_object_to_id_table;
static Lisp_Object lisp_id_to_object_table;

static ptr lisp_to_scheme(Lisp_Object lisp_obj) {
  if (NILP(lisp_obj)) {
    return Sfalse;
  }

  if (SYMBOLP(lisp_obj)) {
    Lisp_Object name = SYMBOL_NAME(lisp_obj);
    ptrdiff_t len = SBYTES(name);
    char *buffer = alloca(1 + len);
    memcpy(buffer, SSDATA(name), len);
    buffer[len] = '\0';
    return Sstring_to_symbol(buffer);
  }

  printf("lisp_to_scheme: %"PRIdPTR"\n", (intptr_t)XLI(lisp_obj));
  /* if (!NILP(call1(Qscheme_value_ref_p, lisp_obj))) { */
  /*   return Scall1(Stop_level_value(Sstring_to_symbol("id->obj")), */
  /*                 Sunsigned(XSCHEME_REF(lisp_obj)->id)); */
  /* } */
    
  if (STRINGP(lisp_obj)) {
    return Sstring_of_length(SSDATA(lisp_obj), SBYTES(lisp_obj));
  }

  if (INTEGERP(lisp_obj)) {
    return Sinteger(XINT(lisp_obj));
  }

  if (FLOATP(lisp_obj)) {
    return Sflonum(XFLOAT_DATA(lisp_obj));
  }

  /* Make sure this Lisp object has an ID number and wrap it as a
     Scheme object. */
  EMACS_INT obj_id;
  Lisp_Object lisp_id = Fgethash(lisp_obj, lisp_object_to_id_table, Qnil);
  if (NILP(lisp_id)) {
    obj_id = next_lisp_object_id++;
    printf("assigning ID: %d %d\n", (int)obj_id, (int)NILP(lisp_obj));
    Lisp_Object lisp_id = make_number(obj_id);
    Fputhash(lisp_obj, lisp_id, lisp_object_to_id_table);
    Fputhash(lisp_id, lisp_id, lisp_id_to_object_table);
  } else {
    eassert(INTEGERP(lisp_id));
    obj_id = XINT(lisp_id);
  }
  return Scall2(Stop_level_value(Sstring_to_symbol("lisp->scheme")),
                Sinteger(XLI(lisp_obj)),
                Sinteger(obj_id));
}

static Lisp_Object scheme_to_lisp(ptr scheme_obj) {
  ptr obj_value;

  if (Ssymbolp(scheme_obj)) {
    return Fintern(scheme_to_lisp(Ssymbol_to_string(scheme_obj)), Qnil);
  }
  if (Sstringp(scheme_obj)) {
    iptr len = Sstring_length(scheme_obj);
    char *buffer = alloca(len);
    for (iptr i = 0; i < len; i++) {
      buffer[i] = Sstring_ref(scheme_obj, i);
    }
    return make_string(buffer, len);
  }
  if (Sfixnump(scheme_obj) || Sbignump(scheme_obj)) {
    return make_number(Sinteger_value(scheme_obj));
  }
  if (Sflonump(scheme_obj)) {
    return make_float(Sflonum_value(scheme_obj));
  }

  obj_value = Scall1(Stop_level_value(Ssymbol_to_string("extract-lisp-object")),
                     scheme_obj);
  if (obj_value != Sfalse) {
    eassert(Sfixnump(obj_value));
    return XIL(Sinteger_value(obj_value));
  }

  obj_value = Scall1(Stop_level_value(Ssymbol_to_string("obj->id")),
                     scheme_obj);
  eassert(Sfixnump(obj_value));
  return call1(Qensure_scheme_value_ref, make_number(Sinteger_value(obj_value)));
}

DEFUN ("scheme-funcall", Fscheme_funcall, Sscheme_funcall, 1, MANY, 0,
       doc: /* TODO */)
  (ptrdiff_t nargs, Lisp_Object *args)
{
  Sinitframe(nargs);
  for (iptr i = 1; i < nargs; i++) {
    Sput_arg(i, lisp_to_scheme(args[i]));
  }
  return scheme_to_lisp(Scall(lisp_to_scheme(args[0]), nargs));
}

DEFUN ("scheme-toplevel-value", Fscheme_toplevel_value, Sscheme_toplevel_value, 1, 1, 0,
       doc: /* TODO */)
  (Lisp_Object symbol)
{
  CHECK_SYMBOL(symbol);
  return scheme_to_lisp(Stop_level_value(lisp_to_scheme(symbol)));
}

static ptr scheme_elisp_intern(const char *name) {
  printf("elisp-intern: %s\n", name);
  Lisp_Object symbol = Fintern(make_string(name, strlen(name)), Qnil);
  printf("elisp-intern -> %"PRIdPTR"\n", (intptr_t)XLI(symbol));
  return lisp_to_scheme(symbol);
}

static int scheme_elisp_boundp(ptr symbol) {
  return !NILP(Fboundp(scheme_to_lisp(symbol)));
}

static int scheme_elisp_fboundp(ptr symbol) {
  return !NILP(Ffboundp(scheme_to_lisp(symbol)));
}

static ptr scheme_elisp_apply(ptr func, ptr args) {
  uptr nargs = 1;
  for (ptr link = args; link != Snil; link = Scdr(link)) {
    eassert(Spairp(link));
    nargs++;
  }
  Lisp_Object *lisp_args = alloca(sizeof(Lisp_Object) * nargs);
  printf("elisp-apply: converting function\n");
  lisp_args[0] = scheme_to_lisp(func);
  printf("elisp-apply: %d\n", (int)nargs);
  uptr i = 1;
  for (ptr link = args; link != Snil; link = Scdr(link)) {
    lisp_args[i] = scheme_to_lisp(Scar(link));
    i++;
  }
  return lisp_to_scheme(Ffuncall(nargs, lisp_args));
}

void scheme_init(void) {
  const char *char_ptr = NULL;
  const char **argv = &char_ptr;

  lisp_object_to_id_table =
    make_hash_table(hashtest_eq,
                    DEFAULT_HASH_SIZE,
                    DEFAULT_REHASH_SIZE,
                    DEFAULT_REHASH_THRESHOLD,
                    Qnil, false);
  lisp_id_to_object_table =
    make_hash_table(hashtest_eql,
                    DEFAULT_HASH_SIZE,
                    DEFAULT_REHASH_SIZE,
                    DEFAULT_REHASH_THRESHOLD,
                    Qnil, false);
  /* lisp_finalizer_table = */
  /*   make_hash_table(hashtest_eql, */
  /*                   DEFAULT_HASH_SIZE, */
  /*                   DEFAULT_REHASH_SIZE, */
  /*                   DEFAULT_REHASH_THRESHOLD, */
  /*                   Qnil, false); */
  
  printf("Starting scheme.\n");
  Sscheme_init(NULL);
  Sregister_boot_file("/usr/local/google/home/jrw/.local/lib/csv9.5.2/a6le/scheme.boot");
  printf("building heap\n");
  Sbuild_heap(NULL, NULL);
  Sforeign_symbol("scheme_elisp_boundp", scheme_elisp_boundp);
  Sforeign_symbol("scheme_elisp_fboundp", scheme_elisp_fboundp);
  Sforeign_symbol("scheme_elisp_intern", scheme_elisp_intern);
  Sforeign_symbol("scheme_elisp_apply", scheme_elisp_apply);
  Sscheme_script("/usr/local/google/home/jrw/git/schemacs/scheme/main.ss", 0, argv);
  Scall0(Stop_level_value(Sstring_to_symbol("emacs-init")));
  /* alloc_id = Sstring_to_symbol("alloc-id"); */
  /* release_id = Sstring_to_symbol("release-id"); */
  /* id_to_obj = Sstring_to_symbol("id->obj"); */

  /* TODO */
  DEFSYM(Qscheme_value_ref_p, "scheme-value-ref-p");
  /* TODO */
  DEFSYM(Qensure_scheme_value_ref, "ensure-scheme-value-ref");
  defsubr(&Sscheme_funcall);
  defsubr(&Sscheme_toplevel_value);

  atexit(Sscheme_deinit);
}

#endif
