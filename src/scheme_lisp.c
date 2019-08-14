#include <config.h>

#ifdef HAVE_CHEZ_SCHEME

#include "lisp.h"
#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>

#pragma GCC diagnostic ignored "-Wdiscarded-qualifiers"

#define SHIFT_BITS (CHAR_BIT * sizeof(EMACS_INT) / 2)

static bool scheme_initialized = false;

/* static EMACS_INT next_lisp_object_id = 0; */
/* static Lisp_Object lisp_object_to_id_table; */
/* static Lisp_Object lisp_id_to_object_table; */

static ptr lisp_to_scheme(Lisp_Object lisp_obj) {
  /* CALLN (Fmessage, build_string("lisp_to_scheme: %S"), lisp_obj); */
  //printf("lisp_to_scheme: %"PRIdPTR"\n", (intptr_t)XLI(lisp_obj));  
  
  if (SYMBOLP(lisp_obj)) {
    Lisp_Object name = SYMBOL_NAME(lisp_obj);
    ptrdiff_t len = SBYTES(name);
    char *buffer = alloca(1 + len);
    memcpy(buffer, SSDATA(name), len);
    buffer[len] = '\0';
    /* printf("converting symbol: %s\n", buffer); */
    ptr p = Sstring_to_symbol(buffer);
    /* printf("done\n"); */
    return p;
  }

  // If lisp_obj is a scheme value ref, extract the ID number and map
  // it back to a real Scheme value.
  Lisp_Object ref_id = call1(Qscheme_value_ref_id, lisp_obj);
  if (!NILP(ref_id)) {
    CHECK_NUMBER(ref_id);
    return Scall1(Stop_level_value(Sstring_to_symbol("scheme-object-for-id")),
                  Sunsigned(XINT(ref_id)));
  }
    
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
  /* EMACS_INT obj_id; */
  /* Lisp_Object lisp_id = Fgethash(lisp_obj, lisp_object_to_id_table, Qnil); */
  /* if (NILP(lisp_id)) { */
  /*   obj_id = next_lisp_object_id++; */
  /*   printf("assigning ID: %d %d\n", (int)obj_id, (int)NILP(lisp_obj)); */
  /*   Lisp_Object lisp_id = make_number(obj_id); */
  /*   Fputhash(lisp_obj, lisp_id, lisp_object_to_id_table); */
  /*   Fputhash(lisp_id, lisp_id, lisp_id_to_object_table); */
  /* } else { */
  /*   eassert(INTEGERP(lisp_id)); */
  /*   obj_id = XINT(lisp_id); */
  /* } */
  Lisp_Object obj_id = call1(Qensure_lisp_object_id, lisp_obj);
  EMACS_UINT lisp_obj_uint = (EMACS_UINT) XLI(lisp_obj);
  uptr hi = lisp_obj_uint >> SHIFT_BITS;
  uptr lo = (lisp_obj_uint << SHIFT_BITS) >> SHIFT_BITS;
  return Scall3(Stop_level_value(Sstring_to_symbol("ensure-lisp-object-ref")),
                Sfixnum((iptr)hi),
                Sfixnum((iptr)lo),
                Sinteger(XINT(obj_id)));
}

static Lisp_Object scheme_to_lisp(ptr scheme_obj) {
  /* printf("scheme_to_lisp\n"); */

  ptr obj_value;
  Lisp_Object lisp_obj = Qnil;
  
  if (Ssymbolp(scheme_obj)) {
    lisp_obj = Fintern(scheme_to_lisp(Ssymbol_to_string(scheme_obj)), Qnil);
    goto end;
  }
  if (Sstringp(scheme_obj)) {
    iptr len = Sstring_length(scheme_obj);
    char *buffer = alloca(len);
    for (iptr i = 0; i < len; i++) {
      buffer[i] = Sstring_ref(scheme_obj, i);
    }
    lisp_obj = make_string(buffer, len);
    goto end;
  }
  if (Sfixnump(scheme_obj) || Sbignump(scheme_obj)) {
    lisp_obj = make_number(Sinteger_value(scheme_obj));
    goto end;
  }
  if (Sflonump(scheme_obj)) {
    lisp_obj = make_float(Sflonum_value(scheme_obj));
    goto end;
  }

  // If scheme_obj is a lisp-object-ref, extract its value and convert
  // to to a real Lisp_Object reference.
  /* printf("calling extract-lisp-object from C\n"); */
  /* eassert(Ssymbolp(Ssymbol_to_string("extract-lisp-object"))); */
  /* eassert(Sprocedurep(Stop_level_value(Ssymbol_to_string("extract-lisp-object")))); */
  obj_value = Scall1(Stop_level_value(Sstring_to_symbol("extract-lisp-object")),
                     scheme_obj);
  if (obj_value != Sfalse) {
    /* printf("found object\n"); */
    eassert(Spairp(obj_value));
    ptr hi_obj = Scar(obj_value);
    ptr lo_obj = Scdr(obj_value);
    eassert(Sfixnump(hi_obj));
    eassert(Sfixnump(lo_obj));
    iptr hi = Sfixnum_value(hi_obj) << SHIFT_BITS;
    iptr lo = Sfixnum_value(lo_obj);
    eassert((hi & lo) == 0);
    lisp_obj = XIL(hi | lo);
    goto end;
  }

  /* printf("calling ensure-scheme-object from C\n"); */
  obj_value = Scall1(Stop_level_value(Sstring_to_symbol("ensure-scheme-object-id")),
                     scheme_obj);
  eassert(Sfixnump(obj_value));
  lisp_obj = call1(Qensure_scheme_value_ref, make_number(Sinteger_value(obj_value)));

 end:
  return lisp_obj;
}

DEFUN ("scheme-funcall", Fscheme_funcall, Sscheme_funcall, 1, MANY, 0,
       doc: /* TODO */)
  (ptrdiff_t nargs, Lisp_Object *args)
{
  /* printf("Fscheme_funcall\n"); */

  ptr *scheme_args = alloca(nargs * sizeof(ptr));
  for (iptr i = 0; i < nargs; i++) {
    scheme_args[i] = lisp_to_scheme(args[i]);
  }

  if (Ssymbolp(scheme_args[0])) {
    scheme_args[0] = Stop_level_value(scheme_args[0]);
  }
  eassert(Sprocedurep(scheme_args[0]));

  Sinitframe(nargs);
  for (iptr i = 1; i < nargs; i++) {
    Sput_arg(i, scheme_args[i - 1]);
  }
  ptr scheme_result = Scall(scheme_args[0], nargs - 1);
  /* printf("converting result\n"); */
  return scheme_to_lisp(scheme_result);
}

DEFUN ("scheme-top-level-value", Fscheme_top_level_value, Sscheme_top_level_value, 1, 1, 0,
       doc: /* TODO */)
  (Lisp_Object symbol)
{
  /* printf("Fscheme_top_level_value\n"); */
  CHECK_SYMBOL(symbol);
  ptr scheme_symbol = lisp_to_scheme(symbol);
  eassert(Ssymbolp(scheme_symbol));
  return scheme_to_lisp(Stop_level_value(scheme_symbol));
}

DEFUN ("forget-scheme-object", Fforget_scheme_object, Sforget_scheme_object, 1, 1, 0,
       doc: /* TODO */)
  (Lisp_Object id)
{
  /* printf("Fforget_scheme_object\n"); */
  CHECK_NUMBER(id);
  if (scheme_initialized) {
    Scall1(Stop_level_value(Sstring_to_symbol("forget-scheme-object")),
           Sinteger(XINT(id)));
  }
  return Qnil;
}

static int scheme_elisp_boundp(ptr symbol) {
  return !NILP(Fboundp(scheme_to_lisp(symbol)));
}

static int scheme_elisp_fboundp(ptr symbol) {
  return !NILP(Ffboundp(scheme_to_lisp(symbol)));
}

static ptr scheme_elisp_call0(ptr func) {
  return lisp_to_scheme(call0(scheme_to_lisp(func)));
}

static ptr scheme_elisp_call1(ptr func, ptr arg) {
  return lisp_to_scheme(call1(scheme_to_lisp(func),
                              scheme_to_lisp(arg)));
}

static ptr scheme_elisp_apply(ptr func, ptr args) {
  uptr nargs = 1;
  for (ptr link = args; link != Snil; link = Scdr(link)) {
    eassert(Spairp(link));
    nargs++;
  }
  Lisp_Object *lisp_args = alloca(sizeof(Lisp_Object) * nargs);
  /* printf("elisp-apply: converting function\n"); */
  lisp_args[0] = scheme_to_lisp(func);
  /* printf("elisp-apply: %d\n", (int)nargs); */
  uptr i = 1;
  for (ptr link = args; link != Snil; link = Scdr(link)) {
    lisp_args[i] = scheme_to_lisp(Scar(link));
    i++;
  }
  return lisp_to_scheme(Ffuncall(nargs, lisp_args));
}

void syms_of_scheme_lisp(void) {
  DEFSYM(Qscheme_value_ref_id, "scheme-value-ref-id");
  DEFSYM(Qensure_scheme_value_ref, "ensure-scheme-value-ref");
  DEFSYM(Qensure_lisp_object_id, "ensure-lisp-object-id");
  DEFSYM(Qscheme_internal_reset, "scheme-internal-reset");
  defsubr(&Sscheme_funcall);
  defsubr(&Sscheme_top_level_value);
  defsubr(&Sforget_scheme_object);
}

void scheme_deinit(void) {
  if (scheme_initialized) {
    Sscheme_deinit();
    if (!NILP(Ffboundp(Qscheme_internal_reset))) {
      CALLN(Ffuncall, Qscheme_internal_reset);
    }
    scheme_initialized = false;
  }
}

void scheme_init(void) {
  const char *char_ptr = NULL;
  const char **argv = &char_ptr;

  /* lisp_object_to_id_table = */
  /*   make_hash_table(hashtest_eq, */
  /*                   DEFAULT_HASH_SIZE, */
  /*                   DEFAULT_REHASH_SIZE, */
  /*                   DEFAULT_REHASH_THRESHOLD, */
  /*                   Qnil, false); */
  /* lisp_id_to_object_table = */
  /*   make_hash_table(hashtest_eql, */
  /*                   DEFAULT_HASH_SIZE, */
  /*                   DEFAULT_REHASH_SIZE, */
  /*                   DEFAULT_REHASH_THRESHOLD, */
  /*                   Qnil, false); */
  /* lisp_finalizer_table = */
  /*   make_hash_table(hashtest_eql, */
  /*                   DEFAULT_HASH_SIZE, */
  /*                   DEFAULT_REHASH_SIZE, */
  /*                   DEFAULT_REHASH_THRESHOLD, */
  /*                   Qnil, false); */

  /* printf("Starting scheme.\n"); */
  Sscheme_init(NULL);
  Sregister_boot_file("/usr/local/google/home/jrw/.local/lib/csv9.5.2/a6le/scheme.boot");
  /* printf("building heap\n"); */
  Sbuild_heap(NULL, NULL);
  Sforeign_symbol("scheme_elisp_boundp", scheme_elisp_boundp);
  Sforeign_symbol("scheme_elisp_fboundp", scheme_elisp_fboundp);
  Sforeign_symbol("scheme_elisp_call0", scheme_elisp_call0);
  Sforeign_symbol("scheme_elisp_call1", scheme_elisp_call1);
  Sforeign_symbol("scheme_elisp_apply", scheme_elisp_apply);
  Sscheme_script("/usr/local/google/home/jrw/git/schemacs/scheme/main.ss", 0, argv);
  Scall0(Stop_level_value(Sstring_to_symbol("emacs-init")));

  atexit(scheme_deinit);
  scheme_initialized = true;
}

#endif
