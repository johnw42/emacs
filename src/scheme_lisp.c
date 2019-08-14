#include <config.h>

#ifdef HAVE_CHEZ_SCHEME

#include "lisp.h"
#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>

#pragma GCC diagnostic ignored "-Wdiscarded-qualifiers"

#undef Smake_vector
#define SHIFT_BITS (CHAR_BIT * sizeof(EMACS_INT) / 2)

static bool scheme_initialized = false;

static ptr lisp_to_scheme(Lisp_Object lisp_obj) {
  if (SYMBOLP(lisp_obj)) {
    Lisp_Object name = SYMBOL_NAME(lisp_obj);
    ptrdiff_t len = SBYTES(name);
    char *buffer = alloca(1 + len);
    memcpy(buffer, SSDATA(name), len);
    buffer[len] = '\0';
    ptr p = Sstring_to_symbol(buffer);
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

  if (SCHEME_REFP(lisp_obj)) {
    return XSCHEME_REF(lisp_obj)->scheme_obj;
  }

  /* Make sure this Lisp object has an ID number and wrap it as a
     Scheme object. */
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
  obj_value = Scall1(Stop_level_value(Sstring_to_symbol("extract-lisp-object")),
                     scheme_obj);
  if (obj_value != Sfalse) {
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
  return scheme_to_lisp(scheme_result);
}

DEFUN ("scheme-top-level-value", Fscheme_top_level_value, Sscheme_top_level_value, 1, 1, 0,
       doc: /* TODO */)
  (Lisp_Object symbol)
{
  CHECK_SYMBOL(symbol);
  ptr scheme_symbol = lisp_to_scheme(symbol);
  eassert(Ssymbolp(scheme_symbol));
  return scheme_to_lisp(Stop_level_value(scheme_symbol));
}

DEFUN ("forget-scheme-object", Fforget_scheme_object, Sforget_scheme_object, 1, 1, 0,
       doc: /* TODO */)
  (Lisp_Object id)
{
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
  lisp_args[0] = scheme_to_lisp(func);
  uptr i = 1;
  for (ptr link = args; link != Snil; link = Scdr(link)) {
    lisp_args[i] = scheme_to_lisp(Scar(link));
    i++;
  }
  return lisp_to_scheme(Ffuncall(nargs, lisp_args));
}

struct locked_scheme_obj {
  ptr *c_ptr;

  // Copy of *c_ptr.  Valid only during scheme_gc().
  ptr scheme_obj;
};

static struct locked_scheme_obj *locked_scheme_objs = NULL;
static iptr locked_scheme_objs_size = 0;
static iptr num_locked_scheme_objs = 0;

void scheme_track_for_elisp(ptr *c_ptr) {
  Slock_object(*c_ptr);
  if (num_locked_scheme_objs == locked_scheme_objs_size) {
    iptr new_size =
      locked_scheme_objs_size == 0 ? 1 : 2 * locked_scheme_objs_size;
    struct locked_scheme_obj *new_array =
      reallocarray(locked_scheme_objs,
                   new_size, sizeof(struct locked_scheme_obj));
    eassert(new_array);
    locked_scheme_objs = new_array;
  }
  eassert(locked_scheme_objs_size > num_locked_scheme_objs);
  locked_scheme_objs[num_locked_scheme_objs].c_ptr = c_ptr;
  ++num_locked_scheme_objs;
}

void scheme_untrack_for_elisp(ptr *c_ptr) {
  for (iptr i = 0; i < num_locked_scheme_objs; i++) {
    if (locked_scheme_objs[i].c_ptr == c_ptr) {
      Sunlock_object(*locked_scheme_objs[i].c_ptr);
      --num_locked_scheme_objs;
      locked_scheme_objs[i].c_ptr =
        locked_scheme_objs[num_locked_scheme_objs].c_ptr;
      return;
    }
  }
  eassert(false);
}

void scheme_gc(void) {
  if (!scheme_initialized) {
    return;
  }

  // Collect and unlock all tracked Scheme objs.
  ptr vec = Smake_vector(num_locked_scheme_objs, Sfalse);
  Slock_object(vec);
  for (iptr i = 0; i < num_locked_scheme_objs; i++) {
    ptr scheme_obj = *locked_scheme_objs[i].c_ptr;
    locked_scheme_objs[i].scheme_obj = scheme_obj;
    Sunlock_object(scheme_obj);
    Svector_set(vec, i, scheme_obj);
  }

  // Run garbage collection, which may move the tracked objs.
  Scall0(Stop_level_value(Sstring_to_symbol("collect")));

  // Update references in C data structures to use the new locations,
  // and lock the objs in place until the call to the function.
  for (iptr i = 0; i < num_locked_scheme_objs; i++) {
    ptr scheme_obj = Svector_ref(vec, i);
    Slock_object(scheme_obj);
    if (scheme_obj != locked_scheme_objs[i].scheme_obj) {
      *locked_scheme_objs[i].c_ptr = scheme_obj;
      locked_scheme_objs[i].scheme_obj = scheme_obj;
    }
  }
  Sunlock_object(vec);
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

  eassert(!scheme_initialized);

  Sscheme_init(NULL);
  Sregister_boot_file("/usr/local/google/home/jrw/.local/lib/csv9.5.2/a6le/scheme.boot");
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
