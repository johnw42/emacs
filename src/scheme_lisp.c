#include <config.h>

#ifdef HAVE_CHEZ_SCHEME

#include "lisp.h"
#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>

#pragma GCC diagnostic ignored "-Wdiscarded-qualifiers"

#undef Smake_vector

static bool scheme_initialized = false;
static ptr c_data_table;

ptr scheme_pseudovector_tag = 0;

static ptr lisp_to_scheme(Lisp_Object lisp_obj) {
  return lisp_obj;
}

static Lisp_Object scheme_to_lisp(ptr scheme_obj) {
  return scheme_obj;
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
  scheme_call0("emacs-init");
  
  c_data_table = scheme_call0("make-eq-hashtable");
  Slock_object(c_data_table);

  scheme_pseudovector_tag = scheme_call0("gensym");
  Slock_object(scheme_pseudovector_tag);

  atexit(scheme_deinit);
  scheme_initialized = true;
}

ptr
scheme_malloc(ptrdiff_t size)
{
  eassert(size > 0);
  ptr bvec = Smake_bytevector(size, 0);
  Slock_object(bvec);
  return bvec;
}

ptr
scheme_intern(const char *str, iptr len, ptr obarray)
{
  ptr name = len < 0 ? Sstring(str) : Sstring_of_length(str, len);
  Slock_object(name);
  ptr sym = Fintern(name, obarray);
  Sunlock_object(name);
  return sym;
}

void *
scheme_alloc_c_data(ptr key, iptr size)
{
  ptr data_obj = scheme_malloc(size);
  scheme_call3("hashtable-set!", c_data_table, key, data_obj);
  return scheme_malloc_ptr(data_obj);
}

ptr
scheme_make_pvec(iptr size,
                 enum pvec_type type,
                 iptr bytes_count,
                 int bytes_fill)
{
  eassert(size >= 0);
  ptr vec = Smake_vector(size + MIN_PVEC_SIZE, Qnil);
  Slock_object(vec);
  ptr bytes = Smake_bytevector(bytes_found, bytes_fill);
  Slock_object(bytes);
  PVEC_FIELD_SET(vec, BYTES, bytes);
  PVEC_FIELD_SET(vec, TAG, scheme_pseudovector_tag);
  PVEC_FIELD_SET(vec, PVEC_TYPE, Sfixnum(type));
}

void
staticpro (Lisp_Object *varaddress)
{
}


#endif
