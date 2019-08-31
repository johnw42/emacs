#include <config.h>

#ifdef HAVE_CHEZ_SCHEME

#include "lisp.h"
#include "buffer.h"
#include "window.h"
#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>

#pragma GCC diagnostic ignored "-Wdiscarded-qualifiers"

static bool scheme_initialized = false;
static ptr c_data_table;

ptr chez_vectorlike_symbol = chez_false;
ptr scheme_misc_symbol = chez_false;
ptr scheme_string_symbol = chez_false;
iptr scheme_greatest_fixnum;
iptr scheme_least_fixnum;
iptr chez_fixnum_width;
const char *last_scheme_function;
const char *last_scheme_call_file;
int last_scheme_call_line;

unsigned gdb_flags = 0;

SCHEME_FPTR_DEF(save_pointer, int, void *, const char *);
SCHEME_FPTR_DEF(check_pointer, int, void *, const char *);
SCHEME_FPTR_DEF(hashtablep, int, ptr);
SCHEME_FPTR_DEF(print_to_bytevector, ptr, ptr);
SCHEME_FPTR_DEF(save_origin, void, ptr);
SCHEME_FPTR_DEF(print_origin, void, ptr);
SCHEME_FPTR_DEF(eq_hash, uint32_t, ptr);
SCHEME_FPTR_DEF(hashtable_values, ptr, ptr);
SCHEME_FPTR_DEF(hashtable_ref, ptr, ptr, ptr, ptr);
SCHEME_FPTR_DEF(symbol_is, int, ptr, const char *);

static ptr lisp_to_scheme(Lisp_Object lisp_obj) {
  return lisp_obj;
}

static Lisp_Object scheme_to_lisp(ptr scheme_obj) {
  return scheme_obj;
}

DEFUN ("scheme-funcall", Fscheme_funcall, Sscheme_funcall, 1, MANY, 0,
       doc: /* TODO
usage: (scheme-funcall TODO) */)
  (ptrdiff_t nargs, Lisp_Object *args)
{
  ptr *scheme_args = alloca(nargs * sizeof(ptr));
  for (iptr i = 0; i < nargs; i++) {
    scheme_args[i] = lisp_to_scheme(args[i]);
  }

  if (chez_symbolp(scheme_args[0])) {
    scheme_args[0] = chez_top_level_value(scheme_args[0]);
  }
  eassert(chez_procedurep(scheme_args[0]));

  chez_initframe(nargs);
  for (iptr i = 1; i < nargs; i++) {
    chez_put_arg(i, scheme_args[i - 1]);
  }
  ptr scheme_result = chez_call(scheme_args[0], nargs - 1);
  return scheme_to_lisp(scheme_result);
}

DEFUN ("scheme-top-level-value", Fscheme_top_level_value, Sscheme_top_level_value, 1, 1, 0,
       doc: /* TODO */)
  (Lisp_Object symbol)
{
  CHECK_SYMBOL(symbol);
  ptr scheme_symbol = lisp_to_scheme(symbol);
  eassert(chez_symbolp(scheme_symbol));
  return scheme_to_lisp(chez_top_level_value(scheme_symbol));
}

DEFUN ("forget-scheme-object", Fforget_scheme_object, Sforget_scheme_object, 1, 1, 0,
       doc: /* TODO */)
  (Lisp_Object id)
{
  CHECK_NUMBER(id);
  if (scheme_initialized) {
    chez_call1
      (chez_top_level_value
       (chez_string_to_symbol ("forget-scheme-object")),
       chez_integer (XINT (id)));
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
  for (ptr link = args; link != chez_nil; link = chez_cdr(link)) {
    eassert(chez_pairp(link));
    nargs++;
  }
  Lisp_Object *lisp_args = alloca(sizeof(Lisp_Object) * nargs);
  lisp_args[0] = scheme_to_lisp(func);
  uptr i = 1;
  for (ptr link = args; link != chez_nil; link = chez_cdr(link)) {
    lisp_args[i] = scheme_to_lisp(chez_car(link));
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
  chez_lock_object(*c_ptr);
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
      chez_unlock_object(*locked_scheme_objs[i].c_ptr);
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

  /* // Collect and unlock all tracked Scheme objs. */
  /* ptr vec = scheme_make_vector(num_locked_scheme_objs, chez_false); */
  /* chez_lock_object(vec); */
  /* for (iptr i = 0; i < num_locked_scheme_objs; i++) { */
  /*   ptr scheme_obj = *locked_scheme_objs[i].c_ptr; */
  /*   locked_scheme_objs[i].scheme_obj = scheme_obj; */
  /*   chez_unlock_object(scheme_obj); */
  /*   chez_vector_set(vec, i, scheme_obj); */
  /* } */

  /* // Run garbage collection, which may move the tracked objs. */
  /* Scall0(Stop_level_value(Sstring_to_symbol("collect"))); */

  /* // Update references in C data structures to use the new locations, */
  /* // and lock the objs in place until the call to the function. */
  /* for (iptr i = 0; i < num_locked_scheme_objs; i++) { */
  /*   ptr scheme_obj = chez_vector_ref(vec, i); */
  /*   chez_lock_object(scheme_obj); */
  /*   if (scheme_obj != locked_scheme_objs[i].scheme_obj) { */
  /*     *locked_scheme_objs[i].c_ptr = scheme_obj; */
  /*     locked_scheme_objs[i].scheme_obj = scheme_obj; */
  /*   } */
  /* } */
  /* chez_unlock_object(vec); */
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
    chez_scheme_deinit();
    if (!NILP(Ffboundp(Qscheme_internal_reset))) {
      CALLN(Ffuncall, Qscheme_internal_reset);
    }
    scheme_initialized = false;
  }
}

static bool
call_guardian(ptr guardian)
{
  ptr rep = chez_call0(guardian);
  if (rep != chez_false)
    {
      scheme_call1("write", rep);
      scheme_call0("newline");
    }
  return rep != chez_false;
}

static
void alloc_test(void)
{
  ptr x = chez_cons(chez_false, chez_false);
  printf("&x = %p\n", (void *)&x);
  //chez_lock_object(x);
  ptr g = chez_call0("make-guardian");
  chez_lock_object(g);
  chez_call2(g, x, chez_fixnum(42));
  chez_call2(g, chez_cons(chez_false, chez_false), chez_fixnum(13));
  for (int i = 0; call_guardian(g) && i < 1000; i++)
    {
      chez_make_vector(1000*1000, chez_false);
    }
  /* chez_call0("collect-rendezvous"); */
  /* chez_call0("collect"); */
  while (call_guardian(g)) continue;
  printf("&x = %p\n", (void *)&x);
}

static void *
get_scheme_func(const char *name)
{
  ptr sym = chez_string_to_symbol (name);
  eassert (chez_symbolp (sym));
  ptr code = chez_top_level_value (sym);
  eassert ((uptr) code >= 0x1000);
  chez_lock_object (code);
  return chez_foreign_callable_entry_point (code);
}

void scheme_init(void) {
  const char *char_ptr = NULL;
  const char **argv = &char_ptr;

  eassert(!scheme_initialized);

  chez_scheme_init(NULL);
  chez_register_boot_file("/usr/local/google/home/jrw/.local/lib/csv9.5.2/a6le/scheme.boot");
  chez_build_heap(NULL, NULL);

  scheme_greatest_fixnum = chez_fixnum_value(scheme_call0("greatest-fixnum"));
  scheme_least_fixnum = chez_fixnum_value(scheme_call0("least-fixnum"));
  chez_fixnum_width = chez_fixnum_value(scheme_call0("fixnum-width"));

  chez_foreign_symbol("abort", abort);
  chez_foreign_symbol("Fequal", Fequal);
  chez_foreign_symbol("Fsxhash_equal", Fsxhash_equal);
  chez_foreign_symbol("scheme_elisp_boundp", scheme_elisp_boundp);
  chez_foreign_symbol("scheme_elisp_fboundp", scheme_elisp_fboundp);
  chez_foreign_symbol("scheme_elisp_call0", scheme_elisp_call0);
  chez_foreign_symbol("scheme_elisp_call1", scheme_elisp_call1);
  chez_foreign_symbol("scheme_elisp_apply", scheme_elisp_apply);
  chez_foreign_symbol("alloc_test", alloc_test);
  chez_scheme_script("/usr/local/google/home/jrw/git/schemacs/scheme/main.ss", 0, argv);
  scheme_call0("emacs-init");

  SCHEME_FPTR_INIT(save_pointer);
  SCHEME_FPTR_INIT(check_pointer);
  SCHEME_FPTR_INIT(hashtablep);
  SCHEME_FPTR_INIT(print_to_bytevector);
  SCHEME_FPTR_INIT(save_origin);
  SCHEME_FPTR_INIT(print_origin);
  SCHEME_FPTR_INIT(eq_hash);
  SCHEME_FPTR_INIT(hashtable_values);
  SCHEME_FPTR_INIT(hashtable_ref);
  SCHEME_FPTR_INIT(symbol_is);

  c_data_table = scheme_call0("make-eq-hashtable");
  chez_lock_object(c_data_table);

  chez_vectorlike_symbol = scheme_call1("gensym", chez_string("emacs-vectorlike"));
  chez_lock_object(chez_vectorlike_symbol);
  scheme_misc_symbol = scheme_call1("gensym", chez_string("emacs-misc"));
  chez_lock_object(scheme_misc_symbol);
  scheme_string_symbol = scheme_call1("gensym", chez_string("emacs-string"));
  chez_lock_object(scheme_string_symbol);

  //atexit(scheme_deinit);
  scheme_initialized = true;
}

static void
print_origin(ptr obj)
{
  SCHEME_FPTR_CALL(print_origin, obj);
}

// Converts a symbol or Scheme string to a Lisp string.
ptr
to_lisp_string(ptr arg)
{
  if (STRINGP (arg))
    return arg;
  if (chez_symbolp (arg))
    arg = scheme_call1 ("symbol->string", arg);
  eassert (chez_stringp (arg));
  iptr n = chez_string_length(arg);
  for (iptr j = 0; j < n; j++)
    if (chez_string_ref (arg, j) >= 0x80)
      {
        // Slow path.
        Lisp_Object lstr = Fmake_string (chez_fixnum (n), chez_fixnum (0));
        for (int i = 0; i < n; i++)
          Faset (lstr, chez_fixnum (i), chez_fixnum (chez_string_ref (arg, i)));
        return lstr;
      }

  // Fast path.
  char *buf = alloca(n);
  for (iptr i = 0; i < n; i++)
    {
      buf[i] = chez_string_ref (arg, i);
    }
  return make_unibyte_string (buf, n);
}

// Converts a symbol or Lisp string to a Scheme string.
ptr
to_scheme_string(ptr arg)
{
  if (chez_stringp (arg))
    return arg;
  if (chez_symbolp (arg))
      return scheme_call1 ("symbol->string", arg);
  eassert (STRINGP (arg));
  iptr n = XINT (Flength (arg));
  ptr sstr = chez_make_uninitialized_string(n);
  chez_lock_object(sstr);
  for (iptr i = 0; i < n; i++)
    chez_string_set (sstr, i, XINT (Faref (arg, make_number(i))));
  return sstr;
}

chez_ptr
make_scheme_string (const char *data, iptr nchars, iptr nbytes, bool multibyte)
{
  if (nchars == nbytes && !multibyte)
    return chez_string_of_length (data, nchars);
  else
    return to_scheme_string
      (make_specified_string (data, nchars, nbytes, multibyte));
}

static struct Lisp_Symbol *
ensure_symbol_c_data (Lisp_Object symbol, Lisp_Object name)
{
  eassert (chez_symbolp (symbol));

  struct Lisp_Symbol *p = scheme_find_c_data (symbol);
  if (p)
    {
      eassert (p->u.s.scheme_obj == symbol);
      return p;
    }

  if (name == chez_false)
    name = to_lisp_string (symbol);
  eassert (STRINGP (name));

  p = scheme_alloc_c_data(symbol, sizeof (struct Lisp_Symbol *));
  // Can't use init_nil_refs here because of how builtin symbols are
  // initialized.
  p->u.s.scheme_obj = symbol;
  p->u.s.name = name;
  p->u.s.plist = Qnil;
  p->u.s.redirect = SYMBOL_PLAINVAL;
  SET_SYMBOL_VAL (p, Qunbound);
  p->u.s.function = Qnil;
  p->u.s.gcmarkbit = false;
  p->u.s.interned = SYMBOL_UNINTERNED;
  p->u.s.trapped_write = SYMBOL_UNTRAPPED_WRITE;
  p->u.s.declared_special = false;
  p->u.s.pinned = false;
  return p;
}

struct Lisp_Symbol *
scheme_make_symbol(ptr name, enum symbol_interned interned)
{
  ptr scheme_symbol = chez_false;
  if (chez_symbolp (name))
      scheme_symbol = name;
  else
    {
      chez_ptr scheme_str = to_scheme_string (name);
      scheme_symbol = scheme_call1
        (interned == SYMBOL_INTERNED_IN_INITIAL_OBARRAY ?
         "string->symbol" : "gensym",
         scheme_str);
      chez_lock_object (scheme_symbol);
    }

  eassert (chez_symbolp (scheme_symbol));

  Lisp_Object lisp_str = to_lisp_string (name);
  struct Lisp_Symbol *xs = ensure_symbol_c_data (scheme_symbol, lisp_str);
  xs->u.s.interned = interned;
  return xs;
}

struct Lisp_Symbol *
XSYMBOL (Lisp_Object a)
{
  return ensure_symbol_c_data (a, chez_false);
}

void *
scheme_alloc_c_data (ptr key, iptr size)
{
  ptr bytes = scheme_malloc(size);
  scheme_call3("hashtable-set!", c_data_table, key, bytes);
  return scheme_malloc_ptr (bytes);
}

void *
scheme_find_c_data (ptr key)
{
  ptr found = SCHEME_FPTR_CALL(hashtable_ref, c_data_table, key, chez_false);
  if (found == chez_false)
    return NULL;
  return scheme_malloc_ptr (found);
}

void
scheme_ptr_fill (ptr *p, ptr init, iptr num_words)
{
  eassert (num_words >= 0);
  eassert (chez_symbolp (Qnil));
  for (iptr i = 0; i < num_words; i++) {
    p[i] = init;
  }
}

enum Lisp_Type
XTYPE (Lisp_Object a)
{
  if (lisp_h_SYMBOLP(a)) {
    return Lisp_Symbol;
  } else if (lisp_h_INTEGERP(a)) {
    return Lisp_Int0;
  } else if (lisp_h_CONSP(a)) {
    return Lisp_Cons;
  } else if (chez_vectorp(a)) {
    if (STRINGP(a)) {
      return Lisp_String;
    } else if (lisp_h_MISCP(a)) {
      return Lisp_Misc;
    } else if (lisp_h_VECTORLIKEP(a)) {
      return Lisp_Vectorlike;
    }
  } else if (lisp_h_FLOATP(a)) {
    return Lisp_Float;
  }
  return Lisp_Chez_Internal;
}

void
fixup_lispsym_init(ptr *p)
{
  // Reverse the transformation applied by LISPSYM_INITIALLY.
  uptr index = ((uptr) *p >> 8) & 0xffff;
  eassert (0 <= index && index < ARRAYELTS(lispsym));
  ptr sym = lispsym[index];
  eassert (SYMBOLP (sym));
  *p = sym;
}

static const char *scheme_classify(ptr x)
{
  switch (XTYPE(x)) {
  case Lisp_Symbol: return "Lisp_Symbol";
  case Lisp_Misc:
    switch (XMISCTYPE(x)) {
    case Lisp_Misc_Free: return "Lisp_Misc_Free";
    case Lisp_Misc_Marker: return "Lisp_Misc_Marker";
    case Lisp_Misc_Overlay: return "Lisp_Misc_Overlay";
    case Lisp_Misc_Save_Value: return "Lisp_Misc_Save_Value";
    case Lisp_Misc_Finalizer: return "Lisp_Misc_Finalizer";
    case Lisp_Misc_Limit: return "Lisp_Misc_Limit";
    default:
      return "Lisp_Misc";
    }
  case Lisp_Int0: return "Lisp_Int0";
  case Lisp_Int1: return "Lisp_Int1";
  case Lisp_String: return "Lisp_String";
  case Lisp_Vectorlike:
    switch (PSEUDOVECTOR_TYPE(XVECTOR(x))) {
    case PVEC_NORMAL_VECTOR: return "PVEC_NORMAL_VECTOR";
    case PVEC_PROCESS: return "PVEC_PROCESS";
    case PVEC_FRAME: return "PVEC_FRAME";
    case PVEC_WINDOW: return "PVEC_WINDOW";
    case PVEC_BOOL_VECTOR: return "PVEC_BOOL_VECTOR";
    case PVEC_BUFFER: return "PVEC_BUFFER";
    case PVEC_HASH_TABLE: return "PVEC_HASH_TABLE";
    case PVEC_TERMINAL: return "PVEC_TERMINAL";
    case PVEC_WINDOW_CONFIGURATION: return "PVEC_WINDOW_CONFIGURATION";
    case PVEC_SUBR: return "PVEC_SUBR";
    case PVEC_OTHER: return "PVEC_OTHER";
    case PVEC_XWIDGET: return "PVEC_XWIDGET";
    case PVEC_XWIDGET_VIEW: return "PVEC_XWIDGET_VIEW";
    case PVEC_THREAD: return "PVEC_THREAD";
    case PVEC_MUTEX: return "PVEC_MUTEX";
    case PVEC_CONDVAR: return "PVEC_CONDVAR";
    case PVEC_MODULE_FUNCTION: return "PVEC_MODULE_FUNCTION";
    case PVEC_COMPILED: return "PVEC_COMPILED";
    case PVEC_CHAR_TABLE: return "PVEC_CHAR_TABLE";
    case PVEC_SUB_CHAR_TABLE: return "PVEC_SUB_CHAR_TABLE";
    case PVEC_RECORD: return "PVEC_RECORD";
    case PVEC_FONT: return "PVEC_FONT";
    default:
      return "Lisp_Vectorlike";
    }
  case Lisp_Cons: return "Lisp_Cons";
  case Lisp_Float: return "Lisp_Float";
  default:
    if (chez_fixnump(x)) return "fixnum?";
    if (chez_charp(x)) return "char?";
    if (chez_nullp(x)) return "null?";
    if (chez_eof_objectp(x)) return "eof_object?";
    if (chez_bwp_objectp(x)) return "bwp_object?";
    if (chez_booleanp(x)) return "boolean?";
    if (chez_pairp(x)) return "pair?";
    if (chez_symbolp(x)) return "symbol?";
    if (chez_procedurep(x)) return "procedure?";
    if (chez_flonump(x)) return "flonum?";
    if (chez_vectorp(x)) return "vector?";
    if (chez_fxvectorp(x)) return "fxvector?";
    if (chez_bytevectorp(x)) return "bytevector?";
    if (chez_stringp(x)) return "string?";
    if (chez_bignump(x)) return "bignum?";
    if (chez_boxp(x)) return "box?";
    if (chez_inexactnump(x)) return "inexactnum?";
    if (chez_exactnump(x)) return "exactnum?";
    if (chez_ratnump(x)) return "ratnum?";
    if (chez_inputportp(x)) return "inputport?";
    if (chez_outputportp(x)) return "outputport?";
    if (chez_recordp(x)) return "record?";
    return NULL;
  }
}

/* static bool chez_fixnump(ptr x) { return chez_fixnump(x); } */
/* static bool scheme_charp(ptr x) { return Scharp(x); } */
/* static bool scheme_nullp(ptr x) { return Snullp(x); } */
/* static bool scheme_eof_objectp(ptr x) { return Seof_objectp(x); } */
/* static bool scheme_bwp_objectp(ptr x) { return Sbwp_objectp(x); } */
/* static bool scheme_booleanp(ptr x) { return Sbooleanp(x); } */
/* static bool chez_pairp(ptr x) { return chez_pairp(x); } */
/* static bool chez_symbolp(ptr x) { return chez_symbolp(x); } */
/* static bool chez_procedurep(ptr x) { return chez_procedurep(x); } */
/* static bool chez_flonump(ptr x) { return chez_flonump(x); } */
/* static bool chez_vectorp(ptr x) { return chez_vectorp(x); } */
/* static bool scheme_fxvectorp(ptr x) { return Sfxvectorp(x); } */
/* static bool chez_bytevectorp(ptr x) { return chez_bytevectorp(x); } */
/* static bool chez_stringp(ptr x) { return chez_stringp(x); } */
/* static bool scheme_bignump(ptr x) { return Sbignump(x); } */
/* static bool scheme_boxp(ptr x) { return Sboxp(x); } */
/* static bool scheme_inexactnump(ptr x) { return Sinexactnump(x); } */
/* static bool scheme_exactnump(ptr x) { return Sexactnump(x); } */
/* static bool scheme_ratnump(ptr x) { return Sratnump(x); } */
/* static bool scheme_inputportp(ptr x) { return Sinputportp(x); } */
/* static bool scheme_outputportp(ptr x) { return Soutputportp(x); } */
/* static bool scheme_recordp(ptr x) { return Srecordp(x); } */

const char *
gdb_print_scheme(Lisp_Object obj)
{
  static char buffer[4096];

  static ptr (*print_fun)(ptr);

  ptr bvec = SCHEME_FPTR_CALL(print_to_bytevector, obj);
  chez_lock_object (bvec);
  eassert (chez_bytevectorp (bvec));
  iptr n = chez_bytevector_length(bvec);
  memcpy(buffer, chez_bytevector_data(bvec), n);
  chez_unlock_object(bvec);
  return buffer;
}

const char *
gdb_print(Lisp_Object obj)
{
  static char buffer[4096];

  memset(buffer, 0, sizeof(buffer));

  if (XTYPE (obj) == Lisp_Chez_Internal)
    return gdb_print_scheme(obj);

  Lisp_Object str = Fprin1_to_string (obj, Qnil);
  iptr n = SCHARS(str);
  if (n > sizeof(buffer) - 5)
    n = sizeof(buffer) - 5;
  for (iptr i = 0; i < n; i++) {
    EMACS_INT c = XINT (Faref(str, make_number(i)));
    buffer[i] = 0 < c && c < 255 ? c : 255;
  }

  return buffer;
}

static const char *last_func_name = NULL;

extern ptr
scheme_function_for_name(const char *name) {
  last_func_name = name;
  ptr sym = chez_string_to_symbol(name);
  eassert(chez_symbolp(sym));
  ptr fun = chez_top_level_value(sym);
  chez_lock_object (fun);
  //eassert(chez_procedurep(fun));
  return fun;
}

// NOT TESTED!
/* Finds all Lisp_Object references in OBJ not managed by the scheme
   garbage collector.  Calls FUN zero or more times, passing DATA
   and a pair of pointers indicating the stard and end of a block of
   Lisp_Object values. */
void
visit_lisp_refs(Lisp_Object obj,
                void (*fun)(void *, Lisp_Object *, ptrdiff_t),
                void *data)
{
  switch (XTYPE(obj))
    {
    case Lisp_Symbol:
      {
        struct Lisp_Symbol *s = XSYMBOL(obj);
        fun(data, &s->u.s.name, 1);
        if (s->u.s.redirect == 0)
          fun(data, &s->u.s.val.value, 1);
        fun(data, &s->u.s.function, 1);
        fun(data, &s->u.s.plist, 1);
        // TODO(jrw): Follow val.blv and val.fwd?
        break;
      }
    case Lisp_Misc:
      {
        union Lisp_Misc *m = XMISC(obj);
        switch (XMISCTYPE(obj))
          {
          case Lisp_Misc_Free:
          case Lisp_Misc_Marker:
#ifdef HAVE_MODULES
          case Lisp_Misc_User_Ptr:
#endif
          case Lisp_Misc_Limit:
            break;
          case Lisp_Misc_Overlay:
            fun(data, &m->u_overlay.start, 1);
            fun(data, &m->u_overlay.end, 1);
            fun(data, &m->u_overlay.plist, 1);
            break;
          case Lisp_Misc_Save_Value:
            switch (m->u_save_value.save_type)
              {
              case SAVE_TYPE_MEMORY:
                // This case can't be implemented without valid_lisp_object_p.
                eassert(false);
                break;
              default:
                for (int index = 0; index < SAVE_VALUE_SLOTS; index++)
                  switch (save_type (&m->u_save_value, index))
                    {
                    case SAVE_OBJECT:
                      fun(data, &m->u_save_value.data[index].object, 1);
                      break;
                    default:
                      break;
                    }
              }
            break;
          case Lisp_Misc_Finalizer:
            fun(data, &m->u_finalizer.function, 1);
            break;
          }
        break;
      }
    case Lisp_Vectorlike:
      if (VECTORP(obj))
        {
          EMACS_INT n = ASIZE(obj);
          if (n > 0)
            fun(data, aref_addr(obj, 0), n);
        }
      else
        {
          EMACS_INT n = PVSIZE(obj);
          if (n > 0)
            fun(data, aref_addr(obj, 0), n);
          switch (PSEUDOVECTOR_TYPE(XVECTOR(obj)))
            {
            case PVEC_NORMAL_VECTOR:
            case PVEC_PROCESS:
            case PVEC_FRAME:
            case PVEC_BOOL_VECTOR:
            case PVEC_TERMINAL:
            case PVEC_WINDOW_CONFIGURATION:
            case PVEC_SUBR:
            case PVEC_OTHER:
            case PVEC_XWIDGET:
            case PVEC_XWIDGET_VIEW:
            case PVEC_MUTEX:
            case PVEC_CONDVAR:
            case PVEC_MODULE_FUNCTION:
            case PVEC_COMPILED:
            case PVEC_RECORD:
            case PVEC_FONT:
            case PVEC_FREE:
              break;
            case PVEC_WINDOW:
              {
                struct window *w = XWINDOW(obj);
                fun(data, &w->prev_buffers, 1);
                fun(data, &w->next_buffers, 1);
                break;
              }
            case PVEC_BUFFER:
              {
                struct buffer *b = XBUFFER(obj);
                fun(data, &b->undo_list_, 1);
                break;
              }
            case PVEC_HASH_TABLE:
              {
                struct Lisp_Hash_Table *t = XHASH_TABLE(obj);
                fun(data, &t->key_and_value, 1);
                fun(data, &t->test.name, 1);
                fun(data, &t->test.user_hash_function, 1);
                fun(data, &t->test.user_cmp_function, 1);
                break;
              }
            case PVEC_CHAR_TABLE:
            case PVEC_SUB_CHAR_TABLE:
              // TODO(jrw);
              break;
            case PVEC_THREAD:
              {
                struct thread_state *t = XTHREAD(obj);
                fun(data, &t->m_re_match_object, 1);
                break;
              }
            }
        }
    default:
      return;
    }
}

static void
init_nil_ref_block (void *data, Lisp_Object *ptrs, ptrdiff_t n)
{
  for (ptrdiff_t i = 0; i < n; i++)
    {
      /* eassert (ptrs[i] == Qnil); */
      eassert (ptrs[i] == Qnil || (uptr) ptrs[i] == 0);
      ptrs[i] = Qnil;
    }
}

void
init_nil_refs (Lisp_Object obj)
{
  eassert (chez_symbolp (Qnil));
  visit_lisp_refs(obj, init_nil_ref_block, NULL);
}


bool
symbol_is(ptr sym, const char *name)
{
  if (chez_symbolp (sym))
    return SCHEME_FPTR_CALL(symbol_is, sym, name);
  if (STRINGP (sym))
    return strncmp(SSDATA(sym), name, SCHARS(sym)) == 0;
  return false;
}

bool
datum_starts_with(ptr obj, const char *str)
{
  /* static const char *prev_str = NULL; */
  /* static size_t prev_length = 0; */

  /* if (str != prev_str) */
  /*   { */
  /*     prev_str = str; */
  /*     prev_length = strlen(str); */
  /*   } */
  return strncmp(gdb_print(obj), str, strlen(str)) == 0;
}


#endif /* HAVE_CHEZ_SCHEME */
