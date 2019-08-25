#include <config.h>

#ifdef HAVE_CHEZ_SCHEME

#include "lisp.h"
#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>

#pragma GCC diagnostic ignored "-Wdiscarded-qualifiers"

static bool scheme_initialized = false;
static ptr c_data_table;

ptr scheme_vectorlike_symbol = Sfalse;
ptr scheme_misc_symbol = Sfalse;
ptr scheme_string_symbol = Sfalse;
bool (*scheme_save_ptr_fun)(void *, const char *);
bool (*scheme_check_ptr_fun)(void *, const char *);

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

  /* // Collect and unlock all tracked Scheme objs. */
  /* ptr vec = scheme_make_vector(num_locked_scheme_objs, Sfalse); */
  /* Slock_object(vec); */
  /* for (iptr i = 0; i < num_locked_scheme_objs; i++) { */
  /*   ptr scheme_obj = *locked_scheme_objs[i].c_ptr; */
  /*   locked_scheme_objs[i].scheme_obj = scheme_obj; */
  /*   Sunlock_object(scheme_obj); */
  /*   Svector_set(vec, i, scheme_obj); */
  /* } */

  /* // Run garbage collection, which may move the tracked objs. */
  /* Scall0(Stop_level_value(Sstring_to_symbol("collect"))); */

  /* // Update references in C data structures to use the new locations, */
  /* // and lock the objs in place until the call to the function. */
  /* for (iptr i = 0; i < num_locked_scheme_objs; i++) { */
  /*   ptr scheme_obj = Svector_ref(vec, i); */
  /*   Slock_object(scheme_obj); */
  /*   if (scheme_obj != locked_scheme_objs[i].scheme_obj) { */
  /*     *locked_scheme_objs[i].c_ptr = scheme_obj; */
  /*     locked_scheme_objs[i].scheme_obj = scheme_obj; */
  /*   } */
  /* } */
  /* Sunlock_object(vec); */
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

static bool
call_guardian(ptr guardian)
{
  ptr rep = Scall0(guardian);
  if (rep != Sfalse)
    {
      scheme_call1("write", rep);
      scheme_call0("newline");
    }
  return rep != Sfalse;
}

static
void alloc_test(void)
{
  ptr x = scheme_cons(Sfalse, Sfalse);
  printf("&x = %p\n", (void *)&x);
  //Slock_object(x);
  ptr g = scheme_call0("make-guardian");
  Slock_object(g);
  Scall2(g, x, Sfixnum(42));
  Scall2(g, scheme_cons(Sfalse, Sfalse), Sfixnum(13));
  for (int i = 0; call_guardian(g) && i < 1000; i++)
    {
      scheme_make_vector(1000*1000, Sfalse);
    }
  /* scheme_call0("collect-rendezvous"); */
  /* scheme_call0("collect"); */
  while (call_guardian(g)) continue;
  printf("&x = %p\n", (void *)&x);
}

static void *
get_scheme_func(const char *name)
{
  return Sforeign_callable_entry_point
    (Stop_level_value (Sstring_to_symbol (name)));
}

void scheme_init(void) {
  const char *char_ptr = NULL;
  const char **argv = &char_ptr;

  eassert(!scheme_initialized);

  Sscheme_init(NULL);
  Sregister_boot_file("/usr/local/google/home/jrw/.local/lib/csv9.5.2/a6le/scheme.boot");
  Sbuild_heap(NULL, NULL);

  Sforeign_symbol("abort", abort);
  Sforeign_symbol("scheme_elisp_boundp", scheme_elisp_boundp);
  Sforeign_symbol("scheme_elisp_fboundp", scheme_elisp_fboundp);
  Sforeign_symbol("scheme_elisp_call0", scheme_elisp_call0);
  Sforeign_symbol("scheme_elisp_call1", scheme_elisp_call1);
  Sforeign_symbol("scheme_elisp_apply", scheme_elisp_apply);
  Sforeign_symbol("alloc_test", alloc_test);
  Sscheme_script("/usr/local/google/home/jrw/git/schemacs/scheme/main.ss", 0, argv);
  scheme_call0("emacs-init");
  
  scheme_save_ptr_fun = get_scheme_func ("save-pointer");
  scheme_check_ptr_fun = get_scheme_func ("check-pointer");
  
  c_data_table = scheme_call0("make-eq-hashtable");
  Slock_object(c_data_table);

  scheme_vectorlike_symbol = scheme_call1("gensym", Sstring("emacs-vectorlike"));
  Slock_object(scheme_vectorlike_symbol);
  scheme_misc_symbol = scheme_call1("gensym", Sstring("emacs-misc"));
  Slock_object(scheme_misc_symbol);
  scheme_string_symbol = scheme_call1("gensym", Sstring("emacs-string"));
  Slock_object(scheme_string_symbol);

  //atexit(scheme_deinit);
  scheme_initialized = true;
}

ptr
scheme_make_lisp_string(ptr str)
{
  if (STRINGP (str))
    return str;
  if (Ssymbolp (str))
    str = scheme_call1 ("symbol->string", str);
  eassert (Sstringp (str));
  iptr n = Sstring_length(str);
  Lisp_Object lstr = Fmake_string (Sfixnum (n), Sfixnum (0));
  for (int i = 0; i < n; i++)
    Faset (lstr, Sfixnum (i), Sfixnum (Sstring_ref (str, i)));
  return lstr;
}

struct Lisp_Symbol *
scheme_make_symbol(ptr name, enum symbol_interned interned)
{
  ptr scheme_str;
  ptr scheme_symbol = Sfalse;
  Lisp_Object lisp_str = Sfalse;
  if (Ssymbolp(name))
    {
      scheme_str = scheme_call1("symbol->string", name);
      scheme_symbol = name;
    }
  else if (Sstringp(name))
    {
      scheme_str = name;
    }
  else
    {
      scheme_str = Sstring_of_length(SSDATA(name), SBYTES(name));
      lisp_str = name;
    }
    
  if (scheme_symbol == Sfalse)
      scheme_symbol = scheme_call1
        (interned == SYMBOL_UNINTERNED ? "gensym" : "string->symbol",
         scheme_str);
  if (lisp_str == Sfalse)
    lisp_str = scheme_make_lisp_string (scheme_str);
    
  eassert (Sstringp (scheme_str));
  eassert (Ssymbolp (scheme_symbol));

  return XSYMBOL(scheme_symbol);
  /* struct Lisp_Symbol *data = scheme_find_c_data (scheme_symbol); */
  /* if (data == NULL) */
  /*   data = SCHEME_ALLOC_C_DATA(scheme_symbol, struct Lisp_Symbol); */
  /* data->u.s.interned = interned; */
  /* data->u.s.redirect = SYMBOL_PLAINVAL; */
  /* data->u.s.trapped_write = SYMBOL_UNTRAPPED_WRITE; */
  /* data->u.s.plist = Qnil; */
  /* data->u.s.scheme_obj = scheme_symbol; */
  /* data->u.s.name = lisp_str; */
  /* return data; */
}

struct Lisp_Symbol *
XSYMBOL (Lisp_Object a)
{
  eassert (Ssymbolp (a));
  struct Lisp_Symbol *p = scheme_find_c_data (a);
  if (p)
    {
      eassert (p->u.s.scheme_obj == a);
      return p;
    }
  p = scheme_alloc_c_data(a, sizeof (struct Lisp_Symbol *));
  p->u.s.scheme_obj = a;
  p->u.s.name = scheme_make_lisp_string (a);
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

ptr
scheme_obarray_ensure(ptr obarray, ptr sym)
{
  struct Lisp_Symbol *data =
    scheme_make_symbol(sym,
                       (NILP(obarray) || obarray == Vobarray)
                       ? SYMBOL_INTERNED_IN_INITIAL_OBARRAY
                       : SYMBOL_INTERNED);
  ptr table = scheme_obarray_table(obarray);
  eassert (scheme_call1 ("hashtable?", table) == Strue);
  ptr scheme_symbol = data->u.s.scheme_obj;
  eassert (Ssymbolp (scheme_symbol));
  ptr scheme_str = scheme_call1("symbol->string", scheme_symbol);
  eassert (Sstringp (scheme_str));
  scheme_call3("hashtable-set!", table, scheme_str, scheme_symbol);
  return scheme_symbol;
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
  ptr found = scheme_call3("hashtable-ref", c_data_table, key, Sfalse);
  if (found == Sfalse)
    return NULL;
  return scheme_malloc_ptr (found);
}

void
scheme_ptr_fill (ptr *p, ptr init, iptr num_words)
{
  eassert (num_words >= 0);
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
  } else if (Svectorp(a)) {
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
  eassert (Sfixnump (*p));
  ptr sym = lispsym[Sfixnum_value(*p)];
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
    if (Sfixnump(x)) return "fixnum?";
    if (Scharp(x)) return "char?";
    if (Snullp(x)) return "null?";
    if (Seof_objectp(x)) return "eof_object?";
    if (Sbwp_objectp(x)) return "bwp_object?";
    if (Sbooleanp(x)) return "boolean?";
    if (Spairp(x)) return "pair?";
    if (Ssymbolp(x)) return "symbol?";
    if (Sprocedurep(x)) return "procedure?";
    if (Sflonump(x)) return "flonum?";
    if (Svectorp(x)) return "vector?";
    if (Sfxvectorp(x)) return "fxvector?";
    if (Sbytevectorp(x)) return "bytevector?";
    if (Sstringp(x)) return "string?";
    if (Sbignump(x)) return "bignum?";
    if (Sboxp(x)) return "box?";
    if (Sinexactnump(x)) return "inexactnum?";
    if (Sexactnump(x)) return "exactnum?";
    if (Sratnump(x)) return "ratnum?";
    if (Sinputportp(x)) return "inputport?";
    if (Soutputportp(x)) return "outputport?";
    if (Srecordp(x)) return "record?";
    return NULL;
  }
}

static bool scheme_fixnump(ptr x) { return Sfixnump(x); }
static bool scheme_charp(ptr x) { return Scharp(x); }
static bool scheme_nullp(ptr x) { return Snullp(x); }
static bool scheme_eof_objectp(ptr x) { return Seof_objectp(x); }
static bool scheme_bwp_objectp(ptr x) { return Sbwp_objectp(x); }
static bool scheme_booleanp(ptr x) { return Sbooleanp(x); }
static bool scheme_pairp(ptr x) { return Spairp(x); }
static bool scheme_symbolp(ptr x) { return Ssymbolp(x); }
static bool scheme_procedurep(ptr x) { return Sprocedurep(x); }
static bool scheme_flonump(ptr x) { return Sflonump(x); }
static bool scheme_vectorp(ptr x) { return Svectorp(x); }
static bool scheme_fxvectorp(ptr x) { return Sfxvectorp(x); }
static bool scheme_bytevectorp(ptr x) { return Sbytevectorp(x); }
static bool scheme_stringp(ptr x) { return Sstringp(x); }
static bool scheme_bignump(ptr x) { return Sbignump(x); }
static bool scheme_boxp(ptr x) { return Sboxp(x); }
static bool scheme_inexactnump(ptr x) { return Sinexactnump(x); }
static bool scheme_exactnump(ptr x) { return Sexactnump(x); }
static bool scheme_ratnump(ptr x) { return Sratnump(x); }
static bool scheme_inputportp(ptr x) { return Sinputportp(x); }
static bool scheme_outputportp(ptr x) { return Soutputportp(x); }
static bool scheme_recordp(ptr x) { return Srecordp(x); }

static const char *
gdb_print_scheme(Lisp_Object obj)
{
  static char buffer[4096];

  static ptr (*print_fun)(ptr);

  if (!print_fun)
    print_fun = get_scheme_func ("print-to-bytevector");
  ptr bvec = print_fun(obj);
  Slock_object (bvec);
  eassert (Sbytevectorp (bvec));
  return Sbytevector_data(bvec);
}

static const char *
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

extern ptr
scheme_function_for_name(const char *name) {
  ptr sym = Sstring_to_symbol(name);
  eassert(Ssymbolp(sym));
  ptr fun = Stop_level_value(sym);
  Slock_object (fun);
  //eassert(Sprocedurep(fun));
  return fun;
}

#endif /* HAVE_CHEZ_SCHEME */
