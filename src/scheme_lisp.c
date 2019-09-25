#include <config.h>

#ifdef HAVE_CHEZ_SCHEME

#include "lisp.h"
#include "buffer.h"
#include "window.h"
#include "intervals.h"
#include "frame.h"
#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <signal.h>
#include <ucontext.h>

#pragma GCC diagnostic ignored "-Wdiscarded-qualifiers"

#ifdef ENABLE_CHECKING
static void run_init_checks(void);
#endif

static bool scheme_initialized = false;
static Lisp_Object c_data_table;

chez_ptr scheme_vectorlike_symbol = chez_false;
chez_ptr scheme_misc_symbol = chez_false;
chez_ptr scheme_string_symbol = chez_false;
chez_ptr c_data_property_symbol = chez_false;
chez_iptr scheme_greatest_fixnum;
chez_iptr scheme_least_fixnum;
chez_iptr scheme_fixnum_width;
chez_ptr scheme_guardian;
struct scheme_fptr_call_info scheme_fptr_call_info;

uint64_t gdb_misc_val = 0;
unsigned gdb_flags = 0;

#define SCHEME_FPTR_DEF(name, rtype, ...)                                \
  rtype scheme_fptr_result_##name;                                       \
  rtype (*scheme_fptr_##name)(const char *, int, __VA_ARGS__) = 0
#include "scheme_fptr.h"

void do_chez_prolog (void)
{
  CHEZ_PROLOG;
}


void syms_of_scheme_lisp(void) {
  /* DEFSYM(Qscheme_value_ref_id, "scheme-value-ref-id"); */
  /* DEFSYM(Qensure_scheme_value_ref, "ensure-scheme-value-ref"); */
  /* DEFSYM(Qensure_lisp_object_id, "ensure-lisp-object-id"); */
  /* DEFSYM(Qscheme_internal_reset, "scheme-internal-reset"); */
  /* defsubr(&Sscheme_funcall); */
  /* defsubr(&Sscheme_top_level_value); */
  /* defsubr(&Sforget_scheme_object); */
}

void scheme_deinit(void) {
  if (scheme_initialized) {
    chez_scheme_deinit();
    scheme_initialized = false;
  }
}

static void *
get_scheme_func(const char *name)
{
  chez_ptr sym = chez_string_to_symbol (name);
  eassert (chez_symbolp (sym));
  chez_ptr code = chez_top_level_value (sym);
  eassert ((chez_uptr) code >= 0x1000);
  chez_lock_object (code);
  return chez_foreign_callable_entry_point (code);
}

void *chez_saved_bp = NULL;

static void
scheme_abort (void)
{
  printf ("scheme_abort\n");
  //abort();
}

static void
scheme_sigaction (int sig, siginfo_t *info, void *ucontext)
{
  sigset_t set;
  sigemptyset (&set);
  sigaddset (&set, sig);
  sigprocmask (SIG_UNBLOCK, &set, (sigset_t *) 0);

  printf("called scheme_sigaction\n");

  // Both versions seem to have the same effect.
  if (chez_saved_bp)
    {
#if 1
      asm ("movq %0, %%rbp" : : "rm" (chez_saved_bp));
#else
      ucontext_t *ctx = ucontext;
      ctx->uc_mcontext.gregs[REG_RBP] = (uint64_t) chez_saved_bp;
#endif
      abort();
    }
}

void scheme_init(void) {
  const char *char_ptr = NULL;
  const char **argv = &char_ptr;

  eassert(!scheme_initialized);

  chez_scheme_init(NULL);
  printf ("boot file: %s\n", CHEZ_SCHEME_DIR "/scheme.boot");
  chez_register_boot_file(CHEZ_SCHEME_DIR "/scheme.boot");
  chez_build_heap(NULL, NULL);

  struct sigaction act, old_act;
  act.sa_sigaction = scheme_sigaction;
  act.sa_flags = SA_SIGINFO | SA_RESETHAND | SA_NODEFER;
  sigemptyset(&act.sa_mask);
  sigaction(SIGSEGV, &act, &old_act);

  scheme_greatest_fixnum = chez_fixnum_value(scheme_call0("greatest-fixnum"));
  scheme_least_fixnum = chez_fixnum_value(scheme_call0("least-fixnum"));
  scheme_fixnum_width = chez_fixnum_value(scheme_call0("fixnum-width"));

  scheme_guardian = scheme_call0 ("make-guardian");
  chez_lock_object (scheme_guardian);

  chez_foreign_symbol("do_scheme_gc", do_scheme_gc);
  chez_foreign_symbol("before_scheme_gc", before_scheme_gc);
  chez_foreign_symbol("after_scheme_gc", after_scheme_gc);
  chez_foreign_symbol("abort", abort);
  chez_foreign_symbol("Fequal", Fequal);
  chez_foreign_symbol("Fsxhash_equal", Fsxhash_equal);
  chez_scheme_script(BUILD_ROOT "/scheme/main.ss", 0, argv);
  scheme_call0("emacs-init");

#define SCHEME_FPTR_DEF(name, ...)                      \
  (scheme_fptr_##name = get_scheme_func ("c-" #name))
#include "scheme_fptr.h"

  c_data_table = UNCHEZ(scheme_call0("make-eq-hashtable"));
  scheme_track (c_data_table);

  scheme_vectorlike_symbol = scheme_call1("gensym", chez_string("emacs-vectorlike"));
  chez_lock_object (scheme_vectorlike_symbol);
  scheme_misc_symbol = scheme_call1("gensym", chez_string("emacs-misc"));
  chez_lock_object (scheme_misc_symbol);
  scheme_string_symbol = scheme_call1("gensym", chez_string("emacs-string"));
  chez_lock_object (scheme_string_symbol);
  c_data_property_symbol = scheme_call1("gensym", chez_string("c-data-property"));
  chez_lock_object (c_data_property_symbol);

#ifdef ENABLE_CHECKING
  run_init_checks();
#endif

  //atexit(scheme_deinit);
  scheme_initialized = true;
}

static void
print_origin(Lisp_Object obj)
{
  (void) SCHEME_FPTR_CALL(print_origin, CHEZ (obj));
}

// Converts a symbol or Scheme string to a Lisp string.
Lisp_Object
to_lisp_string(Lisp_Object arg)
{
  if (STRINGP (arg))
    return arg;
  if (chez_symbolp (CHEZ(arg)))
    arg = UNCHEZ(scheme_call1 ("symbol->string", CHEZ(arg)));
  eassert (chez_stringp (CHEZ(arg)));
  chez_iptr n = chez_string_length(CHEZ(arg));
  for (chez_iptr j = 0; j < n; j++)
    if (chez_string_ref (CHEZ(arg), j) >= 0x80)
      {
        // Slow path.
        Lisp_Object lstr = Fmake_string (make_number (n), make_number (0));
        for (int i = 0; i < n; i++)
          Faset (lstr, make_number (i), make_number (chez_string_ref (CHEZ(arg), i)));
        return lstr;
      }

  // Fast path.
  char *buf = alloca(n);
  for (chez_iptr i = 0; i < n; i++)
    {
      buf[i] = chez_string_ref (CHEZ(arg), i);
    }
  return make_unibyte_string (buf, n);
}

// Converts a symbol or Lisp string to a Scheme string.
Lisp_Object
to_scheme_string(Lisp_Object arg)
{
  if (chez_stringp (CHEZ(arg)))
    return arg;
  if (chez_symbolp (CHEZ(arg)))
    return UNCHEZ(scheme_call1 ("symbol->string", CHEZ(arg)));
  eassert (STRINGP (arg));
  chez_iptr n = XINT (Flength (arg));
  chez_ptr sstr = chez_make_uninitialized_string(n);
  scheme_track (UNCHEZ (sstr));
  for (chez_iptr i = 0; i < n; i++)
    chez_string_set (sstr, i, XINT (Faref (arg, make_number(i))));
  return UNCHEZ(sstr);
}

Lisp_Object
make_scheme_string (const char *data, chez_iptr nchars, chez_iptr nbytes, bool multibyte)
{
  if (nchars == nbytes && !multibyte)
    return UNCHEZ (chez_string_of_length (data, nchars));
  else
    return to_scheme_string
      (make_specified_string (data, nchars, nbytes, multibyte));
}

static struct Lisp_Symbol *
ensure_symbol_c_data (Lisp_Object symbol, Lisp_Object name)
{
  eassert (chez_symbolp (CHEZ (symbol)));

  struct Lisp_Symbol *p = SCHEME_FPTR_CALL (getprop_addr, CHEZ(symbol), c_data_property_symbol);
  if (p)
    {
      eassert (EQ (p->u.s.scheme_obj, symbol));
      return p;
    }

  if (CHEZ(name) == chez_false)
    name = to_lisp_string (symbol);
  eassert (STRINGP (name));

  p = scheme_alloc_c_data(symbol, sizeof (struct Lisp_Symbol));
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
  CHECK_ALLOC(p);
  eassert (EQ (p->u.s.scheme_obj, symbol));
  return p;
}

struct Lisp_Symbol *
scheme_make_symbol(Lisp_Object name, int /*enum symbol_interned*/ interned)
{
  Lisp_Object scheme_symbol = UNCHEZ (chez_false);
  if (chez_symbolp (CHEZ (name)))
      scheme_symbol = name;
  else
    {
      Lisp_Object scheme_str = to_scheme_string (name);
      scheme_symbol = UNCHEZ(scheme_call1
        (interned == SYMBOL_INTERNED_IN_INITIAL_OBARRAY ?
         "string->symbol" : "gensym",
         CHEZ(scheme_str)));
      scheme_track (scheme_symbol);
    }

  eassert (chez_symbolp (CHEZ (scheme_symbol)));

  Lisp_Object lisp_str = to_lisp_string (name);
  struct Lisp_Symbol *xs = ensure_symbol_c_data (scheme_symbol, lisp_str);
  xs->u.s.interned = interned;
  return xs;
}

struct Lisp_Symbol *
XSYMBOL (Lisp_Object a)
{
  struct Lisp_Symbol *p = ensure_symbol_c_data (a, UNCHEZ (chez_false));
  eassert (EQ (p->u.s.scheme_obj, a));
  return p;
}

void *
scheme_alloc_c_data (Lisp_Object key, chez_iptr size)
{
  void *bytes = xzalloc (size);
  SCHEME_FPTR_CALL (putprop_addr, CHEZ(key), c_data_property_symbol, bytes);
  return bytes;
}

void *
scheme_find_c_data (Lisp_Object key)
{
  chez_ptr found = SCHEME_FPTR_CALL(hashtable_ref, CHEZ (c_data_table), CHEZ (key), chez_false);
  if (found == chez_false)
    return NULL;
  return scheme_malloc_ptr (UNCHEZ (found));
}

static void
scheme_ptr_fill (Lisp_Object *p, Lisp_Object init, chez_iptr num_words)
{
  eassert (num_words >= 0);
  eassert (chez_symbolp (CHEZ (Qnil)));
  for (chez_iptr i = 0; i < num_words; i++) {
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
  } else if (chez_vectorp (CHEZ (a))) {
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
fixup_lispsym_inits(Lisp_Object *p, size_t n)
{
  // Reverse the transformation applied by LISPSYM_INITIALLY.
  for (size_t i = 0; i < n; i++) {
    chez_uptr index = (XLI (p[i]) >> 8) & 0xffff;
    eassert (0 <= index && index < ARRAYELTS(lispsym));
    Lisp_Object sym = lispsym[index];
    eassert (SYMBOLP (sym));
    p[i] = sym;
    staticpro (p);
  }
}

static const char *scheme_classify(Lisp_Object x)
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
    if (chez_fixnump(CHEZ(x))) return "fixnum?";
    if (chez_charp(CHEZ(x))) return "char?";
    if (chez_nullp(CHEZ(x))) return "null?";
    if (chez_eof_objectp(CHEZ(x))) return "eof_object?";
    if (chez_bwp_objectp(CHEZ(x))) return "bwp_object?";
    if (chez_booleanp(CHEZ(x))) return "boolean?";
    if (chez_pairp(CHEZ(x))) return "pair?";
    if (chez_symbolp(CHEZ(x))) return "symbol?";
    if (chez_procedurep(CHEZ(x))) return "procedure?";
    if (chez_flonump(CHEZ(x))) return "flonum?";
    if (chez_vectorp(CHEZ(x))) return "vector?";
    if (chez_fxvectorp(CHEZ(x))) return "fxvector?";
    if (chez_bytevectorp(CHEZ(x))) return "bytevector?";
    if (chez_stringp(CHEZ(x))) return "string?";
    if (chez_bignump(CHEZ(x))) return "bignum?";
    if (chez_boxp(CHEZ(x))) return "box?";
    if (chez_inexactnump(CHEZ(x))) return "inexactnum?";
    if (chez_exactnump(CHEZ(x))) return "exactnum?";
    if (chez_ratnump(CHEZ(x))) return "ratnum?";
    if (chez_inputportp(CHEZ(x))) return "inputport?";
    if (chez_outputportp(CHEZ(x))) return "outputport?";
    if (chez_recordp(CHEZ(x))) return "record?";
    return NULL;
  }
}

static chez_ptr
gdb_vector_ref (chez_ptr v, chez_iptr i)
{
  if (chez_vectorp (v) && 0 <= i && i < chez_vector_length (v))
    return chez_vector_ref (v, i);
  return 0;
}

static void *
gdb_bytevector_data (chez_ptr v)
{
  if (chez_bytevectorp (v))
    return chez_bytevector_data (v);
  return 0;
}

static void
append_lisp_refs (void *data, Lisp_Object *refs, ptrdiff_t n)
{
  char **buf_ptr = data;
  for (ptrdiff_t i = 0; i < n; i++)
    {
      int nchars = sprintf (*buf_ptr, " %p", refs + i);
      if (nchars > 0)
        *buf_ptr += nchars;
    }
}

static char *
gdb_lisp_refs(chez_ptr obj)
{
  static char buffer[4096];
  char *buf_ptr = buffer;
  visit_lisp_refs (UNCHEZ (obj), append_lisp_refs, &buf_ptr);
  return buffer;
}

#include <sys/types.h>
#include <unistd.h>
#include <sys/wait.h>

#define DEBUG_BUF_SIZE 4096

static const char *
gdb_write (void (*fun)(char *buf, Lisp_Object obj), chez_ptr obj)
{
  char path[PATH_MAX];
  char *tmpdir = getenv("TMPDIR");
  if (!tmpdir)
    tmpdir = P_tmpdir;
  strncpy (path, tmpdir, sizeof (path) - 1);
  strncat (path, "/lispXXXXXX", sizeof (path) - 1);
  int fd = mkstemp(path);
  unlink (path);

  static char buffer[DEBUG_BUF_SIZE];
  pid_t child = fork();
  if (child != 0)
    {
      int status;
      waitpid(child, &status, 0);
      if (status == 0)
        {
          lseek (fd, 0, SEEK_SET);
          size_t n = read (fd, buffer, DEBUG_BUF_SIZE - 1);
          buffer[n] = '\0';
        }
      else
        sprintf(buffer, "child process error: %d\n", status);
      close (fd);
      return buffer;
    }
  else
    {
      fun (buffer, UNCHEZ (obj));
      size_t n = write (fd, buffer, DEBUG_BUF_SIZE);
      exit(n == DEBUG_BUF_SIZE ? 0 : 1);
    }
}

static void
gdb_print_scheme(char *buf, Lisp_Object obj)
{
  suspend_scheme_gc();
  /* if (STRINGP (obj)) */
  /*   obj = to_scheme_string (obj); */
  chez_ptr bvec = SCHEME_FPTR_CALL(print_to_bytevector, CHEZ (obj));
  chez_lock_object (bvec);
  eassert (chez_bytevectorp (bvec));
  chez_iptr n = chez_bytevector_length(bvec);
  n = min (n, DEBUG_BUF_SIZE - 1);
  memcpy (buf, chez_bytevector_data (bvec), n);
  buf[n] = '\0';
}

static void
gdb_print_lisp(char *buf, Lisp_Object obj)
{
  if (XTYPE (obj) == Lisp_Chez_Internal)
    {
      gdb_print_scheme (buf, obj);
      return;
    }

  Lisp_Object str = Fprin1_to_string (obj, Qnil);
  chez_iptr n = SCHARS(str);
  n = min (n, DEBUG_BUF_SIZE - 1);
  for (chez_iptr i = 0; i < n; i++) {
    EMACS_INT c = XINT (Faref(str, make_number(i)));
    buf[i] = 0 < c && c < 255 ? c : 255;
  }
  buf[n] = '\0';
}

static const char *last_func_name = NULL;

extern chez_ptr
scheme_function_for_name(const char *name) {
  last_func_name = name;
  chez_ptr sym = chez_string_to_symbol(name);
  eassert(chez_symbolp(sym));
  chez_ptr fun = chez_top_level_value(sym);
  scheme_track (UNCHEZ (fun));
  //eassert(chez_procedurep(fun));
  return fun;
}

void
visit_pseudovector_lisp_refs (struct Lisp_Vector *v, lisp_ref_visitor_fun fun, void *data)

{
  fun (data, &v->header.s.scheme_obj, 1);
  EMACS_INT n = pvsize_from_header (&v->header);
  if (n > 0)
    fun(data, v->contents, n);
}


static void
visit_sub_char_table_lisp_refs (struct Lisp_Sub_Char_Table *v, lisp_ref_visitor_fun fun, void *data)
{
  fun (data, &v->header.s.scheme_obj, 1);
  EMACS_INT n = pvsize_from_header (&v->header);
  if (n > 0)
    fun(data, v->contents, n);
}

struct visit_iterval_data {
  lisp_ref_visitor_fun fun;
  void *data;
};

static void
visit_interval_lisp_refs_fun (INTERVAL i, void *data)
{
  struct visit_iterval_data *vid = data;
  vid->fun(data, &i->plist, 1);
}

static void
visit_interval_tree_lisp_refs (INTERVAL i, lisp_ref_visitor_fun fun, void *data)
{
  if (i)
    {
      struct visit_iterval_data vid = {fun, data};
      traverse_intervals_noorder (i, visit_interval_lisp_refs_fun, &vid);
    }
}

static void
visit_overlay_lisp_refs (struct Lisp_Overlay *ptr, lisp_ref_visitor_fun fun, void *data)
{
  while (ptr)
    {
      fun(data, &ptr->start, 1);
      fun(data, &ptr->end, 1);
      fun(data, &ptr->plist, 1);
      ptr = ptr->next;
    }
}

void
visit_buffer_lisp_refs(struct buffer *b, lisp_ref_visitor_fun fun, void *data)
{
  visit_pseudovector_lisp_refs ((void *) b, fun, data);

  fun (data, &b->undo_list_, 1);

  /* if (b->text) */
  /*   visit_interval_tree_lisp_refs (b->text->intervals, fun, data); */

  visit_overlay_lisp_refs (b->overlays_before, fun, data);
  visit_overlay_lisp_refs (b->overlays_after, fun, data);
}

static void
visit_face_cache_lisp_refs (struct face_cache *c, lisp_ref_visitor_fun fun, void *data)
{
  if (c)
    for (int i = 0; i < c->used; ++i)
      {
        struct face *face = FACE_FROM_ID_OR_NULL (c->f, i);
        if (face)
          {
            if (face->font)
              visit_pseudovector_lisp_refs
                ((struct Lisp_Vector *) face->font, fun, data);
            fun (data, face->lface, LFACE_VECTOR_SIZE);
          }
      }
}

static void
visit_glyph_matrix_lisp_refs (struct glyph_matrix *matrix, lisp_ref_visitor_fun fun, void *data)
{
  struct glyph_row *row = matrix->rows;
  struct glyph_row *end = row + matrix->nrows;

  for (; row < end; ++row)
    if (row->enabled_p)
      for (int area = LEFT_MARGIN_AREA; area < LAST_AREA; ++area)
        {
          struct glyph *glyph = row->glyphs[area];
          struct glyph *end_glyph = glyph + row->used[area];

          for (; glyph < end_glyph; ++glyph)
            fun (data, &glyph->object, 1);
        }
}

static void
visit_specpdl_lisp_refs (union specbinding *first, union specbinding *stop,
                         lisp_ref_visitor_fun fun, void *data)
{
  union specbinding *pdl;
  for (pdl = first; pdl != stop; pdl++)
    {
      switch (pdl->kind)
	{
	case SPECPDL_UNWIND:
          fun (data, &pdl->unwind.arg, 1);
	  break;

	case SPECPDL_BACKTRACE:
	  {
	    ptrdiff_t nargs = pdl->bt.nargs;
	    fun (data, &pdl->bt.function, 1);
	    if (nargs == UNEVALLED)
	      nargs = 1;
            fun (data, pdl->bt.args, nargs);
	  }
	  break;

	case SPECPDL_LET_DEFAULT:
	case SPECPDL_LET_LOCAL:
	  fun (data, &pdl->let.where, 1);
	  FALLTHROUGH;
	case SPECPDL_LET:
	  fun (data, &pdl->let.symbol, 1);
	  fun (data, &pdl->let.old_value, 1);
	  fun (data, &pdl->let.saved_value, 1);
	  break;

	case SPECPDL_UNWIND_PTR:
	case SPECPDL_UNWIND_INT:
	case SPECPDL_UNWIND_VOID:
	  break;

	default:
	  emacs_abort ();
	}
    }
}

static void
visit_symbol_lisp_refs(Lisp_Object obj, lisp_ref_visitor_fun fun, void *data)
{
  struct Lisp_Symbol *s = XSYMBOL (obj);
  if (symbol_is (obj, "buffer-undo-list"))
    printf("visiting buffer-undo-list\n");
  if (s->u.s.redirect == SYMBOL_VARALIAS)
    s = s->u.s.val.alias;
  fun (data, &s->u.s.scheme_obj, 1);
  fun (data, &s->u.s.name, 1);
  switch (s->u.s.redirect)
    {
    case SYMBOL_PLAINVAL:
      fun (data, &s->u.s.val.value, 1);
      break;
    case SYMBOL_LOCALIZED:
      {
        struct Lisp_Buffer_Local_Value *blv = SYMBOL_BLV (s);
        fun (data, &blv->where, 1);
        fun (data, &blv->valcell, 1);
        fun (data, &blv->defcell, 1);
      }
      break;
    case SYMBOL_FORWARDED:
      {
        union Lisp_Fwd *fwd = s->u.s.val.fwd;
        switch (XFWDTYPE (fwd))
          {
          case Lisp_Fwd_Obj:
            fun (data, fwd->u_objfwd.objvar, 1);
            break;
          case Lisp_Fwd_Buffer_Obj:
            {
              fun (data, &fwd->u_buffer_objfwd.predicate, 1);
              /* int offset = XBUFFER_OBJFWD (innercontents)->offset; */
              /* int idx = PER_BUFFER_IDX (offset); */
            }
            break;
          /* case Lisp_Fwd_Kboard_obj: */
          /*   // XXX */
          /*   break; */
          }
      }
      break;
    default:
      emacs_abort ();
    }
  fun(data, &s->u.s.function, 1);
  fun(data, &s->u.s.plist, 1);
  fun(data, &s->u.s.name, 1);
}

/* Finds all Lisp_Object references in OBJ not managed by the scheme
   garbage collector.  Calls FUN zero or more times, passing DATA
   and a pair of pointers indicating the stard and end of a block of
   Lisp_Object values. */
void
visit_lisp_refs(Lisp_Object obj, lisp_ref_visitor_fun fun, void *data)
{
  switch (XTYPE(obj))
    {
    case Lisp_Symbol:
      visit_symbol_lisp_refs(obj, fun, data);
      break;
    case Lisp_String:
      {
        struct Lisp_String *s = XSTRING (obj);
        fun (data, &s->u.s.scheme_obj, 1);
        visit_interval_tree_lisp_refs (s->u.s.intervals, fun, data);
      }
      break;
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
            {
              // Handle regular vectors.
              fun(data, aref_addr(obj, 0), n);

              // Handle obarray vectors.
              chez_ptr table = CHEZ (AREF (obj, 0));
              if (SCHEME_FPTR_CALL (hashtablep, table))
                {
                  printf("obarray %p\n", CHEZ(obj));
                  chez_ptr values_vec = SCHEME_FPTR_CALL (hashtable_values, table);
                  // Copy vector so we don't have to lock it.
                  chez_iptr n = chez_vector_length (values_vec);
                  Lisp_Object *values = alloca (n * sizeof (Lisp_Object));
                  for (chez_iptr i = 0; i < n; i++)
                    values[i] = UNCHEZ (chez_vector_ref (values_vec, i));
                  for (chez_iptr i = 0; i < n; i++)
                    {
                      Lisp_Object sym = values[i];
                      eassert (SYMBOLP (sym));
                      visit_lisp_refs (sym, fun, data);
                    }
                }
            }
        }
      else
        {
          switch (PSEUDOVECTOR_TYPE(XVECTOR(obj)))
            {
            case PVEC_NORMAL_VECTOR:
            case PVEC_PROCESS:
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
            case PVEC_RECORD:
            case PVEC_FONT:
            case PVEC_FREE:
            case PVEC_COMPILED:
            case PVEC_CHAR_TABLE:
              visit_pseudovector_lisp_refs (XVECTOR(obj), fun, data);
              break;
            case PVEC_SUB_CHAR_TABLE:
              visit_sub_char_table_lisp_refs (XSUB_CHAR_TABLE(obj), fun, data);
              break;
            case PVEC_FRAME:
              {
                visit_pseudovector_lisp_refs (XVECTOR(obj), fun, data);
                struct frame *f = (struct frame *) XVECTOR(obj);
                visit_face_cache_lisp_refs (f->face_cache, fun, data);
/* #ifdef HAVE_WINDOW_SYSTEM */
/*                 if (FRAME_WINDOW_P (f) && FRAME_X_OUTPUT (f)) */
/*                   { */
/*                     struct font *font = FRAME_FONT (f); */
/*                     if (font) */
/*                       visit_pseudovector_lisp_refs ((struct Lisp_Vector *) font, fun, data); */
/*                   } */
/* #endif */
                break;
              }
            case PVEC_WINDOW:
              {
                visit_pseudovector_lisp_refs (XVECTOR(obj), fun, data);
                struct window *w = XWINDOW(obj);
                if (w->current_matrix)
                  {
                    visit_glyph_matrix_lisp_refs (w->current_matrix, fun, data);
                    visit_glyph_matrix_lisp_refs (w->desired_matrix, fun, data);
                  }
                fun(data, &w->prev_buffers, 1);
                fun(data, &w->next_buffers, 1);
                break;
              }
            case PVEC_BUFFER:
              visit_buffer_lisp_refs (XBUFFER (obj), fun, data);
              break;
            case PVEC_HASH_TABLE:
              {
                visit_pseudovector_lisp_refs (XVECTOR(obj), fun, data);
                struct Lisp_Hash_Table *t = XHASH_TABLE(obj);
                fun(data, &t->key_and_value, 1);
                fun(data, &t->test.name, 1);
                fun(data, &t->test.user_hash_function, 1);
                fun(data, &t->test.user_cmp_function, 1);
                break;
              }
            case PVEC_THREAD:
              {
                visit_pseudovector_lisp_refs (XVECTOR(obj), fun, data);
                struct thread_state *t = XTHREAD(obj);
                visit_specpdl_lisp_refs (t->m_specpdl, t->m_specpdl_ptr, fun, data);
                for (struct handler *handler = t->m_handlerlist;
                     handler; handler = handler->next)
                  {
                    fun (data, &handler->tag_or_ch, 1);
                    fun (data, &handler->val, 1);
                  }
                if (t->m_current_buffer)
                  fun (data, &t->m_current_buffer->header.s.scheme_obj, 1);
                fun (data, &t->m_re_match_object, 1);
                fun (data, &t->m_last_thing_searched, 1);
                fun (data, &t->m_saved_last_thing_searched, 1);
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
      if (CHEZ (ptrs[i]) != data)
        {
          eassert (EQ (ptrs[i], Qnil) || XLI (ptrs[i]) == 0);
          ptrs[i] = Qnil;
        }
    }
}

void
init_nil_refs (Lisp_Object obj)
{
  eassert (chez_symbolp (CHEZ (Qnil)));
  visit_lisp_refs(obj, init_nil_ref_block, CHEZ (obj));
}


bool
symbol_is(Lisp_Object sym, const char *name)
{
  if (chez_symbolp (CHEZ(sym)) ||
      chez_stringp (CHEZ(sym)))
    return SCHEME_FPTR_CALL(symbol_is, CHEZ (sym), name);
  if (STRINGP (sym))
    return strncmp(SSDATA(sym), name, SCHARS(sym)) == 0;
  return false;
}

bool
datum_starts_with(Lisp_Object obj, const char *str)
{
  /* static const char *prev_str = NULL; */
  /* static size_t prev_length = 0; */

  /* if (str != prev_str) */
  /*   { */
  /*     prev_str = str; */
  /*     prev_length = strlen(str); */
  /*   } */
  char buffer[DEBUG_BUF_SIZE];
  gdb_print_lisp (buffer, obj);
  return strcmp(buffer, str) == 0;
}

void *
container_ref (struct container *c, size_t i)
{
  return (char *) c->data + i * c->elem_size;
}

void
container_config (struct container *c, size_t elem_size)
{
  c->elem_size = elem_size;
}

void
container_init (struct container *c, size_t elem_size)
{
  c->data = NULL;
  c->size = 0;
  c->capacity = 0;
  c->sorted_by = NULL;
  container_config(c, elem_size);
}

void
container_free (struct container *c)
{
  free (c->data);
  c->size = 0;
  c->capacity = 0;
}

void
container_reset (struct container *c)
{
  c->size = 0;
  c->sorted_by = NULL;
}

void
container_reserve (struct container *c, size_t min_capacity)
{
  if (min_capacity > c->capacity)
    {
      size_t new_capacity = 1;
      while (new_capacity < min_capacity)
        new_capacity *= 2;
      eassert (new_capacity >= min_capacity);
      c->data = reallocarray (c->data, new_capacity, c->elem_size);
      c->capacity  = new_capacity;
      eassert (c->data);
    }
  eassert (min_capacity == 0 || c->data);
}

void
container_sort (struct container *c, compare_fun compare)
{
  if (c->sorted_by != compare)
    qsort (c->data, c->size, c->elem_size, compare);
}

void *
container_search (struct container *c,
                  const void *key,
                  compare_fun compare,
                  bool force_sort)
{
  eassert (compare);

  if (force_sort)
    container_sort (c, compare);

  if (c->sorted_by == compare)
      return bsearch (key, c->data, c->size, c->elem_size, compare);
  else
      FOR_CONTAINER (i, c)
        {
          void *item = container_ref (c, i);
          if (compare (key, item) == 0)
            return item;
        }

  return NULL;
}


// Returns number of items found.
size_t
container_find_all (struct container *c,
                    const void *key,
                    compare_fun compare,
                    size_t *begin,
                    size_t *end)
{
  eassert (compare);
  eassert (begin);
  eassert (end);

  void *found = container_search (c, key, compare, true);
  if (!found)
    {
      *begin = *end = 0;
      return 0;
    }

  size_t start =
    ((char *) found - (char *) c->data) / c->elem_size;
  eassert (container_ref (c, start) == found);

  // Find the first matching item.
  size_t i;
  for (i = start; i > 0; i--)
    if (compare (container_ref (c, i - 1), found) != 0)
      break;

  // Find the last matching item.
  size_t j;
  for (j = start + 1; j < c->size; j++)
    if (compare (container_ref (c, j), found) != 0)
      break;

  eassert (i <= j);
  *begin = i;
  *end = j;
  return j - i;
}

void
container_append (struct container *c, void *item)
{
  container_reserve (c, c->size + 1);
  char *dest = container_ref (c, c->size);
  memcpy (dest, item, c->elem_size);
  c->sorted_by = NULL;
  c->size++;
}

void
container_delete_if (struct container *c, bool (*pred)(const void *))
{
  size_t di = 0;
  FOR_CONTAINER (si, c)
    {
      eassert (di <= si);
      if (!pred (container_ref (c, si)))
        {
          if (si != di)
            {
              memcpy (container_ref (c, di),
                      container_ref (c, si),
                      c->elem_size);
            }
          di++;
        }
    }
  c->size = di;
}

void
container_uniq (struct container *c, compare_fun compare, merge_fun merge)
{
  if (c->size == 0)
    return;

  container_sort (c, compare);

  size_t di = 0;
  FOR_CONTAINER (si, c)
    if (si > 0)
      {
        eassert (di < si);
        if (compare (container_ref (c, di),
                     container_ref (c, si)) == 0)
          {
            if (merge)
              merge (container_ref (c, di),
                     container_ref (c, si));
            else
              memcpy (container_ref (c, di),
                      container_ref (c, si),
                      c->elem_size);
          }
        else
          {
            di++;
            if (di != si)
              memcpy (container_ref (c, di),
                      container_ref (c, si),
                      c->elem_size);
          }
      }
  c->size = di + 1;
}

bool
may_be_valid (chez_ptr x)
{
  if (x == chez_false ||
      chez_pairp(x) ||
      chez_symbolp(x) ||
      chez_fixnump(x) ||
      chez_bignump(x) ||
      chez_flonump(x) ||
      chez_vectorp(x))
    return true;

  if (chez_stringp(x) ||
      chez_procedurep(x) ||
      chez_recordp(x))
    return true;

  // Valid Scheme types never used as Lisp values.
  /* if (x == chez_true || */
  /*     x == chez_nil || */
  /*     x == chez_eof_object || */
  /*     x == chez_bwp_object || */
  /*     x == chez_unbound || */
  /*     x == chez_void || */
  /*     chez_charp(x) || */
  /*     chez_bignump(x) || */
  /*     chez_stringp(x) || */
  /*     chez_ratnump(x) || */
  /*     chez_inexactnump(x) || */
  /*     chez_exactnump(x) || */
  /*     chez_fxvectorp(x) || */
  /*     chez_bytevectorp(x) || */
  /*     chez_boxp(x) || */
  /*     chez_codep(x) || */
  /*     chez_portp(x)) */
  /*   return true; */

  return false;
}

struct Lisp_Frame_Record *lisp_stack_ptr = NULL;

void
walk_lisp_stack (void (*f)(void *, Lisp_Object *), void *data)
{
  for (struct Lisp_Frame_Record *fr = lisp_stack_ptr; fr; fr = fr->prev)
    {
      struct Lisp_Frame_Layout *fl = fr->layout;
      char *base = (char *) fr;
      ptrdiff_t *offsets = fl->offsets;
      for (size_t i = 0, n = fl->num_locals; i < n; i++)
        f (data, (Lisp_Object *) (base + offsets[i]));
      for (struct Lisp_Array_Record *ar = fr->arrays; ar; ar = ar->prev)
        for (size_t i = 0, n = ar->size; i < n; i++)
          f (data, &ar->data[i]);
    }
}

#ifdef ENABLE_CHECKING
static int ptr_cmp (const void *p, const void *q)
{
  Lisp_Object **pp = p, **qq = q;
  if (*pp - *qq > 0) return 1;
  if (*pp - *qq < 0) return -1;
  return 0;
}

static void
run_init_checks(void)
{
#if 0
  return;

  Lisp_Object a = make_number(1), b = make_number(2);
  ENTER_LISP_FRAME ((a, b), c, d);

  Lisp_Object *expected[] = {&a, &b, &c, &d};
  qsort (expected, ARRAYELTS (expected),
         sizeof (Lisp_Object *), ptr_cmp);
  Lisp_Object *found[256];
  ptrdiff_t nfound = get_lisp_frame_ptrs (found);
  eassert (nfound == ARRAYELTS (expected));
  qsort (found, nfound, sizeof (Lisp_Object *), ptr_cmp);
  eassert (memcmp (found, expected, sizeof expected) == 0);

  {
    Lisp_Object args[] = {make_number(3), make_number(4)};
    ENTER_LISP_FRAME_VA (2, args, ());
    LISP_LOCAL_ARRAY (e, 2);

    Lisp_Object *expected[] =
      {
       &a, &b, &c, &d,
       &args[0], &args[1],
       &e[0], &e[1]
      };
    qsort (expected, ARRAYELTS (expected),
           sizeof (Lisp_Object *), ptr_cmp);
    ptrdiff_t nfound = get_lisp_frame_ptrs (found);
    eassert (nfound == ARRAYELTS (expected));
    qsort (found, nfound, sizeof (Lisp_Object *), ptr_cmp);
    eassert (memcmp (found, expected, sizeof expected) == 0);

    EXIT_LISP_FRAME_NO_RETURN ();
  }

  nfound = get_lisp_frame_ptrs (found);
  eassert (nfound == ARRAYELTS (expected));
  qsort (found, nfound, sizeof (Lisp_Object *), ptr_cmp);
  eassert (memcmp (found, expected, sizeof expected) == 0);

  EXIT_LISP_FRAME_NO_RETURN();
  eassert (lisp_stack_size == 1);

  lisp_frame_record_init();

  printf("tests pass!\n");
#endif
}
#endif



#endif /* HAVE_CHEZ_SCHEME */
