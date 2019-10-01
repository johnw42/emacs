// Only include via lisp.h!

#include "pp_foldmap.h"

#ifdef HAVE_CHEZ_SCHEME
extern void *chez_saved_bp;

#define CHEZ_PROLOG asm ("movq %%rbp, %0" : "=r" (chez_saved_bp))
#define CHEZ_EPILOG chez_saved_bp = 0
#include "chez_scheme.h"
#endif

#ifdef PARANOID_XMALLOC
#define CHECK_ALLOC(p) do { KILROY_WAS_HERE; check_alloc(p); } while (0)
void check_alloc (void *);

struct xmalloc_header {
  const char *file;
  intptr_t line;
  size_t size;
  uintptr_t signature[4];
  void *data[];
};
#else
#define CHECK_ALLOC(p) ((void)(p))
#endif

#ifdef HAVE_CHEZ_SCHEME
#define CONST_UNLESS_SCHEME
#define IF_SCHEME(var) ((void)(var))
#else /* not HAVE_CHEZ_SCHEME */
#define CONST_UNLESS_SCHEME const
#define IF_SCHEME(var)
#define scheme_check_ptr(x,y) ((void)0)
#endif /* not HAVE_CHEZ_SCHEME */

#ifdef HAVE_CHEZ_SCHEME

typedef struct { chez_ptr ptr; } Lisp_Object;
#define CHEZ(x) ((x).ptr)
#define UNCHEZ(x) ((Lisp_Object){x})
#define CHEZ_PTR(x) (&CHEZ (x))
#define UNCHEZ_PTR(x) ((Lisp_Object *)(x))

#undef CHECK_LISP_OBJECT_TYPE
enum CHECK_LISP_OBJECT_TYPE { CHECK_LISP_OBJECT_TYPE = true };

void scheme_init(void);
void scheme_deinit(void);
void syms_of_scheme_lisp(void);

Lisp_Object scheme_oblookup(Lisp_Object obarray, Lisp_Object name, bool add_if_missing);
/* Lisp_Object scheme_intern(const char *str, iptr len, Lisp_Object obarray); */
/* void scheme_intern_sym(Lisp_Object sym, Lisp_Object obarray); */
void *scheme_alloc_c_data(Lisp_Object key, chez_iptr size);
void *scheme_find_c_data (Lisp_Object key);
chez_ptr scheme_obarray_table (Lisp_Object obarray);
void scheme_Lisp_Object_fill (Lisp_Object *p, Lisp_Object init, chez_iptr num_words);
Lisp_Object to_lisp_string (Lisp_Object str);
Lisp_Object to_scheme_string (Lisp_Object lstr);
Lisp_Object make_scheme_string (const char *data, chez_iptr nchars, chez_iptr nbytes, bool multibyte);
typedef void (*lisp_ref_visitor_fun)(void *, Lisp_Object *, ptrdiff_t);
void visit_lisp_refs(Lisp_Object obj, lisp_ref_visitor_fun, void *data);
void init_nil_refs(Lisp_Object obj);
bool symbol_is(Lisp_Object sym, const char *name);
bool datum_starts_with(Lisp_Object, const char *);
void do_scheme_gc (void);
void suspend_scheme_gc (void);
void resume_scheme_gc (void);
extern int disable_scheme_gc;
int before_scheme_gc (void);
void after_scheme_gc (void);
Lisp_Object scheme_track (Lisp_Object);
Lisp_Object scheme_untrack (Lisp_Object);

void gdb_break(void);


bool analyze_scheme_ref(Lisp_Object ref, const char *label);
bool analyze_scheme_ref_ptr(Lisp_Object *ptr, const char *label);

extern uint64_t gdb_misc_val;
extern unsigned gdb_flags;
extern int gdb_hit_count;

#define SCHEME_FPTR_DEF(name, rtype, ...)                                \
  extern rtype scheme_fptr_result_##name;                                \
  extern rtype (*scheme_fptr_##name)(const char *, int, __VA_ARGS__)
#include "scheme_fptr.h"

void do_chez_prolog (void);

/* #define SCHEME_FPTR_CALL(name, ...)                             \ */
/*   (last_scheme_function = #name,                                \ */
/*     last_scheme_call_file = __FILE__,                           \ */
/*     last_scheme_call_line = __LINE__,                           \ */
/*     do_chez_prolog(),                                           \ */
/*     scheme_fptr_result_##name =                                 \ */
/*     scheme_fptr_##name(__FILE__, __LINE__, __VA_ARGS__),        \ */
/*     CHEZ_EPILOG,                                                \ */
/*     scheme_fptr_result_##name) */

#define SCHEME_FPTR_CALL(name, ...)                                   \
  (scheme_fptr_call_info.fun = #name,                                 \
   scheme_fptr_call_info.file = __FILE__,                             \
   scheme_fptr_call_info.line = __LINE__,                             \
   scheme_fptr_call_fun(),                                            \
   scheme_fptr_##name(__FILE__, __LINE__, __VA_ARGS__))

extern chez_ptr scheme_vectorlike_symbol;
extern chez_ptr scheme_misc_symbol;
extern chez_ptr scheme_string_symbol;
extern chez_iptr scheme_greatest_fixnum;
extern chez_iptr scheme_least_fixnum;
extern chez_iptr scheme_fixnum_width;
extern chez_ptr scheme_guardian;

struct scheme_fptr_call_info {
  const char *fun;
  const char *file;
  int line;
  size_t count;
};
extern struct scheme_fptr_call_info scheme_fptr_call_info;

INLINE void scheme_fptr_call_fun(void) {
  scheme_fptr_call_info.count++;
}

#define SCHEME_PV_TAG(pv) chez_vector_ref(pv, 0)
#define SCHEME_PV_ADDR(pv) chez_vector_ref(pv, 1)
#define SCHEME_PV_GCVEC(pv) chez_vector_ref(pv, 2)
#define SCHEME_PV_TAG_SET(pv, x) chez_vector_set(pv, 0, x)
#define SCHEME_PV_ADDR_SET(pv, x) chez_vector_set(pv, 1, x)
#define SCHEME_PV_GCVEC_SET(pv, x) chez_vector_set(pv, 2, x)
#define SCHEME_PV_LENGTH 3

// Gets GC vector.  Returns chez_false if the object could have a
// vector but doesn't, and chez_true if it cannot have a vector.
chez_ptr scheme_get_gcvec (Lisp_Object obj);
void scheme_set_gcvec (Lisp_Object obj, chez_ptr);

#undef LOCK
#undef UNLOCK

/* INLINE Lisp_Object */
/* scheme_locked_symbol(const char *name) { */
/*   chez_ptr sym = chez_string_to_symbol(name); */
/*   chez_lock_object(sym); */
/*   return UNCHEZ (sym); */
/* } */

#define Smake_string emacs_Smake_string
#define Smake_vector emacs_Smake_vector
#define Scons emacs_Scons


#undef USE_LSB_TAG

#define scheme_save_ptr(ptr, type) ((void)0)
#define scheme_check_ptr(ptr, type) ((void)0)

INLINE void *
scheme_malloc_ptr(Lisp_Object addr) {
  gdb_misc_val = (uint64_t) CHEZ (addr);
  analyze_scheme_ref (addr, "scheme_malloc_ptr");
  eassert (chez_fixnump (CHEZ (addr)));
  void *data = (void *) chez_fixnum_value (CHEZ (addr));
  CHECK_ALLOC (data);
  return data;
}

extern chez_ptr scheme_function_for_name(const char *name);

#define scheme_call0(f) (chez_call0(scheme_function_for_name(f)))
#define scheme_call1(f, a) (chez_call1(scheme_function_for_name(f), a))
#define scheme_call2(f, a, b) (chez_call2(scheme_function_for_name(f), a, b))
#define scheme_call3(f, a, b, c) (chez_call3(scheme_function_for_name(f), a, b, c))
INLINE void
scheme_ptr_copy (Lisp_Object *dest, Lisp_Object *src, chez_iptr num_words)
{
  memcpy (dest, src, num_words * sizeof (Lisp_Object));
}

struct Lisp_Symbol *scheme_make_symbol(Lisp_Object name, int /*enum symbol_interned*/ interned);

INLINE void
gdb_set_flag (int i)
{
  gdb_flags |= 1U << i;
}

INLINE void
gdb_unset_flag (int i)
{
  gdb_flags &= ~(1U << i);
}

INLINE bool
gdb_pop_flag (int i)
{
  if (gdb_flags & (1U << i))
    {
      gdb_unset_flag (i);
      return true;
    }
  return false;
}

struct Lisp_Vector;
struct buffer;

void visit_pseudovector_lisp_refs (struct Lisp_Vector *v, lisp_ref_visitor_fun fun, void *data);
void visit_buffer_lisp_refs (struct buffer *b, lisp_ref_visitor_fun fun, void *data);
void visit_fringe_lisp_refs (lisp_ref_visitor_fun fun, void *data);
void visit_regexp_cache_lisp_refs(lisp_ref_visitor_fun fun, void *data);
void visit_kboard_lisp_refs (lisp_ref_visitor_fun fun, void *data);

void alloc_preinit (void);

typedef int (*compare_fun)(const void *, const void *);
typedef void (*merge_fun)(void *, const void *);

struct container {
  void *data;
  size_t size;
  size_t capacity;
  size_t elem_size;
  compare_fun sorted_by;
};

void *container_ref (struct container *c, size_t i);
void container_config (struct container *c, size_t elem_size);
void container_init (struct container *c, size_t elem_size);
void container_free (struct container *c);
void container_reset (struct container *c);
void container_reserve (struct container *c, size_t min_capacity);
void container_sort (struct container *c, compare_fun compare);
void *container_search (struct container *c, const void *key, compare_fun compare, bool force_sort);
size_t container_find_all (struct container *c, const void *key, compare_fun compare, size_t *begin, size_t *end);
void container_append (struct container *c, void *item);
void container_delete_if (struct container *c, bool (*pred)(const void *));
void container_uniq (struct container *c, compare_fun compare, merge_fun merge);


#define FOR_CONTAINER(i, c) for (size_t i = 0; i < (c)->size; i++)

#define CONTAINER_REF(type, c, i) \
  (eassume ((c)->elem_size == sizeof (type)), ((type *) (c)->data) + (i))

#define NAMED_CONTAINER_DECL(name, type)                                \
  struct container name;                                                \
  typedef type name##_type

#define CONTAINER_OWNS_ADDR(c, addr)                                    \
  ((char *) (c)->data < (char *) (addr) &&                              \
   ((char *) (c)->data + (c)->elem_size * (c)->capacity >= (char *) (addr)))

#define ASSERT_TYPE(type, x) (true ? (type)(x) : (x))
#define EXTERN_NAMED_CONTAINER(name, type) extern NAMED_CONTAINER_DECL (name, type)
#define STATIC_NAMED_CONTAINER(name, type) static NAMED_CONTAINER_DECL (name, type)
#define NAMED_CONTAINER_REF(name, i) CONTAINER_REF (name##_type, &name, i)
#define NAMED_CONTAINER_REF_VAR(var, name, i) name##_type *var = CONTAINER_REF (name##_type, &name, i)
#define NAMED_CONTAINER_APPEND(name, val)                       \
  (name.elem_size = sizeof (name##_type),                       \
   container_append (&name, ASSERT_TYPE (name##_type *, val)))
#define NAMED_CONTAINER_CONFIG(name)                    \
  container_config (&name, sizeof (name##_type))
#define FOR_NAMED_CONTAINER(i, name) FOR_CONTAINER (i, &name)

void mark_lisp_refs (void);
bool mark_and_enqueue (Lisp_Object obj, const char *label);
extern int gc_count;
extern bool gc_running;

bool may_be_valid (chez_ptr x);
#endif

// TODO(jrw)
#define SUSPEND_GC()
#define RESUME_GC()
#define RETURN_RESUME_GC(...) return __VA_ARGS__

#define ENTER_LISP_FRAME(...)                    \
  ENTER_LISP_FRAME_T(Lisp_Object, __VA_ARGS__)
#define ENTER_LISP_FRAME_VA(...)                 \
  ENTER_LISP_FRAME_VA_T(Lisp_Object, __VA_ARGS__)
#define EXIT_LISP_FRAME(expr)                                   \
  do                                                            \
    {                                                           \
      this_lisp_frame_type this_lisp_frame_val = (expr);        \
      EXIT_LISP_FRAME_NO_RETURN();                              \
      return this_lisp_frame_val;                               \
    }                                                           \
  while (0)
#define EXIT_LISP_FRAME_VOID()                                  \
  do                                                            \
    {                                                           \
      EXIT_LISP_FRAME_NO_RETURN();                              \
      return;                                                   \
    }                                                           \
  while (0)

#ifdef HAVE_CHEZ_SCHEME
struct Lisp_Frame_Layout {
#ifdef ENABLE_CHECKING
  const char *func_name;
#endif
  size_t num_locals;
  ptrdiff_t *offsets;
};

struct Lisp_Frame_Record {
  struct Lisp_Frame_Record *prev;
  struct Lisp_Frame_Layout *layout;
  struct Lisp_Array_Record *arrays;
};

struct Lisp_Array_Record {
  struct Lisp_Array_Record *prev;
  Lisp_Object *data;
  size_t size;
};

// TODO: Make per-thread.
extern struct Lisp_Frame_Record *lisp_stack_ptr;

#ifdef ENABLE_CHECKING
#define LISP_FRAME_INFO_FUNC .func_name = __func__,
#else
#define LISP_FRAME_INFO_FUNC
#endif

void walk_lisp_stack (void (*f)(void *, Lisp_Object *), void *);

#define ENTER_LISP_FRAME_T(type, params, ...)                           \
  static bool is_this_lisp_frame_layout_init = false;                   \
  static ptrdiff_t this_lisp_offset_array                               \
  [PP_NARGS params + PP_NARGS (__VA_ARGS__)];                           \
  static struct Lisp_Frame_Layout this_lisp_frame_layout =              \
    {LISP_FRAME_INFO_FUNC \
     .num_locals = PP_NARGS params + PP_NARGS (__VA_ARGS__),            \
     .offsets = this_lisp_offset_array};                                \
  typedef type this_lisp_frame_type;                                    \
  (void) (this_lisp_frame_type *) 0;                                    \
  PP_MAP(LISP_LOCAL_VAR, __VA_ARGS__)                                   \
  struct Lisp_Frame_Record this_lisp_frame_record =                     \
    {.prev = lisp_stack_ptr, .layout = &this_lisp_frame_layout,         \
     .arrays = NULL};                                                   \
  if (!is_this_lisp_frame_layout_init)                                  \
    {                                                                   \
      is_this_lisp_frame_layout_init = true;                            \
      int offset_index = 0;                                             \
      LISP_LOCAL_MAP_ADDR params;                                       \
      LISP_LOCAL_MAP_ADDR(__VA_ARGS__);                                 \
    }                                                                   \
  lisp_stack_ptr = &this_lisp_frame_record
#define LISP_LOCAL_ARRAY(name, size_)                                   \
  Lisp_Object name[size_];                                              \
  set_nil (name, size_);                                                \
  struct Lisp_Array_Record name##_array_record =                        \
    {.prev = this_lisp_frame_record.arrays,                             \
     .data = name, .size = size_};                                      \
  this_lisp_frame_record.arrays = &name##_array_record;
#define LISP_DYNAMIC_ARRAY(name)                                        \
  Lisp_Object *name = NULL;                                             \
  struct Lisp_Array_Record name##_array_record =                        \
    {.prev = this_lisp_frame_record.arrays, .size = 0};                 \
  this_lisp_frame_record.arrays = &name##_array_record;                 \
  USE_SAFE_ALLOCA
#define LISP_ARRAY_PARAM(name, size_)                                   \
  struct Lisp_Array_Record args_array_record =                          \
    {.prev = this_lisp_frame_record.arrays,                             \
     .data = name, .size = size_};                                      \
  this_lisp_frame_record.arrays = &args_array_record
#define UPDATE_LISP_DYNAMIC_ARRAY(name, size_)                          \
  do                                                                    \
    {                                                                   \
      name##_array_record.data = name;                                  \
      name##_array_record.size = size_;                                 \
      set_nil (name##_array_record.data, name##_array_record.size);     \
    }                                                                   \
  while (0)
#define ENTER_LISP_FRAME_VA_T(type, nargs, args, ...)                   \
  ENTER_LISP_FRAME_T (type __VA_OPT__(, __VA_ARGS__));                  \
  LISP_ARRAY_PARAM (args, nargs)
#define LISP_LOCAL_VAR(name) Lisp_Object name = Qnil;
#define LISP_LOCAL_ADDR(var)                             \
  this_lisp_offset_array[offset_index++] =               \
    (char *) &var - (char *) &this_lisp_frame_record;
#define LISP_LOCAL_MAP_ADDR(...) \
  __VA_OPT__(PP_MAP(LISP_LOCAL_ADDR, __VA_ARGS__))
#define EXIT_LISP_FRAME_NO_RETURN()                     \
  (lisp_stack_ptr = this_lisp_frame_record.prev)
#define SAVE_LISP_FRAME_PTR() \
  struct Lisp_Frame_Record *saved_lisp_stack_ptr = lisp_stack_ptr
#define RESTORE_LISP_FRAME_PTR() (lisp_stack_ptr = saved_lisp_stack_ptr)

#define REGISTER_LISP_GLOBALS(...) \
  do { __VA_OPT__ (PP_MAP (REGISTER_LISP_GLOBAL, __VA_ARGS__)) } while (0)

#define REGISTER_LISP_GLOBAL_ARRAY(name) \
  scheme_register_globals (name, ARRAYELTS(name))

#define REGISTER_LISP_GLOBAL(name)            \
  scheme_register_globals (&name, 1);

#else /* not HAVE_CHEZ_SCHEME */

#define ENTER_LISP_FRAME_T(type, params, ...)       \
  __VA_OPT__(Lisp_Object __VA_ARGS__;)              \
    typedef type this_lisp_frame_type;              \
  (void) (this_lisp_frame_type *) 0
#define ENTER_LISP_FRAME_VA_T(type, nargs, args, params, ...)   \
  ENTER_LISP_FRAME_T(type, params __VA_OPT__(,  __VA_ARGS__))
#define EXIT_LISP_FRAME_NO_RETURN() ((void)0)
#define SAVE_LISP_FRAME_PTR() ((void)0)
#define RESTORE_LISP_FRAME_PTR() ((void)0)
#define LISP_LOCAL_ARRAY(name, size_)                                   \
  Lisp_Object name[size_];                                              \
  set_nil (name, size_)
#define LISP_DYNAMIC_ARRAY(name)                                        \
  Lisp_Object *name = NULL;                                             \
  USE_SAFE_ALLOCA
#define LISP_ARRAY_PARAM(name, size_) ((void)0)
#define UPDATE_LISP_DYNAMIC_ARRAY(name, size_) ((void)0)
#define REGISTER_LISP_GLOBALS(...) ((void)0)
#define REGISTER_LISP_GLOBAL_ARRAY(...) ((void)0)

#endif

#ifdef HAVE_CHEZ_SCHEME
#define IS_SCHEME_REF(ref, num) (CHEZ (ref) == (void *)num)
#else
#define IS_SCHEME_REF(ref, num) false
#endif
