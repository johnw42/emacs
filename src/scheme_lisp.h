// Only include via lisp.h!

#include "pp_foldmap.h"

#ifdef HAVE_CHEZ_SCHEME
#define SCHEME_SUBRS
//#define PARANOID_XMALLOC
//#define SCHEME_EVAL_SUB
//#define SCHEME_DEBUG_STACK
//#define SCHEME_STRINGS
#endif

void gdb_break(void);

#if defined(HAVE_CHEZ_SCHEME) && defined(ENABLE_CHECKING)
#define SCHEME_PARANOIA 40
#else
#define SCHEME_PARANOIA 0
#endif

#define SCHEME_ASSERT(paranoia, cond) eassert (SCHEME_PARANOIA < paranoia || (cond))

#ifdef HAVE_CHEZ_SCHEME

#define SCHEME_FALSE_P(obj) (CHEZ (obj) == chez_false)
#define SCHEME_FALSE UNCHEZ (chez_false)

#define SCHEME_NIL UNCHEZ (chez_nil)
#define SCHEME_T   UNCHEZ (chez_true)

#define SCHEME_NIL_P(x) (CHEZ(x) == chez_nil)
#define SCHEME_T_P(x) (CHEZ(x) == chez_true)

#define CHEZ_BP_STACK_SIZE 1024
extern void *chez_bp_stack[CHEZ_BP_STACK_SIZE];
extern int chez_bp_stack_top;
extern void *scheme_last_pc;
extern int scheme_call_count;

bool check_special_case(void);

#if 1
#define CHEZ_PROLOG
#define CHEZ_EPILOG
#else
#define CHEZ_PROLOG                                                 \
  void *rbp;                                                        \
  asm ("movq %%rbp, %0" : "=mr" (rbp));                             \
  chez_bp_stack[chez_bp_stack_top] = rbp;                           \
  scheme_last_pc = ((void **)rbp)[1];                               \
  ++chez_bp_stack_top
#define CHEZ_EPILOG chez_bp_stack[--chez_bp_stack_top] = 0
#endif

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

#if defined(HAVE_CHEZ_SCHEME) && defined(ENABLE_CHECKING)
#define INSPECT_SCHEME_REF(ref, label) inspect_scheme_ref(ref, NULL, true, label)
#define INSPECT_SCHEME_REF_PTR(ptr, label) inspect_scheme_ref(SCHEME_FALSE, ptr, true, label)
#define INSPECT_SCHEME_REF_MAYBE_INVALID(ref, label) inspect_scheme_ref(ref, NULL, false, label)
#define INSPECT_SCHEME_REF_INFO(info, label) inspect_scheme_ref(UNCHEZ ((info)->ref), (Lisp_Object *) (info)->ref_ptr, true, label)
#define INSPECT_MALLOC_PTR(ptr, label) inspect_malloc_ptr(ptr, label)
#else
#define INSPECT_SCHEME_REF(ref, label) false
#define INSPECT_SCHEME_REF_PTR(ptr, label) false
#define INSPECT_SCHEME_REF_MAYBE_INVALID(ref, label) false
#define INSPECT_SCHEME_REF_INFO(info, label) false
#define INSPECT_MALLOC_PTR(ptr, label) false
#endif


#ifdef HAVE_CHEZ_SCHEME

typedef struct { chez_ptr ptr; } Lisp_Object;
#define CHEZ(x) ((x).ptr)
#define UNCHEZ(x) ((Lisp_Object){x})
#define CHEZ_PTR(x) (&CHEZ (x))
#define UNCHEZ_PTR(x) ((Lisp_Object *)(x))

#undef CHECK_LISP_OBJECT_TYPE
enum CHECK_LISP_OBJECT_TYPE { CHECK_LISP_OBJECT_TYPE = true };

void load_magic_refs (void);
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
const char *scheme_classify (Lisp_Object x);
void schedule_free (Lisp_Object x, void *data);
struct Lisp_Symbol *ensure_symbol_c_data (Lisp_Object symbol, Lisp_Object name);

extern uint64_t gdb_misc_val;
extern unsigned gdb_flags;
extern int gdb_hit_count;

#define SCHEME_FPTR_DEF(scheme_name, c_name)    \
  extern chez_ptr scheme_##c_name
#include "scheme_fptr.h"

#define SCHEME_FPTR_CALL0(c_name) chez_call0 (scheme_##c_name)
#define SCHEME_FPTR_CALL1(c_name, a1) chez_call1 (scheme_##c_name, a1)
#define SCHEME_FPTR_CALL2(c_name, a1, a2) chez_call2 (scheme_##c_name, a1, a2)
#define SCHEME_FPTR_CALL3(c_name, a1, a2, a3) chez_call3 (scheme_##c_name, a1, a2, a3)

extern chez_ptr scheme_special_vector_symbol;
extern chez_ptr scheme_vectorlike_symbol;
extern chez_ptr scheme_misc_symbol;
extern chez_ptr scheme_string_symbol;
extern chez_ptr error_result_symbol;
extern chez_iptr scheme_greatest_fixnum;
extern chez_iptr scheme_least_fixnum;
extern chez_iptr scheme_fixnum_width;
extern chez_ptr scheme_guardian;
extern chez_ptr analyze_guardian;
extern chez_ptr free_guardian;
extern chez_ptr buffer_guardian;
extern struct Scheme_Object_Header *first_scheme_object_header;
extern chez_ptr scheme_object_list;
extern chez_ptr scheme_intervals_map;

void link_scheme_obj (struct Scheme_Object_Header *soh, Lisp_Object val);

#define SCHEME_PV_TAG(pv) chez_vector_ref(pv, 0)
#define SCHEME_PV_SUBTAG(pv) chez_vector_ref(pv, 1)
#define SCHEME_PV_ADDR(pv) chez_vector_ref(pv, 2)
#define SCHEME_PV_GCVEC(pv) chez_vector_ref(pv, 3)
#define SCHEME_PV_TAG_SET(pv, x) chez_vector_set(pv, 0, x)
#define SCHEME_PV_SUBTAG_SET(pv, x) chez_vector_set(pv, 1, x)
#define SCHEME_PV_ADDR_SET(pv, x) chez_vector_set(pv, 2, x)
#define SCHEME_PV_GCVEC_SET(pv, x) chez_vector_set(pv, 3, x)
#define SCHEME_PV_LENGTH 4

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

bool inspect_scheme_ref (Lisp_Object ref,
                         Lisp_Object *ptr,
                         bool is_valid,
                         const char *label);
bool inspect_malloc_ptr (void *ptr,
                         const char *label);

INLINE void *
scheme_malloc_ptr(Lisp_Object addr) {
  gdb_misc_val = (uint64_t) CHEZ (addr);
  INSPECT_SCHEME_REF (addr, "scheme_malloc_ptr");

  SCHEME_ASSERT (50, chez_fixnump (CHEZ (addr)));
  void *data = (void *) chez_fixnum_value (CHEZ (addr));
  CHECK_ALLOC (data);
  return data;
}

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
#define ASSERT_GC_SUSPENDED()

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
#define LISP_ARRAY_PARAM(name, size_)                   \
  LISP_ARRAY_MISC(name, size_, array_param_record)


#ifdef HAVE_CHEZ_SCHEME
void walk_lisp_stack (void (*f)(void *, Lisp_Object *), void *);
void ensure_lisp_stack_capacity (size_t entries_to_add);

#ifdef SCHEME_DEBUG_STACK

struct Lisp_Var_Record {
  const char *func;
  Lisp_Object *data;
  size_t size;
};

extern struct Lisp_Var_Record *lisp_stack;
extern size_t lisp_stack_size;
extern size_t lisp_stack_capacity;

#define ENTER_LISP_FRAME_T(type, params, ...)                           \
  size_t this_old_lisp_stack_size = lisp_stack_size;                    \
  ensure_lisp_stack_capacity (PP_NARGS params + PP_NARGS (__VA_ARGS__)); \
  typedef type this_lisp_frame_type;                                    \
  (void) (this_lisp_frame_type *) 0;                                    \
  PP_MAP(LISP_LOCAL_VAR, __VA_ARGS__)                                   \
  LISP_LOCAL_MAP_ADDR params;                                           \
  LISP_LOCAL_MAP_ADDR(__VA_ARGS__)
#define LISP_LOCAL_ARRAY(name, size_)                                   \
  Lisp_Object name[size_];                                              \
  set_nil (name, size_);                                                \
  LISP_ARRAY_PARAM (name, size_)
#define LISP_DYNAMIC_ARRAY(name)                                        \
  Lisp_Object *name = NULL;                                             \
  USE_SAFE_ALLOCA
#define LISP_ARRAY_MISC(name, size_, record_name)                       \
  ensure_lisp_stack_capacity (1);                                       \
  lisp_stack[lisp_stack_size].func = __func__;                          \
  lisp_stack[lisp_stack_size].size = size_;                             \
  lisp_stack[lisp_stack_size].data = name;                              \
  lisp_stack_size++
#define UPDATE_LISP_DYNAMIC_ARRAY(name, size_)                          \
  do                                                                    \
    {                                                                   \
      set_nil (name, size_);                                            \
      LISP_ARRAY_PARAM (name, size_);                                   \
    }                                                                   \
  while (0)
#define ENTER_LISP_FRAME_VA_T(type, nargs, args, ...)                   \
  ENTER_LISP_FRAME_T (type __VA_OPT__(, __VA_ARGS__));                  \
  LISP_ARRAY_PARAM (args, nargs)
#define LISP_LOCAL_VAR(name) Lisp_Object name = Qnil;
#define LISP_LOCAL_ADDR(var)                                            \
  lisp_stack[lisp_stack_size].func = __func__;                          \
  lisp_stack[lisp_stack_size].size = 1;                                 \
  lisp_stack[lisp_stack_size].data = &var;                              \
  lisp_stack_size++;
#define LISP_LOCAL_MAP_ADDR(...)                        \
  __VA_OPT__(PP_MAP(LISP_LOCAL_ADDR, __VA_ARGS__))
#define EXIT_LISP_FRAME_NO_RETURN()                     \
  (lisp_stack_size = this_old_lisp_stack_size)
#define SAVE_LISP_FRAME_PTR()                                           \
  size_t saved_lisp_stack_size = lisp_stack_size
#define CHECK_LISP_FRAME_PTR()                                          \
  do                                                                    \
    {                                                                   \
      SCHEME_ASSERT (0, lisp_stack_size == saved_lisp_stack_size);      \
    }                                                                   \
  while (0)

#define RESTORE_LISP_FRAME_PTR()                                \
  do                                                            \
    {                                                           \
      lisp_stack_size = saved_lisp_stack_size;                  \
    }                                                           \
  while (0)


#else  // not SCHEME_DEBUG_STACK
struct Lisp_Frame_Layout {
#ifdef ENABLE_CHECKING
  const char *func_name;
#endif
  size_t num_locals;
  ptrdiff_t *offsets;
};

struct Lisp_Frame_Record {
#ifdef ENABLE_CHECKING
  const char *func_name;
#endif
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
    {LISP_FRAME_INFO_FUNC .prev = lisp_stack_ptr,                       \
     .layout = &this_lisp_frame_layout, .arrays = NULL};                \
  if (!is_this_lisp_frame_layout_init)                                  \
    {                                                                   \
      is_this_lisp_frame_layout_init = true;                            \
      int offset_index = 0;                                             \
      LISP_LOCAL_MAP_ADDR params;                                       \
      LISP_LOCAL_MAP_ADDR(__VA_ARGS__);                                 \
    }                                                                   \
  lisp_stack_ptr = &this_lisp_frame_record

// Declare a local array of Lisp_Object values of a given size.
// The contents of the array are initalized to Qnil.
#define LISP_LOCAL_ARRAY(name, size_)                                   \
  Lisp_Object name[size_];                                              \
  set_nil (name, size_);                                                \
  struct Lisp_Array_Record name##_array_record =                        \
    {.prev = this_lisp_frame_record.arrays,                             \
     .data = name, .size = size_};                                      \
  this_lisp_frame_record.arrays = &name##_array_record;

// Declare a pointer that will be used to hold a local array
// of Lisp_Object values.  The pointer is initalized to NULL.
#define LISP_DYNAMIC_ARRAY(name)                                        \
  Lisp_Object *name = NULL;                                             \
  struct Lisp_Array_Record name##_array_record =                        \
    {.prev = this_lisp_frame_record.arrays, .size = 0};                 \
  this_lisp_frame_record.arrays = &name##_array_record;                 \
  USE_SAFE_ALLOCA

// Register previously declared and initialized array of Lisp_Object
// values.
#define LISP_ARRAY_MISC(name, size_, record_name)                       \
  struct Lisp_Array_Record record_name =                                \
    {.prev = this_lisp_frame_record.arrays,                             \
     .data = name, .size = size_};                                      \
  this_lisp_frame_record.arrays = &record_name

// Record the size of a dynamic array previously declared with
// LISP_DYNAMIC_ARRAY, and initialize all members to the array to
// Qnil.  (Probably should only be used from SAFE_ALLOCA_LISP.)
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
#define SAVE_LISP_FRAME_PTR()                                           \
  struct Lisp_Frame_Record *saved_lisp_stack_ptr = lisp_stack_ptr
#define CHECK_LISP_FRAME_PTR()                                          \
  do                                                                    \
    {                                                                   \
      SCHEME_ASSERT (0, lisp_stack_ptr == saved_lisp_stack_ptr);        \
    }                                                                   \
  while (0)

#define RESTORE_LISP_FRAME_PTR()                                \
  do                                                            \
    {                                                           \
      lisp_stack_ptr = saved_lisp_stack_ptr;                    \
    }                                                           \
  while (0)
#endif  // not SCHEME_DEBUG_STACK

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
#define LISP_ARRAY_MISC(name, size_, record_name) ((void)0)
#define UPDATE_LISP_DYNAMIC_ARRAY(name, size_) ((void)0)
#define REGISTER_LISP_GLOBALS(...) ((void)0)
#define REGISTER_LISP_GLOBAL_ARRAY(...) ((void)0)

#endif

#ifdef HAVE_CHEZ_SCHEME
#define IS_SCHEME_REF(ref, num) (CHEZ (ref) == (void *)num)
#else
#define IS_SCHEME_REF(ref, num) false
#endif

// Define LOGF(level, fmt, ...) to call printf(fmt, ...) with a
// newline suffix iff ENABLE_CHECKING is defined and level >=
// LOGF_LEVEL.  By contention, LOGF_LEVEL should be between 1 and 50.
#ifdef ENABLE_CHECKING
#define LOGF_LEVEL 50
#define LOGF(level, fmt, ...)                           \
  ((void)((level) < LOGF_LEVEL ? 0 :                    \
          printf (fmt "\n" __VA_OPT__(, __VA_ARGS__))))
#else
#define LOGF(level, fmt, ...) ((void)0)
#endif

// Define TRACEF as an extension of LOGF that prefixes each message
// with the source file name and line number.  TRACEF may be called
// with only a single argument to log the source location and nothing
// else.
#define TRACEF(level, ...)                                              \
  LOGF (level, "%s:%d" __VA_OPT__(": " ARGS_CAR(__VA_ARGS__)),          \
        __FILE__, __LINE__ ARGS_COMMA_CDR(__VA_ARGS__))
#define ARGS_CAR(a0, ...) a0
#define ARGS_COMMA_CDR(a0, ...) __VA_OPT__(, __VA_ARGS__)
