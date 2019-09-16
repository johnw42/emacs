// Only include via lisp.h!

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

#define SCHEME_FPTR_DEF(name, rtype, ...) \
  extern rtype (*scheme_fptr_##name)(const char *, int, __VA_ARGS__)
#include "scheme_fptr.h"

void do_chez_preamble (void);

#define SCHEME_FPTR_CALL(name, ...)                             \
  (last_scheme_function = #name,                                \
   last_scheme_call_file = __FILE__,                            \
   last_scheme_call_line = __LINE__,                            \
   do_chez_preamble(),                                          \
   (*scheme_fptr_##name)(__FILE__, __LINE__, __VA_ARGS__))

extern chez_ptr scheme_vectorlike_symbol;
extern chez_ptr scheme_misc_symbol;
extern chez_ptr scheme_string_symbol;
extern chez_iptr scheme_greatest_fixnum;
extern chez_iptr scheme_least_fixnum;
extern chez_iptr scheme_fixnum_width;
extern const char *last_scheme_function;
extern const char *last_scheme_call_file;
extern int last_scheme_call_line;
extern chez_ptr scheme_guardian;

#define SCHEME_PV_TAG(pv) chez_vector_ref(pv, 0)
#define SCHEME_PV_ADDR(pv) chez_vector_ref(pv, 1)
#define SCHEME_PV_EPHEMERON(pv) chez_vector_ref(pv, 2)
#define SCHEME_PV_ADDR_SET(pv, x) chez_vector_set(pv, 1, x)
#define SCHEME_PV_EPHEMERON_SET(pv, x) chez_vector_set(pv, 2, chez_false)  // XXX
#define SCHEME_PV_LENGTH 3

#define SCHEME_ALLOC_C_DATA(key, type) \
  ((type *)scheme_alloc_c_data((key), sizeof(type)))

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
bool mark_and_enqueue (Lisp_Object obj);

bool may_be_valid (chez_ptr x);
#endif

#define PP_NARG(...)                                         \
  PP_ARG_N(__VA_ARGS__,63,62,61,60,                          \
           59,58,57,56,55,54,53,52,51,50,                    \
           49,48,47,46,45,44,43,42,41,40,                    \
           39,38,37,36,35,34,33,32,31,30,                    \
           29,28,27,26,25,24,23,22,21,20,                    \
           19,18,17,16,15,14,13,12,11,10,                    \
           9,8,7,6,5,4,3,2,1,0)
#define PP_ARG_N( \
          _1, _2, _3, _4, _5, _6, _7, _8, _9,_10, \
         _11,_12,_13,_14,_15,_16,_17,_18,_19,_20, \
         _21,_22,_23,_24,_25,_26,_27,_28,_29,_30, \
         _31,_32,_33,_34,_35,_36,_37,_38,_39,_40, \
         _41,_42,_43,_44,_45,_46,_47,_48,_49,_50, \
         _51,_52,_53,_54,_55,_56,_57,_58,_59,_60, \
         _61,_62,_63,N,...) N

#define SCHEME_ENTER_LISP_FRAME(...)                                    \
  LISP_LOCALS_SELECT (__VA_ARGS__ __VA_OPT__(,), LISP_LOCALS_ARGS_16, LISP_LOCALS_ARGS_15, LISP_LOCALS_ARGS_14, LISP_LOCALS_ARGS_13, LISP_LOCALS_ARGS_12, LISP_LOCALS_ARGS_11, LISP_LOCALS_ARGS_10, LISP_LOCALS_ARGS_9, LISP_LOCALS_ARGS_8, LISP_LOCALS_ARGS_7, LISP_LOCALS_ARGS_6, LISP_LOCALS_ARGS_5, LISP_LOCALS_ARGS_4, LISP_LOCALS_ARGS_3, LISP_LOCALS_ARGS_2, LISP_LOCALS_ARGS_1, LISP_LOCALS_ARGS_0) (__VA_ARGS__)
#define LISP_LOCALS(...) \
  LISP_LOCALS_SELECT (__VA_ARGS__ __VA_OPT__(,), LISP_LOCALS_DECL_16, LISP_LOCALS_DECL_15, LISP_LOCALS_DECL_14, LISP_LOCALS_DECL_13, LISP_LOCALS_DECL_12, LISP_LOCALS_DECL_11, LISP_LOCALS_DECL_10, LISP_LOCALS_DECL_9, LISP_LOCALS_DECL_8, LISP_LOCALS_DECL_7, LISP_LOCALS_DECL_6, LISP_LOCALS_DECL_5, LISP_LOCALS_DECL_4, LISP_LOCALS_DECL_3, LISP_LOCALS_DECL_2, LISP_LOCALS_DECL_1, LISP_LOCALS_DECL_0) (__VA_ARGS__); \
  PUSH_LISP_LOCALS (false, PP_NARG(__VA_ARGS__), LISP_LOCALS_SELECT (__VA_ARGS__ __VA_OPT__(,), LISP_LOCALS_ADDR_16, LISP_LOCALS_ADDR_15, LISP_LOCALS_ADDR_14, LISP_LOCALS_ADDR_13, LISP_LOCALS_ADDR_12, LISP_LOCALS_ADDR_11, LISP_LOCALS_ADDR_10, LISP_LOCALS_ADDR_9, LISP_LOCALS_ADDR_8, LISP_LOCALS_ADDR_7, LISP_LOCALS_ADDR_6, LISP_LOCALS_ADDR_5, LISP_LOCALS_ADDR_4, LISP_LOCALS_ADDR_3, LISP_LOCALS_ADDR_2, LISP_LOCALS_ADDR_1, LISP_LOCALS_ADDR_0) (__VA_ARGS__))
#define LISP_LOCALS_SELECT(_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, macro, ...) macro
#define LISP_LOCALS_DECL_16(n1, n2, n3, n4, n5, n6, n7, n8, n9, n10, n11, n12, n13, n14, n15, n16) LISP_LOCALS_DECL_1(n1); LISP_LOCALS_DECL_15 (n2, n3, n4, n5, n6, n7, n8, n9, n10, n11, n12, n13, n14, n15, n16)
#define LISP_LOCALS_ADDR_16(n1, n2, n3, n4, n5, n6, n7, n8, n9, n10, n11, n12, n13, n14, n15, n16) LISP_LOCALS_ADDR_1(n1), LISP_LOCALS_ADDR_15 (n2, n3, n4, n5, n6, n7, n8, n9, n10, n11, n12, n13, n14, n15, n16)
#define LISP_LOCALS_DECL_15(n1, n2, n3, n4, n5, n6, n7, n8, n9, n10, n11, n12, n13, n14, n15) LISP_LOCALS_DECL_1(n1); LISP_LOCALS_DECL_14 (n2, n3, n4, n5, n6, n7, n8, n9, n10, n11, n12, n13, n14, n15)
#define LISP_LOCALS_ADDR_15(n1, n2, n3, n4, n5, n6, n7, n8, n9, n10, n11, n12, n13, n14, n15) LISP_LOCALS_ADDR_1(n1), LISP_LOCALS_ADDR_14 (n2, n3, n4, n5, n6, n7, n8, n9, n10, n11, n12, n13, n14, n15)
#define LISP_LOCALS_DECL_14(n1, n2, n3, n4, n5, n6, n7, n8, n9, n10, n11, n12, n13, n14) LISP_LOCALS_DECL_1(n1); LISP_LOCALS_DECL_13 (n2, n3, n4, n5, n6, n7, n8, n9, n10, n11, n12, n13, n14)
#define LISP_LOCALS_ADDR_14(n1, n2, n3, n4, n5, n6, n7, n8, n9, n10, n11, n12, n13, n14) LISP_LOCALS_ADDR_1(n1), LISP_LOCALS_ADDR_13 (n2, n3, n4, n5, n6, n7, n8, n9, n10, n11, n12, n13, n14)
#define LISP_LOCALS_DECL_13(n1, n2, n3, n4, n5, n6, n7, n8, n9, n10, n11, n12, n13) LISP_LOCALS_DECL_1(n1); LISP_LOCALS_DECL_12 (n2, n3, n4, n5, n6, n7, n8, n9, n10, n11, n12, n13)
#define LISP_LOCALS_ADDR_13(n1, n2, n3, n4, n5, n6, n7, n8, n9, n10, n11, n12, n13) LISP_LOCALS_ADDR_1(n1), LISP_LOCALS_ADDR_12 (n2, n3, n4, n5, n6, n7, n8, n9, n10, n11, n12, n13)
#define LISP_LOCALS_DECL_12(n1, n2, n3, n4, n5, n6, n7, n8, n9, n10, n11, n12) LISP_LOCALS_DECL_1(n1); LISP_LOCALS_DECL_11 (n2, n3, n4, n5, n6, n7, n8, n9, n10, n11, n12)
#define LISP_LOCALS_ADDR_12(n1, n2, n3, n4, n5, n6, n7, n8, n9, n10, n11, n12) LISP_LOCALS_ADDR_1(n1), LISP_LOCALS_ADDR_11 (n2, n3, n4, n5, n6, n7, n8, n9, n10, n11, n12)
#define LISP_LOCALS_DECL_11(n1, n2, n3, n4, n5, n6, n7, n8, n9, n10, n11) LISP_LOCALS_DECL_1(n1); LISP_LOCALS_DECL_10 (n2, n3, n4, n5, n6, n7, n8, n9, n10, n11)
#define LISP_LOCALS_ADDR_11(n1, n2, n3, n4, n5, n6, n7, n8, n9, n10, n11) LISP_LOCALS_ADDR_1(n1), LISP_LOCALS_ADDR_10 (n2, n3, n4, n5, n6, n7, n8, n9, n10, n11)
#define LISP_LOCALS_DECL_10(n1, n2, n3, n4, n5, n6, n7, n8, n9, n10) LISP_LOCALS_DECL_1(n1); LISP_LOCALS_DECL_9 (n2, n3, n4, n5, n6, n7, n8, n9, n10)
#define LISP_LOCALS_ADDR_10(n1, n2, n3, n4, n5, n6, n7, n8, n9, n10) LISP_LOCALS_ADDR_1(n1), LISP_LOCALS_ADDR_9 (n2, n3, n4, n5, n6, n7, n8, n9, n10)
#define LISP_LOCALS_DECL_9(n1, n2, n3, n4, n5, n6, n7, n8, n9) LISP_LOCALS_DECL_1(n1); LISP_LOCALS_DECL_8 (n2, n3, n4, n5, n6, n7, n8, n9)
#define LISP_LOCALS_ADDR_9(n1, n2, n3, n4, n5, n6, n7, n8, n9) LISP_LOCALS_ADDR_1(n1), LISP_LOCALS_ADDR_8 (n2, n3, n4, n5, n6, n7, n8, n9)
#define LISP_LOCALS_DECL_8(n1, n2, n3, n4, n5, n6, n7, n8) LISP_LOCALS_DECL_1(n1); LISP_LOCALS_DECL_7 (n2, n3, n4, n5, n6, n7, n8)
#define LISP_LOCALS_ADDR_8(n1, n2, n3, n4, n5, n6, n7, n8) LISP_LOCALS_ADDR_1(n1), LISP_LOCALS_ADDR_7 (n2, n3, n4, n5, n6, n7, n8)
#define LISP_LOCALS_DECL_7(n1, n2, n3, n4, n5, n6, n7) LISP_LOCALS_DECL_1(n1); LISP_LOCALS_DECL_6 (n2, n3, n4, n5, n6, n7)
#define LISP_LOCALS_ADDR_7(n1, n2, n3, n4, n5, n6, n7) LISP_LOCALS_ADDR_1(n1), LISP_LOCALS_ADDR_6 (n2, n3, n4, n5, n6, n7)
#define LISP_LOCALS_DECL_6(n1, n2, n3, n4, n5, n6) LISP_LOCALS_DECL_1(n1); LISP_LOCALS_DECL_5 (n2, n3, n4, n5, n6)
#define LISP_LOCALS_ADDR_6(n1, n2, n3, n4, n5, n6) LISP_LOCALS_ADDR_1(n1), LISP_LOCALS_ADDR_5 (n2, n3, n4, n5, n6)
#define LISP_LOCALS_DECL_5(n1, n2, n3, n4, n5) LISP_LOCALS_DECL_1(n1); LISP_LOCALS_DECL_4 (n2, n3, n4, n5)
#define LISP_LOCALS_ADDR_5(n1, n2, n3, n4, n5) LISP_LOCALS_ADDR_1(n1), LISP_LOCALS_ADDR_4 (n2, n3, n4, n5)
#define LISP_LOCALS_DECL_4(n1, n2, n3,  n4) LISP_LOCALS_DECL_1(n1); LISP_LOCALS_DECL_3 (n2, n3, n4)
#define LISP_LOCALS_ADDR_4(n1, n2, n3, n4) LISP_LOCALS_ADDR_1(n1), LISP_LOCALS_ADDR_3 (n2, n3, n4)
#define LISP_LOCALS_DECL_3(n1, n2, n3) LISP_LOCALS_DECL_1(n1); LISP_LOCALS_DECL_2 (n2, n3)
#define LISP_LOCALS_ADDR_3(n1, n2, n3) LISP_LOCALS_ADDR_1(n1), LISP_LOCALS_ADDR_2 (n2, n3)
#define LISP_LOCALS_DECL_2(n1, n2) LISP_LOCALS_DECL_1(n1); LISP_LOCALS_DECL_1 (n2)
#define LISP_LOCALS_ADDR_2(n1, n2) LISP_LOCALS_ADDR_1(n1), LISP_LOCALS_ADDR_1 (n2)
#define LISP_LOCALS_ARGS_16(n1, n2, n3, n4, n5, n6, n7, n8, n9, n10, n11, n12, n13, n14, n15, n16) push_lisp_locals(true, 16, LISP_LOCALS_ADDR_16 (n1, n2, n3, n4, n5, n6, n7, n8, n9, n10, n11, n12, n13, n14, n15, n16))
#define LISP_LOCALS_ARGS_15(n1, n2, n3, n4, n5, n6, n7, n8, n9, n10, n11, n12, n13, n14, n15) push_lisp_locals(true, 15, LISP_LOCALS_ADDR_15 (n1, n2, n3, n4, n5, n6, n7, n8, n9, n10, n11, n12, n13, n14, n15))
#define LISP_LOCALS_ARGS_14(n1, n2, n3, n4, n5, n6, n7, n8, n9, n10, n11, n12, n13, n14) push_lisp_locals(true, 14, LISP_LOCALS_ADDR_14 (n1, n2, n3, n4, n5, n6, n7, n8, n9, n10, n11, n12, n13, n14))
#define LISP_LOCALS_ARGS_13(n1, n2, n3, n4, n5, n6, n7, n8, n9, n10, n11, n12, n13) push_lisp_locals(true, 13, LISP_LOCALS_ADDR_13 (n1, n2, n3, n4, n5, n6, n7, n8, n9, n10, n11, n12, n13))
#define LISP_LOCALS_ARGS_12(n1, n2, n3, n4, n5, n6, n7, n8, n9, n10, n11, n12) push_lisp_locals(true, 12, LISP_LOCALS_ADDR_12 (n1, n2, n3, n4, n5, n6, n7, n8, n9, n10, n11, n12))
#define LISP_LOCALS_ARGS_11(n1, n2, n3, n4, n5, n6, n7, n8, n9, n10, n11) push_lisp_locals(true, 11, LISP_LOCALS_ADDR_11 (n1, n2, n3, n4, n5, n6, n7, n8, n9, n10, n11))
#define LISP_LOCALS_ARGS_10(n1, n2, n3, n4, n5, n6, n7, n8, n9, n10) push_lisp_locals(true, 10, LISP_LOCALS_ADDR_10 (n1, n2, n3, n4, n5, n6, n7, n8, n9, n10))
#define LISP_LOCALS_ARGS_9(n1, n2, n3, n4, n5, n6, n7, n8, n9) push_lisp_locals(true, 9, LISP_LOCALS_ADDR_9 (n1, n2, n3, n4, n5, n6, n7, n8, n9))
#define LISP_LOCALS_ARGS_8(n1, n2, n3, n4, n5, n6, n7, n8) push_lisp_locals(true, 8, LISP_LOCALS_ADDR_8 (n1, n2, n3, n4, n5, n6, n7, n8))
#define LISP_LOCALS_ARGS_7(n1, n2, n3, n4, n5, n6, n7) push_lisp_locals(true, 7, LISP_LOCALS_ADDR_7 (n1, n2, n3, n4, n5, n6, n7))
#define LISP_LOCALS_ARGS_6(n1, n2, n3, n4, n5, n6) push_lisp_locals(true, 6, LISP_LOCALS_ADDR_6 (n1, n2, n3, n4, n5, n6))
#define LISP_LOCALS_ARGS_5(n1, n2, n3, n4, n5) push_lisp_locals(true, 5, LISP_LOCALS_ADDR_5 (n1, n2, n3, n4, n5))
#define LISP_LOCALS_ARGS_4(n1, n2, n3, n4) push_lisp_locals(true, 4, LISP_LOCALS_ADDR_4 (n1, n2, n3, n4))
#define LISP_LOCALS_ARGS_3(n1, n2, n3) push_lisp_locals(true, 3, LISP_LOCALS_ADDR_3 (n1, n2, n3))
#define LISP_LOCALS_ARGS_2(n1, n2) push_lisp_locals(true, 2, LISP_LOCALS_ADDR_2 (n1, n2))
#define LISP_LOCALS_ARGS_1(n1) push_lisp_locals(true, 1, LISP_LOCALS_ADDR_1 (n1))

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

extern ptrdiff_t lisp_frame_record_count;

union lisp_frame_record {
  ptrdiff_t count;
  Lisp_Object *ptr;
  void *sentinel;
};

void push_lisp_locals(bool already_initialized, int n, ...);
void push_lisp_local_array(bool already_initialized, Lisp_Object *ptr, ptrdiff_t n);
void pop_lisp_locals(int n);
bool walk_lisp_frame_records (ptrdiff_t *pos,
                              union lisp_frame_record **ptr,
                              ptrdiff_t *count);
#endif

#ifdef HAVE_CHEZ_SCHEME
#define ENTER_LISP_FRAME_T(type, ...)                                   \
  typedef type this_lisp_frame_type;                                    \
  ptrdiff_t orig_lisp_frame_record_count = lisp_frame_record_count;     \
  SCHEME_ENTER_LISP_FRAME(__VA_ARGS__)
#define ENTER_LISP_FRAME_VA_T(type, nargs, args, ...)                    \
  typedef type this_lisp_frame_type;                                    \
  ptrdiff_t orig_lisp_frame_record_count = lisp_frame_record_count;     \
  push_lisp_local_array(true, args, nargs)                              \
  __VA_OPT__(; SCHEME_ENTER_LISP_FRAME (__VA_ARGS__))
#define EXIT_LISP_FRAME_NO_RETURN()                             \
  do                                                            \
    {                                                           \
      ((void)(this_lisp_frame_type *)0);                        \
      lisp_frame_record_count = orig_lisp_frame_record_count;   \
    }                                                           \
  while (0)
#define PUSH_LISP_LOCALS(...) push_lisp_locals (__VA_ARGS__)
#define LISP_LOCALS_ARGS_0(n1) ((void)0)
#define LISP_LOCALS_DECL_1(n1) Lisp_Object n1
#define LISP_LOCALS_ADDR_1(n1) &n1
#define LISP_LOCAL_ARRAY(name, size) \
  Lisp_Object name[size];            \
  push_lisp_local_array(false, name, size)
#define LISP_LOCAL_ALLOCA(name, size)           \
  ptrdiff_t name##_size = size;                 \
  Lisp_Object *name;                            \
  SAFE_ALLOCA_LISP(name, name##_size);          \
  push_lisp_local_array(false, name, name##_size)
#else
#define PUSH_LISP_LOCALS(...) ((void)0)
#define ENTER_LISP_FRAME_T(type, ...) typedef type this_lisp_frame_type
#define ENTER_LISP_FRAME_VA_T(...) ((void)0)
#define EXIT_LISP_FRAME_NO_RETURN() ((void)(this_lisp_frame_type *)0)
#define LISP_LOCALS_DECL_1(n1) Lisp_Object n1
#define LISP_LOCALS_ADDR_1(n1) ((void)0)
#define LISP_LOCAL_ARRAY(name, size) Lisp_Object name[size]
#define LISP_LOCAL_ALLOCA(name, size) \
  Lisp_Object *name;                  \
  SAFE_ALLOCA_LISP(name, size)
#endif

#ifdef HAVE_CHEZ_SCHEME
#define IS_SCHEME_REF(ref, num) (CHEZ (ref) == (void *)num)
#else
#define IS_SCHEME_REF(ref, num) false
#endif
