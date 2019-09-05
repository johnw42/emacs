// Only include via lisp.h!

#ifdef HAVE_CHEZ_SCHEME
#include "chez_scheme.h"
#endif

INLINE_HEADER_BEGIN

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

extern unsigned gdb_flags;
extern int gdb_hit_count;

#define SCHEME_FPTR_DEF(name, rtype, ...) \
  extern rtype (*scheme_fptr_##name)(const char *, int, __VA_ARGS__)
#include "scheme_fptr.h"

#define SCHEME_FPTR_CALL(name, ...)                             \
  (last_scheme_function = #name,                                \
   last_scheme_call_file = __FILE__,                            \
   last_scheme_call_line = __LINE__,                            \
   (*scheme_fptr_##name)(__FILE__, __LINE__, __VA_ARGS__))

extern Lisp_Object scheme_vectorlike_symbol;
extern Lisp_Object scheme_misc_symbol;
extern Lisp_Object scheme_string_symbol;
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
#define SCHEME_PV_EPHEMERON_SET(pv, x) chez_vector_set(pv, 2, x)
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
  void *data = (void *) chez_integer_value (CHEZ (addr));
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

struct container {
  void *data;
  size_t size;
  size_t capacity;
  size_t elem_size;
  int (*compare)(const void *, const void *);
  bool is_sorted;
};

#define FOR_CONTAINER(i, c) for (size_t i = 0; i < (c)->size; i++)

#define CONTAINER_REF(type, c, i) \
  (eassume ((c)->elem_size == sizeof (type)), ((type *) (c)->data) + (i))

INLINE void *
container_ref (struct container *c, size_t i)
{
  return (char *) c->data + i * c->elem_size;
}

INLINE void
container_config (struct container *c, size_t elem_size, int (*compare)(const void *, const void *))
{
  c->elem_size = elem_size;
  c->compare = compare;
}

INLINE void
container_init (struct container *c, size_t elem_size, int (*compare)(const void *, const void *))
{
  c->data = NULL;
  c->size = 0;
  c->capacity = 0;
  c->is_sorted = false;
  container_config(c, elem_size, compare);
}

INLINE void
container_free (struct container *c)
{
  free (c->data);
  c->size = 0;
  c->capacity = 0;
}

INLINE void
container_reset (struct container *c)
{
  c->size = 0;
}

INLINE void
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

INLINE void
container_sort (struct container *c)
{
  if (!c->is_sorted)
    qsort (c->data, c->size, c->elem_size, c->compare);
}

INLINE void *
container_search (struct container *c, const void *key)
{
  eassert (c->compare);

  if (c->is_sorted)
    return bsearch (key, c->data, c->size, c->elem_size, c->compare);

  FOR_CONTAINER (i, c)
    {
      void *found = container_ref (c, i);
      if (c->compare (key, found) == 0)
        return found;
    }
  return NULL;
}

INLINE void
container_append (struct container *c, void *item)
{
  container_reserve (c, c->size + 1);
  char *dest = container_ref (c, c->size);
  memcpy (dest,
          item, c->elem_size);
  if (c->size == 0)
    c->is_sorted = true;
  else if (c->is_sorted && c->compare)
    {
      if (c->compare (container_ref (c, c->size - 1), dest) > 0)
        c->is_sorted = false;
    }
  c->size++;
}

INLINE void
container_delete_if (struct container *c, bool (*pred)(const void *))
{
  size_t j = 0;
  FOR_CONTAINER (i, c)
    {
      eassert (j <= i);
      if (pred (container_ref (c, i)))
        {
          if (i != j)
            {
              memcpy (container_ref (c, j),
                      container_ref (c, i),
                      c->elem_size);
            }
          j++;
        }
    }
  c->size = j;
}

INLINE void
container_uniq (struct container *c)
{
  container_sort (c);

  size_t j = 0;
  FOR_CONTAINER (i, c)
    if (i > 0)
      {
        eassert (j <= i);
        if (c->compare (container_ref (c, j),
                        container_ref (c, i)) != 0)
          {
            j++;
            if (i != j)
              {
                memcpy (container_ref (c, j),
                        container_ref (c, i),
                        c->elem_size);
              }
          }
      }
}

#define NAMED_CONTAINER_DECL(name, type)                                \
  struct container name;                                                \
  typedef type name##_type

#define ASSERT_TYPE(type, x) (true ? (type)(x) : (x))
#define EXTERN_NAMED_CONTAINER(name, type) extern NAMED_CONTAINER_DECL (name, type)
#define STATIC_NAMED_CONTAINER(name, type) static NAMED_CONTAINER_DECL (name, type)
#define NAMED_CONTAINER_REF(name, i) CONTAINER_REF (name##_type, &name, i)
#define NAMED_CONTAINER_APPEND(name, val)                       \
  (name.elem_size = sizeof (name##_type), \
   container_append (&name, ASSERT_TYPE (name##_type *, val)))
#define NAMED_CONTAINER_CONFIG(name, cmp)                               \
  container_config (&name, sizeof (name##_type), cmp)
#define FOR_NAMED_CONTAINER(i, name) FOR_CONTAINER (i, &name)

void mark_lisp_refs (void);
bool mark_and_enqueue (Lisp_Object obj);

#define IS_SCHEME_REF(ref, num) (CHEZ (ref) == (void *)num)
#define IS_MAGIC_SCHEME_REF(p) \
  (false ||                         \
   IS_SCHEME_REF (p, 0x4165a19f) || \
   IS_SCHEME_REF (p, 0x4165a1cb) || \
   false)
//#define IS_MAGIC_SCHEME_REF_ADDR(p) // false ((chez_ptr *)0x7fffffffcdc8)

#endif /* not HAVE_CHEZ_SCHEME */
