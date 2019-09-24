SCHEME_FPTR_DEF(save_pointer, int, void *, const char *);
SCHEME_FPTR_DEF(check_pointer, int, void *, const char *);
SCHEME_FPTR_DEF(hashtablep, int, chez_ptr);
SCHEME_FPTR_DEF(print_to_bytevector, chez_ptr, chez_ptr);
SCHEME_FPTR_DEF(save_origin, int /*void*/, chez_ptr);
SCHEME_FPTR_DEF(print_origin, int /*void*/, chez_ptr);
SCHEME_FPTR_DEF(eq_hash, uint32_t, chez_ptr);
SCHEME_FPTR_DEF(hashtable_values, chez_ptr, chez_ptr);
SCHEME_FPTR_DEF(hashtable_ref, chez_ptr, chez_ptr, chez_ptr, chez_ptr);
SCHEME_FPTR_DEF(symbol_is, int, chez_ptr, const char *);
SCHEME_FPTR_DEF(trivial, int, int);
SCHEME_FPTR_DEF(ephemeron_cons, chez_ptr, chez_ptr, chez_ptr);
SCHEME_FPTR_DEF(getprop_addr, void *, chez_ptr, chez_ptr);
SCHEME_FPTR_DEF(putprop_addr, int /*void*/, chez_ptr, chez_ptr, void *);
#undef SCHEME_FPTR_DEF
