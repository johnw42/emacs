//#include <stdio.h>

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
  LISP_LOCALS_SELECT (__VA_ARGS__ __VA_OPT__(,), LISP_LOCALS_ARGS_16, LISP_LOCALS_ARGS_15, LISP_LOCALS_ARGS_14, LISP_LOCALS_ARGS_13, LISP_LOCALS_ARGS_12, LISP_LOCALS_ARGS_11, LISP_LOCALS_ARGS_10, LISP_LOCALS_ARGS_9, LISP_LOCALS_ARGS_8, LISP_LOCALS_ARGS_7, LISP_LOCALS_ARGS_6, LISP_LOCALS_ARGS_5, LISP_LOCALS_ARGS_4, LISP_LOCALS_ARGS_3, LISP_LOCALS_ARGS_2, LISP_LOCALS_ARGS_1, LISP_LOCALS_ARGS_0) (__VA_ARGS__); \
  push_lisp_locals (false, PP_NARG(__VA_ARGS__), LISP_LOCALS_SELECT (__VA_ARGS__ __VA_OPT__(,), LISP_LOCALS_ARGS_16, LISP_LOCALS_ARGS_15, LISP_LOCALS_ARGS_14, LISP_LOCALS_ARGS_13, LISP_LOCALS_ARGS_12, LISP_LOCALS_ARGS_11, LISP_LOCALS_ARGS_10, LISP_LOCALS_ARGS_9, LISP_LOCALS_ARGS_8, LISP_LOCALS_ARGS_7, LISP_LOCALS_ARGS_6, LISP_LOCALS_ARGS_5, LISP_LOCALS_ARGS_4, LISP_LOCALS_ARGS_3, LISP_LOCALS_ARGS_2, LISP_LOCALS_ARGS_1, LISP_LOCALS_ARGS_0) (__VA_ARGS__))
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

#define LISP_LOCALS_ARGS_0(n1) ((void)0)
#define LISP_LOCALS_DECL_1(n1) Lisp_Object n1
#define LISP_LOCALS_ADDR_1(n1) &n1

void main()
{
  SCHEME_ENTER_LISP_FRAME();
  SCHEME_ENTER_LISP_FRAME(x);
  SCHEME_ENTER_LISP_FRAME(x, y);

  LISP_LOCALS(a);
  LISP_LOCALS(a, b);
  /* int *xp = x_ptr, *yp = y_ptr; */
  /* printf ("%d\n", PP_NARG(x, y)); */
}
