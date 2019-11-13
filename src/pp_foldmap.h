#ifndef EMACS_PP_FOLDMAP_H
#define EMACS_PP_FOLDMAP_H

// Based on ideas from https://stackoverflow.com/a/10526117 and
// https://stackoverflow.com/a/2124385.

// A macro that transforms each of its varargs with the one-argument
// macro F and combines the results with two-argument macro G.  With a
// single vararg A, expands to f(A), and with no varargs, expands to Z.
#define PP_FOLDMAP(G, F, Z, ...) \
  PP_CAT(PP_FOLDMAP_, PP_NARGS(__VA_ARGS__)) (G, F, Z, __VA_ARGS__)

// Expands to F(A_1) F(A_2) ... F(A_n) for each of the n varargs A_i.
// With no varargs, the expansion is empty.
#define PP_MAP(F, ...) \
  PP_FOLDMAP(PP_JUXT, F, PP_EMPTY, ## __VA_ARGS__)

#define PP_EMPTY

// Expands to F(A_1), F(A_2), ..., F(A_n) for each of the n varargs A_i.
// With no varargs, the expansion is empty.
#define PP_MAP_COMMA(F, ...) \
  PP_FOLDMAP(PP_COMMA, F, , __VA_ARGS__)

// Expands to G(A_1, G(A_2, G(... G(A_n-1, A_n)))) for each of the n
// varargs A_i.  If n = 1, expands to A_1.  If n = 0, expands to Z.
#define PP_FOLDR(G, Z, ...)                              \
  PP_FOLDMAP(G, PP_ARG, Z , ## __VA_ARGS__)

// =================
//   Helper Macros
// =================

// A macro that returns the number of arguments it is given.
#define PP_NARGS(...)                                   \
  PP_NARGS_HELPER(, ## __VA_ARGS__,63,62,61,60,            \
                  59,58,57,56,55,54,53,52,51,50,        \
                  49,48,47,46,45,44,43,42,41,40,        \
                  39,38,37,36,35,34,33,32,31,30,        \
                  29,28,27,26,25,24,23,22,21,20,        \
                  19,18,17,16,15,14,13,12,11,10,        \
                  9,8,7,6,5,4,3,2,1,0)
#define PP_NARGS_HELPER(_0,                       \
          _1, _2, _3, _4, _5, _6, _7, _8, _9,_10, \
         _11,_12,_13,_14,_15,_16,_17,_18,_19,_20, \
         _21,_22,_23,_24,_25,_26,_27,_28,_29,_30, \
         _31,_32,_33,_34,_35,_36,_37,_38,_39,_40, \
         _41,_42,_43,_44,_45,_46,_47,_48,_49,_50, \
         _51,_52,_53,_54,_55,_56,_57,_58,_59,_60, \
         _61,_62,_63,N,...) N

#define PP_FOLDMAP_0(G, F, Z, dummy) Z
#define PP_FOLDMAP_1(G, F, Z, a1) F(a1)
#define PP_FOLDMAP_2(G, F, Z, a1, a2) G(F(a1), F(a2))
#define PP_FOLDMAP_3(G, F, Z, a1, ...) G(F(a1), PP_FOLDMAP_2(G, F, Z, __VA_ARGS__))
#define PP_FOLDMAP_4(G, F, Z, a1, ...) G(F(a1), PP_FOLDMAP_3(G, F, Z, __VA_ARGS__))
#define PP_FOLDMAP_5(G, F, Z, a1, ...) G(F(a1), PP_FOLDMAP_4(G, F, Z, __VA_ARGS__))
#define PP_FOLDMAP_6(G, F, Z, a1, ...) G(F(a1), PP_FOLDMAP_5(G, F, Z, __VA_ARGS__))
#define PP_FOLDMAP_7(G, F, Z, a1, ...) G(F(a1), PP_FOLDMAP_6(G, F, Z, __VA_ARGS__))
#define PP_FOLDMAP_8(G, F, Z, a1, ...) G(F(a1), PP_FOLDMAP_7(G, F, Z, __VA_ARGS__))
#define PP_FOLDMAP_9(G, F, Z, a1, ...) G(F(a1), PP_FOLDMAP_8(G, F, Z, __VA_ARGS__))
#define PP_FOLDMAP_10(G, F, Z, a1, ...) G(F(a1), PP_FOLDMAP_9(G, F, Z, __VA_ARGS__))
#define PP_FOLDMAP_11(G, F, Z, a1, ...) G(F(a1), PP_FOLDMAP_10(G, F, Z, __VA_ARGS__))
#define PP_FOLDMAP_12(G, F, Z, a1, ...) G(F(a1), PP_FOLDMAP_11(G, F, Z, __VA_ARGS__))
#define PP_FOLDMAP_13(G, F, Z, a1, ...) G(F(a1), PP_FOLDMAP_12(G, F, Z, __VA_ARGS__))
#define PP_FOLDMAP_14(G, F, Z, a1, ...) G(F(a1), PP_FOLDMAP_13(G, F, Z, __VA_ARGS__))
#define PP_FOLDMAP_15(G, F, Z, a1, ...) G(F(a1), PP_FOLDMAP_14(G, F, Z, __VA_ARGS__))
#define PP_FOLDMAP_16(G, F, Z, a1, ...) G(F(a1), PP_FOLDMAP_15(G, F, Z, __VA_ARGS__))
#define PP_FOLDMAP_17(G, F, Z, a1, ...) G(F(a1), PP_FOLDMAP_16(G, F, Z, __VA_ARGS__))
#define PP_FOLDMAP_18(G, F, Z, a1, ...) G(F(a1), PP_FOLDMAP_17(G, F, Z, __VA_ARGS__))
#define PP_FOLDMAP_19(G, F, Z, a1, ...) G(F(a1), PP_FOLDMAP_18(G, F, Z, __VA_ARGS__))
#define PP_FOLDMAP_20(G, F, Z, a1, ...) G(F(a1), PP_FOLDMAP_19(G, F, Z, __VA_ARGS__))
#define PP_FOLDMAP_21(G, F, Z, a1, ...) G(F(a1), PP_FOLDMAP_20(G, F, Z, __VA_ARGS__))
#define PP_FOLDMAP_22(G, F, Z, a1, ...) G(F(a1), PP_FOLDMAP_21(G, F, Z, __VA_ARGS__))
#define PP_FOLDMAP_23(G, F, Z, a1, ...) G(F(a1), PP_FOLDMAP_22(G, F, Z, __VA_ARGS__))
#define PP_FOLDMAP_24(G, F, Z, a1, ...) G(F(a1), PP_FOLDMAP_23(G, F, Z, __VA_ARGS__))
#define PP_FOLDMAP_25(G, F, Z, a1, ...) G(F(a1), PP_FOLDMAP_24(G, F, Z, __VA_ARGS__))
#define PP_FOLDMAP_26(G, F, Z, a1, ...) G(F(a1), PP_FOLDMAP_25(G, F, Z, __VA_ARGS__))
#define PP_FOLDMAP_27(G, F, Z, a1, ...) G(F(a1), PP_FOLDMAP_26(G, F, Z, __VA_ARGS__))
#define PP_FOLDMAP_28(G, F, Z, a1, ...) G(F(a1), PP_FOLDMAP_27(G, F, Z, __VA_ARGS__))
#define PP_FOLDMAP_29(G, F, Z, a1, ...) G(F(a1), PP_FOLDMAP_28(G, F, Z, __VA_ARGS__))
#define PP_FOLDMAP_30(G, F, Z, a1, ...) G(F(a1), PP_FOLDMAP_29(G, F, Z, __VA_ARGS__))
#define PP_FOLDMAP_31(G, F, Z, a1, ...) G(F(a1), PP_FOLDMAP_30(G, F, Z, __VA_ARGS__))
#define PP_FOLDMAP_32(G, F, Z, a1, ...) G(F(a1), PP_FOLDMAP_31(G, F, Z, __VA_ARGS__))
#define PP_FOLDMAP_33(G, F, Z, a1, ...) G(F(a1), PP_FOLDMAP_32(G, F, Z, __VA_ARGS__))
#define PP_FOLDMAP_34(G, F, Z, a1, ...) G(F(a1), PP_FOLDMAP_33(G, F, Z, __VA_ARGS__))
#define PP_FOLDMAP_35(G, F, Z, a1, ...) G(F(a1), PP_FOLDMAP_34(G, F, Z, __VA_ARGS__))
#define PP_FOLDMAP_36(G, F, Z, a1, ...) G(F(a1), PP_FOLDMAP_35(G, F, Z, __VA_ARGS__))
#define PP_FOLDMAP_37(G, F, Z, a1, ...) G(F(a1), PP_FOLDMAP_36(G, F, Z, __VA_ARGS__))
#define PP_FOLDMAP_38(G, F, Z, a1, ...) G(F(a1), PP_FOLDMAP_37(G, F, Z, __VA_ARGS__))
#define PP_FOLDMAP_39(G, F, Z, a1, ...) G(F(a1), PP_FOLDMAP_38(G, F, Z, __VA_ARGS__))
#define PP_FOLDMAP_40(G, F, Z, a1, ...) G(F(a1), PP_FOLDMAP_39(G, F, Z, __VA_ARGS__))
#define PP_FOLDMAP_41(G, F, Z, a1, ...) G(F(a1), PP_FOLDMAP_40(G, F, Z, __VA_ARGS__))
#define PP_FOLDMAP_42(G, F, Z, a1, ...) G(F(a1), PP_FOLDMAP_41(G, F, Z, __VA_ARGS__))
#define PP_FOLDMAP_43(G, F, Z, a1, ...) G(F(a1), PP_FOLDMAP_42(G, F, Z, __VA_ARGS__))
#define PP_FOLDMAP_44(G, F, Z, a1, ...) G(F(a1), PP_FOLDMAP_43(G, F, Z, __VA_ARGS__))
#define PP_FOLDMAP_45(G, F, Z, a1, ...) G(F(a1), PP_FOLDMAP_44(G, F, Z, __VA_ARGS__))
#define PP_FOLDMAP_46(G, F, Z, a1, ...) G(F(a1), PP_FOLDMAP_45(G, F, Z, __VA_ARGS__))
#define PP_FOLDMAP_47(G, F, Z, a1, ...) G(F(a1), PP_FOLDMAP_46(G, F, Z, __VA_ARGS__))
#define PP_FOLDMAP_48(G, F, Z, a1, ...) G(F(a1), PP_FOLDMAP_47(G, F, Z, __VA_ARGS__))
#define PP_FOLDMAP_49(G, F, Z, a1, ...) G(F(a1), PP_FOLDMAP_48(G, F, Z, __VA_ARGS__))
#define PP_FOLDMAP_50(G, F, Z, a1, ...) G(F(a1), PP_FOLDMAP_49(G, F, Z, __VA_ARGS__))
#define PP_FOLDMAP_51(G, F, Z, a1, ...) G(F(a1), PP_FOLDMAP_50(G, F, Z, __VA_ARGS__))
#define PP_FOLDMAP_52(G, F, Z, a1, ...) G(F(a1), PP_FOLDMAP_51(G, F, Z, __VA_ARGS__))
#define PP_FOLDMAP_53(G, F, Z, a1, ...) G(F(a1), PP_FOLDMAP_52(G, F, Z, __VA_ARGS__))
#define PP_FOLDMAP_54(G, F, Z, a1, ...) G(F(a1), PP_FOLDMAP_53(G, F, Z, __VA_ARGS__))
#define PP_FOLDMAP_55(G, F, Z, a1, ...) G(F(a1), PP_FOLDMAP_54(G, F, Z, __VA_ARGS__))
#define PP_FOLDMAP_56(G, F, Z, a1, ...) G(F(a1), PP_FOLDMAP_55(G, F, Z, __VA_ARGS__))
#define PP_FOLDMAP_57(G, F, Z, a1, ...) G(F(a1), PP_FOLDMAP_56(G, F, Z, __VA_ARGS__))
#define PP_FOLDMAP_58(G, F, Z, a1, ...) G(F(a1), PP_FOLDMAP_57(G, F, Z, __VA_ARGS__))
#define PP_FOLDMAP_59(G, F, Z, a1, ...) G(F(a1), PP_FOLDMAP_58(G, F, Z, __VA_ARGS__))
#define PP_FOLDMAP_60(G, F, Z, a1, ...) G(F(a1), PP_FOLDMAP_59(G, F, Z, __VA_ARGS__))
#define PP_FOLDMAP_61(G, F, Z, a1, ...) G(F(a1), PP_FOLDMAP_60(G, F, Z, __VA_ARGS__))
#define PP_FOLDMAP_62(G, F, Z, a1, ...) G(F(a1), PP_FOLDMAP_61(G, F, Z, __VA_ARGS__))
#define PP_FOLDMAP_63(G, F, Z, a1, ...) G(F(a1), PP_FOLDMAP_62(G, F, Z, __VA_ARGS__))

// Expands the expansion of A concatenated with the expansion of B.
#define PP_CAT(A, B) PP_CAT_HELPER(A, B)
#define PP_CAT_HELPER(A, B) A ## B

#define PP_ARG(x) x
#define PP_JUXT(x, y) x y
#define PP_COMMA(x, y) x, y

#endif
