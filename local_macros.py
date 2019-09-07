#! /usr/bin/env python3

MAX_LOCALS = 16
MAX_ARGS = MAX_LOCALS


def Main():
    p = "LISP_LOCALS"
    tok_seps = [("DECL", ";"), ("ADDR", ",")]
    nums = range(1, MAX_LOCALS + 1)
    macros = lambda tok: ", ".join(f"{p}_{tok}_{i}" for i in reversed(nums))
    print(
        f"""\
#define SCHEME_ENTER_LISP_FRAME(...) \\
    {p}_SELECT (__VA_ARGS__, {macros('ARGS')}) (__VA_ARGS__)
#define {p}(...) \\
    {p}_SELECT (__VA_ARGS__, {macros('DECL')}) (__VA_ARGS__); \\
    push_lisp_locals (false, PP_NARG(__VA_ARGS__), {p}_SELECT(__VA_ARGS__, {macros('ADDR')})(__VA_ARGS__))"""
    )
    # for tok, _ in tok_seps:
    #     macros = ", ".join(f"{p}_{tok}_{i}" for i in reversed(nums))
    #     print(f"#define {p}_{tok}_MACROS {macros}")
    dummy_args = ", ".join(f"_{i}" for i in nums)
    print(f"#define {p}_SELECT({dummy_args}, macro, ...) macro")
    for n in reversed(nums[1:]):
        args = ", ".join(f"n{i}" for i in range(2, n + 1))
        for tok, sep in tok_seps:
            print(
                f"""\
#define {p}_{tok}_{n}(n1, {args}) \
{p}_{tok}_1(n1){sep} {p}_{tok}_{n-1} ({args})"""
            )
    nums = range(1, MAX_ARGS + 1)
    for n in reversed(nums):
        args = ", ".join(f"n{i}" for i in range(1, n + 1))
        print(
            f"""\
#define {p}_ARGS_{n}({args}) \
push_lisp_locals(true, {n}, {p}_ADDR_{n} ({args}))"""
        )


Main()
