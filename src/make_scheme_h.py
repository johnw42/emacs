#! /usr/bin/env python3
import sys
import re


def RemoveArgName(arg):
    m = re.match(r"^(.+[ *])[a-z]+$", arg)
    if m:
        return m[1]
    m = re.match("(.+)[a-z]+(\[+)", arg)
    if m:
        return m[1] + m[2]
    m = re.match(r"(.*\(\*)[a-z]*(\).*)", arg)
    if m:
        return m[1] + m[2]
    return arg


def MakeArgDecl(atype, name):
    i = atype.find("(*)")
    if i != -1:
        return atype[: i + 2] + name + atype[i + 2 :]
    i = atype.find("[")
    if i != -1:
        return atype[:i] + name + atype[i:]
    return atype + " " + name


def Main():
    extra_lines = []
    for line in sys.stdin:
        line = re.sub(r"\b([iu]?ptr)\b", r"chez_\1", line.rstrip("\n"))
        m = re.match(r"#define S(.*)", line)
        if m:
            line = "#define chez_" + re.sub(r"\bS", "chez_", m[1])
        print(line)
        m = re.match("^EXPORT (.*)S([a-z][a-z0-9_]+) PROTO\(\((.*)\)\);$", line)
        if m:
            rtype = m[1]
            name = m[2]
            arg_types = list(map(RemoveArgName, re.split(r", *", m[3])))
            if arg_types == ["void"]:
                arg_names = []
                arg_decls = ["void"]
            else:
                arg_names = [f"arg{i}" for i in range(len(arg_types))]
                arg_decls = [
                    MakeArgDecl(atype, name)
                    for atype, name in zip(arg_types, arg_names)
                ]
            comma = ", "
            ret = "" if re.match(r"void *", rtype) else "return "
            print(
                f"""\
INLINE {rtype}chez_{name} ({comma.join(arg_decls)}) {{
  CHEZ_PREAMBLE;
  {ret}S{name}({comma.join(arg_names)});
}}"""
            )
            extra_lines += [f"#define S{name} emacs_S{name}"]
    for line in extra_lines:
        print(line)


Main()
