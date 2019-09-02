#! /usr/bin/env python3

import os
import re
import sys
import subprocess
import glob

g_changes_made = False


class Updater:
    def __init__(self, filename, line_index):
        self.filename = filename
        self.line_index = line_index
        with open(filename) as f:
            self.lines = f.readlines()
        m = re.match(r"^(\s*)([^\n]+)\n?$", self.lines[line_index])
        self.indent = m[1]
        self.line = self.old_line = m[2]

    def __enter__(self):
        return self

    def __call__(self, new_line):
        assert "\n" not in new_line
        self.lines[self.line_index] = new_line + "\n"

    def __exit__(self, x, *_):
        if x:
            return

        if self.old_line == self.line:
            return

        global g_changes_made
        g_changes_made = True

        print(f"Updating {self.filename}:{self.line_index+1}:")
        print(f"- {self.old_line}")

        new_line = self.line
        assert "\n" not in new_line
        self.lines[self.line_index] = self.indent + new_line + "\n"

        print(f"+ {new_line}")

        if len(sys.argv) == 1:
            with open(self.filename, "w") as f:
                f.writelines(self.lines)


def ExprEnd(line, pos):
    assert "//" not in line
    assert "/*" not in line
    assert '"' not in line
    assert "'" not in line
    stack = 0
    i = pos
    while i < len(line):
        c = line[i]
        if c in "([{":
            stack += 1
            i += 1
        elif c in "}])":
            stack -= 1
            i += 1
        elif stack > 0:
            i += 1
        else:
            m = re.match("[A-Za-z0-9_ ]+", line[i:])
            if not m:
                break
            i += m.end(0)
    if stack == 0 and i > pos and not line[pos:i].isspace():
        # while i > pos and line[i].isspace():
        #     i -= 1
        return i
    else:
        return None


def ExprBefore(line, pos):
    start = None
    end = 0
    for i in range(0, pos):
        next_end = ExprEnd(line, i)
        # print(i, repr(line[i:next_end]))
        if next_end is not None and next_end > end and next_end < pos:
            start = i
            end = next_end
            # print("end =", end)
    return start, end


class Matcher:
    def __init__(self, line):
        self.line = line
        self.match = None

    def __call__(self, pat):
        self.match = re.match(pat, self.line)
        return bool(self.match)

    def __getattr__(self, attr):
        return getattr(self.match, attr)

    def __getitem__(self, i):
        return self.match[i]

    def __len__(self):
        return len(self.match)


class ErrMatcher(Matcher):
    def __init__(self, line):
        m = re.match(r"^([^:]+):(\d+):(\d+): (?:error|warning): ", line)
        if m:
            self.err_file = m[1]
            self.err_line = int(m[2]) - 1
            self.err_column = int(m[3]) - 1
            line = line[m.end(0) :]
        else:
            line = ""
        Matcher.__init__(self, line)

    def Update(self):
        return Updater(self.err_file, self.err_line)


class CodeMatcher(Matcher):
    def __init__(self, line):
        m = re.match(r"\s*", line)
        line = line[m.end(0) :]
        Matcher.__init__(self, line)

    def __call__(self, pat):
        return Matcher.__call__(self, pat) and self.end(0) == len(self.line)


arg_rx_cache = {}


def ArgRx(depth, single_arg):
    key = (depth, single_arg)
    found = arg_rx_cache.get(key)
    if found is None:
        if depth == 0:
            return r"[^=,;()]*" if single_arg else r"[^;()]*"
        subn = ArgRx(depth - 1, False)
        sub1 = ArgRx(depth - 1, True) if single_arg else subn
        found = r"(?:" + sub1 + r"(?:\(" + subn + r"\)" + sub1 + r")*)"
        arg_rx_cache[key] = found
    return found


def ArgsRx(count, depth=2):
    return r" *\(" + r", *".join([f"(" + ArgRx(depth, True) + r")"] * count) + r"\)"


ID_REGEX = r"([A-Za-z_][A-Za-z_0-9]*)"
ARG_REGEX = r"([^=,;]+)"
ARGS_REGEX = r"([^=;]+)"
STRICT_ARG_REGEX = "(" + ArgRx(1, True) + ")"


def Main():
    print(ArgsRx(2, 1))
    os.chdir(sys.path[0])
    for fn in glob.glob("*.[ch]"):
        if fn in ["msdos.c", "global.h", "chez_scheme.h"]:
            continue
        print(fn)
        with open(fn) as f:
            text = f.read()
            lines = text.split("\n")
            for i, line in enumerate(lines):
                line = re.sub(
                    r"\bXVECTOR_CACHE_UPDATE" + ArgsRx(2), r"\1 = XVECTOR (\2)", line
                )
                line = re.sub(
                    r"\bXVECTOR_CACHE" + ArgsRx(2),
                    r"struct Lisp_Vector *\1 = XVECTOR (\2)",
                    line,
                )
                line = re.sub(
                    r"\bXVECTOR_CACHE_IF" + ArgsRx(3),
                    r"struct Lisp_Vector *\2 = \1 ? XVECTOR (\3) : NULL",
                    line,
                )
                # line = re.sub(r"\bxvector_t\b *", r"struct Lisp_Vector *", line)
                # line = re.sub(r"\bxvector_contents_t\b *", r"Lisp_Object *", line)
                # line = re.sub(r"\bXVC_NULL\b", r"NULL", line)
                # line = re.sub(r"\b(as_xvc?|xvc?_unwrap)" + ArgsRx(1), r"\2", line)
                # line = re.sub(
                #     r"\bxvector_contents" + ArgsRx(1), r"XVECTOR (\1)->contents", line
                # )
                # line = re.sub(r"\bxvc_ref" + ArgsRx(2), r"\1[\2]", line)
                # line = re.sub(r"\bXVC_LOCAL" + ArgsRx(2), r"Lisp_Object \1[\2]", line)
                # line = re.sub(
                #     r"\bLFACE_LOCAL" + ArgsRx(1),
                #     r"Lisp_Object \1[LFACE_VECTOR_SIZE]",
                #     line,
                # )
                # line = re.sub(r"\bxvc_set" + ArgsRx(3, 3), r"\1[\2] = \3", line)
                lines[i] = line
        new_text = "\n".join(lines)
        if text != new_text:
            with open(fn, "w") as f:
                f.write(new_text)

    # proc = subprocess.Popen(
    #     ["make", "-j", "-k", "temacs"], stdout=subprocess.PIPE, stderr=subprocess.STDOUT
    # )
    # lines = [line.decode().rstrip("\n") for line in proc.stdout]
    # proc.wait()
    # lines.reverse()
    # for i in range(1, len(lines)):
    #     state = 0
    #     em = ErrMatcher(lines[i])
    #     cm = CodeMatcher(lines[i - 1][1:])
    #     if False:
    #         pass
    #     elif em(r"invalid type argument of ‘->’ \(have ‘int’\)"):
    #         # print(lines[i])
    #         if cm(
    #             r"(\s*)Lisp_Object \*"
    #             + ID_REGEX
    #             + r" = XVECTOR\s*\("
    #             + ARG_REGEX
    #             + r"\)->contents;$"
    #         ):
    #             with em.Update() as u:
    #                 _, indent, var, vec = cm
    #                 u.line = f"{indent}XVECTOR_CACHE ({var}, {vec});"
    #     elif em(
    #         r"implicit declaration of function ‘XVECTOR’; did you mean ‘XVECTOR_’\? \[-Wimplicit-function-declaration\]"
    #     ):
    #         if cm(r"(.*)XVECTOR \(" + STRICT_ARG_REGEX + r"\)->contents(.*)"):
    #             with em.Update() as u:
    #                 _, pre, vec, post = cm
    #                 u.line = f"{pre}XVECTOR_CONTENTS ({vec}){post}"
    #         # if cm(
    #         #     r"XSETPVECTYPE\s*\(XVECTOR\s*\("
    #         #     + ARG_REGEX
    #         #     + r"\),\s*"
    #         #     + ARG_REGEX
    #         #     + r"\);$"
    #         # ):
    #         #     with em.Update() as u:
    #         #         _, vec_arg, type_arg = cm
    #         #         u.line = f"SETPVECTYPE ({vec_arg}, {type_arg});"
    #         # elif cm(
    #         #     r"(?:struct Lisp_Vector \*)?"
    #         #     + ID_REGEX
    #         #     + r" = XVECTOR\s*\("
    #         #     + ARGS_REGEX
    #         #     + r"\);$"
    #         # ) or cm(
    #         #     r"(?:struct Lisp_Object \*)?"
    #         #     + ID_REGEX
    #         #     + r" = XVECTOR\s*\("
    #         #     + ARGS_REGEX
    #         #     + r"\)->contents;$"
    #         # ):
    #         #     with em.Update() as u:
    #         #         _, var, arg = cm
    #         #         u.line = f"XVECTOR_CACHE ({var}, {arg});"
    #         # elif cm(
    #         #     r"(.*?)XVECTOR\s*\("
    #         #     + STRICT_ARG_REGEX
    #         #     + r"\)->contents\["
    #         #     + STRICT_ARG_REGEX
    #         #     + r"\](.*)"
    #         # ):
    #         #     with em.Update() as u:
    #         #         _, pre, vec, idx, post = cm
    #         #         u.line = f"{pre}AREF ({vec}, {idx}){post}"
    #     # if em(r"‘.*’ has no member named ‘(.*)’; did you mean ‘(\1)_’?"):
    #     #     with em.Update() as u:
    #     #         pass
    #     #         # print(u.line)
    #     #         # print(" " * m.err_column + "%")
    #     #         # i, j = ExprBefore(u.line, m.err_column)
    #     #         # print(" " * i + "*" + " " * (j - i - 1) + "*")
    #     #         # for i in range(m.err_column):
    #     #         #     end = ExprEnd(u.line, i)
    #     #         #     if end:
    #     #         #         print(" " * i + "^" + " " * (end - i - 1) + "^")
    #     #         # print(">>", u.line[ExprBefore(u.line, m.err_column) : m.err_column])
    # if not g_changes_made:
    #     lines.reverse()
    #     for line in lines:
    #         print(line)


Main()
