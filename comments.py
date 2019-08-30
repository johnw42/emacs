#! /usr/bin/env python3
import glob
import os
import sys
import re

PREPROC_RE = r"^(\s*#\s*\b(if|ifdef|elif|ifndef|else|endif)\b)"


def Negate(s):
    if s is True:
        return s
    if s.startswith("not "):
        return s[4:]
    else:
        return "not " + s


def Main():
    os.chdir(sys.path[0])
    for path in glob.glob("src/*.[ch]"):
        with open(path, "rt") as f:
            try:
                lines = f.readlines()
            except UnicodeDecodeError:
                continue
        stack = []
        has_changes = False
        for i in range(len(lines)):
            line = lines[i]
            m = re.match(PREPROC_RE, line)
            top = stack[-1] if stack else None
            if m:
                prefix = m[1]
                tok = m[2]
                # print(tok, stack)
                if tok == "if":
                    stack.append(None)
                elif tok == "ifdef":
                    if "HAVE_CHEZ_SCHEME" in line:
                        stack.append("HAVE_CHEZ_SCHEME")
                    else:
                        stack.append(None)
                elif tok == "ifndef":
                    if "HAVE_CHEZ_SCHEME" in line:
                        stack.append("not HAVE_CHEZ_SCHEME")
                    else:
                        stack.append(None)
                elif tok == "else":
                    if top:
                        stack[-1] = top = Negate(top)
                        lines[i] = prefix + f" /* {top} */\n"
                elif tok == "endif":
                    if top:
                        lines[i] = prefix + f" /* {top} */\n"
                    stack.pop()
                if lines[i] != line:
                    has_changes = True
        if has_changes:
            print(path)
            with open(path, "wt") as f:
                f.writelines(lines)


Main()
