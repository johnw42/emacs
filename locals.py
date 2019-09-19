import glob
import re
import sys


class CallBuilder:
    def __init__(self, indent, to_call):
        self.fill_col = 70
        self.out_line = indent + to_call + " "
        self.indents = [indent]
        self.has_arg = False
        self.next_chunk = ""
        self.out_lines = []
        self.at_bol = False

    def WriteTo(self, f):
        for line in self.out_lines:
            f.write(line.rstrip() + "\n")
        f.write(self.out_line.rstrip() + ";\n")

    def EndLine(self):
        self.out_lines.append(self.out_line)
        self.out_line = self.indents[-1]

    def BeginArgs(self):
        if self.has_arg:
            self.out_line += ", "
        self.out_line += "("
        self.indents.append(" " * len(self.out_line))
        self.has_arg = False

    def EndArgs(self):
        self.out_line += ")"
        self.indents.pop()
        self.has_arg = True

    def AddArg(self, arg):
        if not arg.strip():
            return
        if self.has_arg:
            self.out_line += ", "
        if len(self.out_line) + len(arg) >= self.fill_col:
            self.EndLine()
        self.out_line += arg
        self.has_arg = True


def Main():
    for filename in sorted(glob.glob("src/*.c")):
        lines = open(filename, "rt", encoding="Latin-1").readlines()
        i = 0
        with open(filename, "wt", encoding="Latin-1") as f:
            while i < len(lines):
                m = re.match(
                    r"^(\s+)(ENTER_LISP_FRAME[A-Z_]*?)\s*\(([^)]*)(\);)?$", lines[i]
                )
                if m:
                    indent = m[1]
                    macro_name = m[2]
                    macro_args = re.split(r",\s*", m[3])
                    if not m[4]:
                        while True:
                            i += 1
                            m = re.match(r"^\s*([^)]*)(\);)$", lines[i])
                            macro_args += re.split(r",\s*", m[1])
                            if m[2]:
                                break
                    i += 1
                    num_pre_args = 0
                    if "_VA" in macro_name:
                        num_pre_args += 2
                    if "_T" in macro_name:
                        num_pre_args += 1
                    pre_args = macro_args[:num_pre_args]
                    del macro_args[:num_pre_args]

                    macro_locals = []
                    m = re.match(r"^(\s+)LISP_LOCALS\s*\(([^)]*)(\);)?$", lines[i])
                    if m:
                        assert indent == m[1]
                        macro_locals = re.split(r",\s*", m[2])
                        if not m[3]:
                            while True:
                                i += 1
                                m = re.match(r"^\s*([^)]*)(\);)?$", lines[i])
                                macro_locals += re.split(r"\s*,\s*", m[1])
                                if m[2]:
                                    break
                        i += 1
                    b = CallBuilder(indent, macro_name)
                    b.BeginArgs()
                    for arg in pre_args:
                        b.AddArg(arg)
                    b.BeginArgs()
                    for arg in macro_args:
                        b.AddArg(arg)
                    b.EndArgs()
                    out_line = indent + macro_name
                    for arg in macro_locals:
                        b.AddArg(arg)
                    b.EndArgs()
                    b.WriteTo(f)
                    continue
                f.write(lines[i])
                i += 1


Main()
