#! /usr/bin/env python3

import re
import sys
import contextlib


class Updater:
    def __init__(self, filename, line_index):
        self.filename = filename
        self.line_index = line_index
        with open(filename) as f:
            self.lines = f.readlines()
        self.line = self.lines[line_index].rstrip("\n")

    def __enter__(self):
        return self

    def __call__(self, new_line):
        assert "\n" not in new_line
        self.lines[self.line_index] = new_line + "\n"

    def __exit__(self, x, *_):
        new_line = self.line.rstrip("\n")
        assert "\n" not in new_line

        if self.lines[self.line_index].rstrip("\n") == new_line:
            return

        # print(new_line)
        self.lines[self.line_index] = new_line + "\n"

        if not x:
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
        self.match = match = re.search(pat, self.line)
        if not self.match:
            return False
        self.before = self.line[: match.start(0)]
        self.after = self.line[match.end(0) :]
        return True

    def __getattr__(self, attr):
        return getattr(self.match, attr)

    def __getitem__(self, i):
        return self.match[i]

    def __len__(self):
        return len(self.match)

    def Err(self, pat):
        if self(r"^[^:]+:\d+:\d+: error: " + pat):
            parts = self.line.split(":")[:3]
            self.err_file = parts[0]
            self.err_line = int(parts[1]) - 1
            self.err_column = int(parts[2]) - 1
            return True
        return False

    def Update(self):
        return Updater(self.err_file, self.err_line)


def main():
    lines = list(sys.stdin)
    lines.reverse()
    for i, line in enumerate(lines):
        state = 0
        line = line.rstrip()
        m = Matcher(line)
        if m.Err(r"‘.*’ has no member named ‘([^’]*)’"):
            name = m[1]
            # print(repr(name), lines[i - 1].rstrip("\n"))
            with m.Update() as u:
                um = Matcher(u.line)
                if um(r"^(\s*)([^=]+)->([a-z_]+) = ([^;]+);$") and um[3] == name:
                    _, indent, obj, field, rhs = um
                    u.line = f"{indent}PV_LISP_FIELD_SET ({obj}, {field}, {rhs});"
                elif (
                    um(r"([A-Z_]+\s*\([^()]+\)|[a-z_A-Z]+)->([a-z_]+)")
                    # and um[2] == name
                ):
                    _, obj, field = um
                    u.line = um.before + f"PV_LISP_FIELD_REF({obj}, {field})" + um.after
                # print(" " * m.err_column + "%")
                # i, j = ExprBefore(u.line, m.err_column)
                # print(" " * i + "*" + " " * (j - i - 1) + "*")
                # for i in range(m.err_column):
                #     end = ExprEnd(u.line, i)
                #     if end:
                #         print(" " * i + "^" + " " * (end - i - 1) + "^")
                # print(">>", u.line[ExprBefore(u.line, m.err_column) : m.err_column])


main()
