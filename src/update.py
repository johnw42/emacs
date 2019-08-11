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
        if self.lines[self.line_index].rstrip("\n") == self.line:
            return

        new_line = self.line.rstrip("\n")
        assert "\n" not in new_line
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
        self.match = re.match(pat, line)
        return bool(self.match)

    def __getattr__(self, attr):
        return getattr(self.match, attr)

    def __getitem__(self, i):
        return self.match[i]

    def __len__(self):
        return len(self.match)

    def Err(self, pat):
        if self(r"[^:]+:\d+:\d+: error: " + pat):
            parts = self.line.split(":")[:3]
            self.err_file = parts[0]
            self.err_line = int(parts[1]) - 1
            self.err_column = int(parts[2]) - 1
            return True
        return False

    def Update(self):
        return Updater(self.err_file, self.err_line)


for line in sys.stdin:
    state = 0
    line = line.rstrip()
    m = Matcher(line)
    # print(line)
    if state in [1, 2]:
        state += 1
        print(line)
    elif m.Err(r"‘.*’ has no member named ‘(.*)’; did you mean ‘(\1)_’?"):
        with m.Update() as u:
            print(u.line)
            print(" " * m.err_column + "%")
            i, j = ExprBefore(u.line, m.err_column)
            print(" " * i + "*" + " " * (j - i - 1) + "*")
            for i in range(m.err_column):
                end = ExprEnd(u.line, i)
                if end:
                    print(" " * i + "^" + " " * (end - i - 1) + "^")
            # print(">>", u.line[ExprBefore(u.line, m.err_column) : m.err_column])
