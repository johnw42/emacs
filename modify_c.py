#! /usr/bin/env python3
import glob
import sys
import os
import re


# class Chunk:
#     def __init__(self, data, start, stop):
#         self.data = data
#         self.start = start
#         self.stop = stop

#     def __str__(self):
#         return self.data[self.start : self.stop].decode()

#     def __repr__(self):
#         return f"Chunk(..., {self.start}, {self.stop})"

#     def match(self, regex):
#         return re.compile(regex).match(self.data, self.start, self.stop)

#     def find(self, needle, start, stop=None):
#         if stop is None:
#             stop = len(self.data)
#         return self.data.find(needle, start, stop)

#     def __getitem__(self, i):
#         return self.data[i]

#     def __eq__(self, other):
#         if isinstance(other, bytes):
#             return self.data[self.start : self.stop] == other
#         return (
#             self.data is other.data
#             and self.start == other.start
#             and self.stop == other.stop
#         )


RE_CACHE = {}


class SourceFile:
    def __init__(self, path):
        self.path = path
        self.changes = []

    def Load(self):
        with open(self.path, "rt", encoding="Latin-1") as f:
            self.data = f.read()

    def Save(self):
        if self.HasChanges():
            self.CommitChanges()
            with open(self.path, "wt", encoding="Latin-1") as f:
                f.write(self.data)

    def HasChanges(self):
        return len(self.changes) != 0

    def CommitChanges(self):
        if not self.HasChanges():
            return

        changes = self.changes
        changes.sort(key=lambda c: (c[1], c[0]))
        for c1, c2 in zip(self.changes, self.changes[1:]):
            if c2[0] < c1[2]:
                raise Exception("overlapping changes")

        changes.reverse()
        data = self.data
        parts = []
        prev_start = len(self.data)
        for start, stop, new_data in changes:
            parts.append(data[prev_start:stop])
            parts.append(new_data)
            prev_start = start
        parts.append(data[:prev_start])
        self.data = "".join(parts)
        self.changes = []

    def LineEnd(self, pos):
        try:
            return self.data.index("\n", pos)
        except ValueError:
            raise IndexError(
                "end of file" if pos == len(self.data) else "unterminated line"
            )

    def PrevLineStart(self, pos):
        try:
            return self.data.rindex("\n", 0, pos - 1) + 1
        except ValueError:
            if pos == 0:
                raise IndexError("no previous line")
            return 0

    def NextLineStart(self, pos):
        return self.LineEnd(pos) + 1

    def IsEof(self, pos):
        return pos == len(self.data)

    def Lines(self):
        offset = 0
        while offset < len(self.data):
            eol = self.NextLineStart(offset)
            yield Chunk(self.data, offset, eol)
            offset = eol

    def __getitem__(self, i):
        return self.data[i]

    def __setitem__(self, index, val):
        if not isinstance(index, slice):
            index = slice(index, index + 1)
        self.changes.append((index.start, index.stop, val))

    def CompilePattern(self, pattern):
        if pattern not in RE_CACHE:
            RE_CACHE[pattern] = re.compile(pattern)
        return RE_CACHE[pattern]

    def match(self, pattern, start):
        m = self.CompilePattern(pattern).match(self.data, start)
        assert not m or m.end() >= start
        return m

    def search(self, pattern, start):
        m = self.CompilePattern(pattern).search(self.data, start)
        assert not m or m.end() >= start
        return m

    def MustMatch(self, pattern, start):
        m = self.match(pattern, start)
        if not m:
            raise Exception(f"no match at {self.path} offset {start}")
        return m

    def ForwardExpr(self, pos):
        pos = self.SkipSpaces(pos)
        next_pos = self.NextMatchingDelimiter(pos)
        if next_pos is not None:
            return "other", pos, next_pos
        m = self.match(r"[a-zA-Z0-9_]+", pos)
        if m:
            return "name", pos, m.end()
        return self.data[pos], pos, pos + 1

    def NextMatchingDelimiter(self, pos):
        data = self.data
        depth = 0
        start = pos
        while True:
            c = data[pos]
            if c in "([{":
                depth += 1
            elif c in ")]}":
                depth -= 1
            elif c in "'\"":
                while True:
                    pos += 1
                    if data[pos] == c:
                        break
                    elif data[pos] == "\\":
                        pos += 1
            elif data[pos : pos + 2] == "//":
                while data[pos] != "\n":
                    pos += 1
            elif data[pos : pos + 2] == "/*":
                while data[pos : pos + 2] != "*/":
                    pos += 1
            if depth == 0:
                return pos if pos != start else None
            pos += 1

    def SkipSpaces(self, pos):
        data = self.data
        while True:
            m = self.match(r"\s+", pos)
            if m:
                pos = m.end()
            # if data[pos : pos + 2] == "//":
            #     while data[pos] != "\n":
            #         pos += 1
            # elif data[pos : pos + 2] == "/*":
            #     while data[pos : pos + 2] != "*/":
            #         pos += 1
            # else:
            return pos

    def index(self, text, pos):
        return self.data.index(text, pos)


def ParseLocalVarDecl(f, start, stop):
    while True:
        m = f.match(r"\**", start)
        pre_decl = m.group()
        pos = m.end()
        t, name_start, name_end = f.ForwardExpr(pos)
        assert t == "name"
        name = f[name_start:name_end]
        expr_end = name_end
        init_start = None
        while True:
            t, expr_start, expr_end = f.ForwardExpr(expr_end)
            if t == "=":
                post_decl = f[name_end:expr_start].strip()
                init_start = expr_end
            elif t in ",;":
                if init_start is None:
                    init_expr = None
                    post_decl = f[name_end:expr_start].strip()
                else:
                    init_expr = f[init_start:expr_start].strip()
                yield pre_decl, name, post_decl, init_expr
                if t == ";":
                    return
                start = f.SkipSpaces(expr_end)
                break

    # for var in text.split(","):
    #     var = var.strip()
    #     init = None
    #     if "=" in var:
    #         var, init = (x.strip() for x in var.split("="))
    #     yield var, init
    yield text


def ProcessFunction(f, open_brace_pos):
    # print("candidate at", f.path, open_brace_pos)
    name_start = f.PrevLineStart(open_brace_pos)
    while f.match(r"^\S", name_start):
        name_start = f.PrevLineStart(name_start)
    m = f.match(r"([A-Za-z0-9_]+)\s*\(", name_start)
    if not m:
        return
    name = m.group(1)
    t_start = f.PrevLineStart(name_start)
    if name == "record_event":
        return
    m = f.MustMatch(r"(?:(?:INLINE|static)\s+)?(\S.*)", t_start)
    r_type = m.group(1)
    close_brace_pos = f.NextMatchingDelimiter(open_brace_pos)
    f.MustMatch(r"}\s", close_brace_pos)
    here = f.NextLineStart(open_brace_pos)
    while here < close_brace_pos:
        m = f.match(r"\s*(?:for\s*\(\s*)Lisp_Object\s", here)
        if m:
            decl_end = f.index(";", m.end())
            for v in ParseLocalVarDecl(f, m.end(), decl_end):
                print(v)
            here = decl_end
        here = f.NextLineStart(here)


def Main():
    os.chdir(sys.path[0])
    for path in glob.glob("src/*.[ch]"):
        print(path)
        f = SourceFile(path)
        f.Load()
        here = 0
        while not f.IsEof(here):
            eol = f.LineEnd(here)
            try:
                if f[here:eol] == "{":
                    ProcessFunction(f, here)
            except IndexError:
                pass
            here = f.NextLineStart(eol)
        if f.HasChanges():
            f.CommitChanges()
            return


Main()
