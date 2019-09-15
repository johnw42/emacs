#! /usr/bin/env python3
import glob
import sys
import os
import re
import warnings


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
        data = self.data
        if self.HasChanges():
            self.CommitChanges()
            assert data != self.data
            assert len(data) * 0.8 <= len(self.data) <= len(data) * 1.2
            assert "LISP_LOCALS" in self.data
            with open(self.path, "wt", encoding="Latin-1") as f:
                f.write(self.data)
            print("wrote to", self.path)

    def HasChanges(self):
        return len(self.changes) != 0

    def CommitChanges(self):
        if not self.HasChanges():
            return

        changes = self.changes
        changes.sort(key=lambda c: (c[1], c[0]))
        for c1, c2 in zip(self.changes, self.changes[1:]):
            if c2[0] < c1[1]:
                raise Exception("overlapping changes")
        # print(changes)
        data = self.data
        parts = []
        prev_stop = 0
        for start, stop, new_data in changes:
            parts.append(data[prev_stop:start])
            parts.append(new_data)
            prev_stop = stop
        parts.append(data[prev_stop:])
        self.data = "".join(parts)
        assert self.data != data
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
        if self.data[index] == val:
            warnings.warn("no change")
        else:
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

    def BackToLineStart(self, pos):
        while pos > 0 and self.data[pos - 1] != "\n":
            pos -= 1
        return pos

    def NextMatchingDelimiter(self, pos):
        data = self.data
        depth = 0
        start = pos
        while pos < len(data):
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
                pos += 1
            if depth == 0:
                return pos + 1 if pos != start else None
            pos += 1
        raise Exception(f"error at {start}")

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
    if "(*cons)" in f[start:stop]:
        return
    while True:
        m = f.match(r"\**", start)
        pre_decl = m.group()
        pos = m.end()
        t, name_start, name_end = f.ForwardExpr(pos)
        assert t == "name", (t, pos)
        name = f[name_start:name_end]
        expr_end = name_end
        init_start = None
        while True:
            prev_expr_end = expr_end
            t, expr_start, expr_end = f.ForwardExpr(expr_end)
            assert expr_end <= stop + 1, (t, prev_expr_end, expr_start)
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
    f_name = m.group(1)
    print(f_name)
    t_start = f.PrevLineStart(name_start)
    if f_name == "record_event":
        return
    m = f.MustMatch(r"(?:(?:INLINE|static)\s+)?(\S.*)", t_start)
    r_type = m.group(1)
    close_brace_pos = f.NextMatchingDelimiter(open_brace_pos)
    f.MustMatch(r"}\s", close_brace_pos - 1)
    here = f.NextLineStart(open_brace_pos)
    local_vars = []
    while here < close_brace_pos:
        m = f.match(
            r"(\s*)(for\s*\(\s*)?((?:(?:register|const)\s+)*Lisp_Object\s)", here
        )
        if m:
            indent = m.group(1)
            in_loop = m.group(2)
            decl_start = m.start(3)
            decl_end = f.index(";", m.end())
            assign_exprs = []
            for pre_decl, name, post_decl, init_expr in ParseLocalVarDecl(
                f, m.end(), decl_end
            ):
                if pre_decl or post_decl:
                    warnings.warn(f"pre_decl or post_decl in {f_name}")
                    return
                if name not in local_vars:
                    local_vars.append(name)
                if init_expr:
                    assign_exprs.append(name + " = " + init_expr)
            if assign_exprs or in_loop:
                if in_loop:
                    sep = ", "
                else:
                    sep = ";\n" + indent
                f[decl_start:decl_end] = sep.join(assign_exprs)
            else:
                f[f.BackToLineStart(decl_start) : f.NextLineStart(decl_end)] = ""
            here = decl_end
        here = f.NextLineStart(here)
    if local_vars:
        pos = f.NextLineStart(open_brace_pos)
        f[pos:pos] = "  LISP_LOCALS(" + ", ".join(local_vars) + ");\n"


def Main():
    os.chdir(sys.path[0])
    for path in glob.glob("src/*.[ch]"):
        print(path)
        f = SourceFile(path)
        f.Load()
        here = 0
        while not f.IsEof(here):
            eol = f.LineEnd(here)
            if f[here:eol] == "{":
                ProcessFunction(f, here)
            here = f.NextLineStart(eol)
        if f.HasChanges():
            print(path, "updated")
            f.Save()
            sys.exit(1)


Main()
