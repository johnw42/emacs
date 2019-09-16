#! /usr/bin/env python3
import glob
import sys
import os
import re
import warnings


RE_CACHE = {}


def CompileRegex(pattern):
    if pattern not in RE_CACHE:
        RE_CACHE[pattern] = re.compile(pattern, re.MULTILINE | re.DOTALL)
    return RE_CACHE[pattern]


class Sexp:
    def __init__(self, typ, *args):
        self.type = typ
        self.prev_token = None
        self.next_token = None
        self.prev_expr = None
        self.next_expr = None
        self.parent = None
        if len(args) == 1:
            children = args[0]
            assert isinstance(children, list)
            self.children = children
            self.line = children[0].line
            self.col = children[0].col
            self.start = children[0].start
            self.stop = children[-1].stop
            for child in children:
                assert child.parent is None
            self.text = None
            self.tokens = [t for c in children for t in c.tokens]
        else:
            start, line, col, text = args
            assert isinstance(text, str)
            self.start = start
            self.line = line
            self.col = col
            self.stop = start + len(text)
            self.text = text
            self.children = []
            self.tokens = [self]

    def __repr__(self):
        r = f"Sexp({repr(self.type)}, {self.start}, {self.line}, {self.col}, "
        if self.text:
            r += repr(self.text)
        else:
            r += "[...]"
        return r + ")"


def LinkExprs(exprs):
    prev = None
    for expr in exprs:
        if prev:
            prev.next_expr = expr
            expr.prev_expr = prev
        prev = expr


class SourceFile:
    def __init__(self, path):
        self.path = path
        self.changes = []

    def Load(self):
        with open(self.path, "rt", encoding="Latin-1") as f:
            self.data = f.read()
        self.Parse()

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

    def Tokenize(self):
        data = self.data
        line = 0
        line_start = 0
        prev_token = None
        self.line_starts = [0]

        def MakeToken(typ, start, stop):
            nonlocal line, line_start, prev_token
            for i in range(line_start, start):
                if data[i] == "\n":
                    line += 1
                    line_start = i + 1
                    self.line_starts.append(line_start)
            token = Sexp(typ, start, line, start - line_start, data[start:stop])
            if prev_token:
                token.prev_token = prev_token
                prev_token.next_token = token
            prev_token = token
            return token

        i = 0
        eof = len(data)
        while i < eof:
            # Skip whitespace and comments.
            while i < eof:
                m = self.Match(r"\s+", i)
                if m:
                    i = m.end()
                if self.Match(r"/\*", i):
                    i = self.MustFind(r"\*/", i + 2).end()
                elif self.Match(r"//", i):
                    i = self.MustFind(r"\n", i + 2).end()
                else:
                    break

            if i >= eof:
                break

            # Tokenize preprocessor directives as a single token.
            m = self.Match(r"^\s*#", i)
            if m:
                pp_start = m.start()
                i = m.end()
                while True:
                    m = self.MustFind(r"(/\*)|(\\\n)|(\n)", i)
                    if m[1]:
                        i = self.MustFind(r"\*/", m.end()).end()
                    elif m[2]:
                        i = m.end()
                    else:
                        assert m[3]
                        yield MakeToken("#", pp_start, m.end())
                        i = m.end()
                        break
                continue

            # Match identifier-like tokens.
            m = self.Match(r"[a-zA-Z0-9_]+", i)
            if m:
                yield MakeToken("word", m.start(), m.end())
                i = m.end()
                continue

            # Match string literals.
            m = self.Match(r"'(\\.|[^'])*'", i) or self.Match(r'"(\\.|[^"])*"', i)
            if m:
                yield MakeToken(data[i], i, m.end())
                i = m.end()
                continue

            yield MakeToken(data[i], i, i + 1)
            i += 1

    def Parse(self):
        stack = [[]]
        for token in self.Tokenize():
            if token.type in "([{":
                stack.append([token])
            elif token.type in ")]}":
                children = stack.pop()
                children.append(token)
                LinkExprs(children)
                assert stack, token
                stack[-1].append(Sexp("expr" + children[0].type, children))
            else:
                stack[-1].append(token)
        assert len(stack) == 1
        self.exprs = stack[0]
        LinkExprs(self.exprs)

    def ExprAt(self, pos):
        def FindIn(exprs):
            for i, expr in enumerate(exprs):
                if expr.start == pos:
                    return exprs, i
                if expr.end > pos:
                    return FindIn(expr.children)
            raise Exception(f"no expr found at {pos}")

        return FindIn(self.exprs)

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

    def TranslateIndex(self, index):
        if isinstance(index, slice):
            if isinstance(index.start, Sexp):
                return slice(index.start.start, index.stop.stop)
            return index
        elif isinstance(index, Sexp):
            return slice(index.start, index.stop)
        else:
            return slice(index, index + 1)

    def __getitem__(self, i):
        return self.data[self.TranslateIndex(i)]

    def __setitem__(self, index, val):
        index = self.TranslateIndex(index)
        if self.data[index] == val:
            warnings.warn("no change")
        else:
            self.changes.append((index.start, index.stop, val))

    def Match(self, pattern, start):
        if start < 0:
            return None
        m = CompileRegex(pattern).match(self.data, start)
        assert not m or m.end() >= start
        return m

    def Search(self, pattern, start):
        m = CompileRegex(pattern).search(self.data, start)
        assert not m or m.end() >= start
        return m

    def MustMatch(self, pattern, start):
        m = self.Match(pattern, start)
        if not m:
            raise Exception(f"no match at {self.path} offset {start}")
        return m

    def MustFind(self, pattern, start):
        m = self.Search(pattern, start)
        if not m:
            raise Exception(f"no match at {self.path} starting at offset {start}")
        return m

    def ForwardExpr(self, pos):
        pos = self.SkipSpaces(pos)
        next_pos = self.NextMatchingDelimiter(pos)
        if next_pos is not None:
            return "other", pos, next_pos
        m = self.Match(r"[a-zA-Z0-9_]+", pos)
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
            m = self.Match(r"\s+", pos)
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


def ParseLocalVarDecl(f, type_token):
    # if "(*cons)" in f[f.start:stop]:
    #     return

    token = type_token
    if token.next_expr.type == "expr(":
        # Ignore function pointers.
        return
    while token.type != ";":
        token = token.next_expr
        pre_decl = ""
        if token.text == "*":
            pre_decl += token.text
            token = token.next_expr
        assert token.type == "word"
        name = token.text
        post_decl = ""
        token = token.next_expr
        if token.type not in "=,;)":
            post_decl_start = post_decl_end = token
            while post_decl_end.next_expr.type not in "=,;)":
                post_decl_end = post_decl_end.next_expr
            post_decl = f[post_decl_start:post_decl_end]
            token = post_decl_end.next_expr
        init = None
        if token.type == "=":
            init_start = init_end = token.next_expr
            while init_end.next_expr.type not in ",;)":
                init_end = init_end.next_expr
            init = f[init_start:init_end]
            token = init_end.next_expr
        assert token.type in ",;)", token
        yield pre_decl, name, post_decl, init


SAFE_FUNCTIONS = """
    if for while emacs_abort eassert
    XINT INTEGERP STRINGP
    BVAR
    CHAR_TABLE_REF CHAR_VALID_P
""".split()


def ProcessFunction(f, body_expr):
    # Make sure we've actaully found a function.
    args_expr = body_expr.prev_expr
    if args_expr.type != "expr(":
        return
    name_tok = args_expr.prev_expr
    if name_tok.type == "word" or name_tok.col == 0:
        is_defun = False
        f_name = name_tok.text
    elif name_tok.type == "expr(" and name_tok.prev_expr.text == "DEFUN":
        is_defun = True
        f_name = "DEFUN"
    else:
        return

    # Skip functions with no function calls.
    for i in range(len(body_expr.tokens) - 1):
        word, paren = body_expr.tokens[i : i + 2]
        if (
            word.type == "word"
            and paren.type == "("
            and word.text not in SAFE_FUNCTIONS
        ):
            break
    else:
        return

    if is_defun:
        r_type = "Lisp_Object"
    else:
        type_start = type_end = name_tok.prev_expr
        while type_start.col != 0:
            type_start = type_start.prev_expr
        if type_start.text in ["INLINE", "static"]:
            type_start = type_start.next_expr
        r_type = f[type_start.start : type_end.stop]

    lisp_args = []
    has_varargs = False
    for i in range(1, len(args_expr.tokens) - 1):
        if (
            args_expr.tokens[i].text == "Lisp_Object"
            and args_expr.tokens[i + 1].type == "word"
        ):
            lisp_args.append(args_expr.tokens[i + 1].text)
        elif list(t.text for t in args_expr.tokens[i : i + 5]) == [
            "nargs",
            ",",
            "Lisp_Object",
            "*",
            "args",
        ]:
            has_varargs = True
            break

    macro_name = "ENTER_LISP_FRAME"
    macro_args = list(lisp_args)
    if has_varargs:
        macro_name += "_VA"
        macro_args[0:] = ["nargs", "args"]
    if r_type not in ["Lisp_Object", "void"]:
        macro_args[0:0] = [r_type]
        macro_name += "_T"
    entry_expr = macro_name + " (" + ", ".join(macro_args) + ")"

    local_vars = []
    for token in body_expr.tokens:
        if token.text != "Lisp_Object":
            continue
        decl_start = token
        while decl_start.prev_expr.text in ["register", "const"]:
            decl_start = decl_start.prev_token
        context = f[f.line_starts[decl_start.line] : token.start]
        m = re.match(r"(\s*)(for\s*\(\s*)?$", context)
        if not m:
            continue
        indent = m[1]
        in_loop = bool(m[2])
        decl_end = token
        while decl_end.text != ";":
            decl_end = decl_end.next_expr
        assign_exprs = []
        if not all(
            pre_decl or post_decl
            for pre_decl, _, post_decl, _ in ParseLocalVarDecl(f, token)
        ):
            for pre_decl, name, post_decl, init_expr in ParseLocalVarDecl(f, token):
                if pre_decl or post_decl:
                    assert not in_loop
                    decl = "Lisp_Object " + pre_decl + name + post_decl
                    if init_expr:
                        decl += " = " + init_expr
                    assign_exprs.append(decl)
                    continue
                if name not in local_vars:
                    local_vars.append(name)
                if init_expr:
                    assign_exprs.append(name + " = " + init_expr)
            if assign_exprs:
                if in_loop:
                    f[decl_start:decl_end] = ", ".join(assign_exprs) + ";\n"
                else:
                    sep = ";\n" + indent
                    f[decl_start:decl_end] = (";\n" + indent).join(assign_exprs) + ";\n"
            else:
                f[f.line_starts[decl_start.line] : f.NextLineStart(decl_end.stop)] = ""

    is_lisp_func = local_vars or has_varargs or lisp_args
    if is_lisp_func:
        pos = body_expr.tokens[0].stop
        to_insert = "\n  " + entry_expr + ";"
        if local_vars:
            to_insert += "\n  LISP_LOCALS (" + ", ".join(local_vars) + ");"
        f[pos:pos] = to_insert
        if r_type == "void":
            pos = body_expr.tokens[-1].start
            f[pos:pos] = "  EXIT_LISP_FRAME_VOID ();\n"
            for token in body_expr.tokens:
                if token.text == "return":
                    f[token] = "EXIT_LISP_FRAME_VOID ()"
        else:
            for token in body_expr.tokens:
                if token.text == "return":
                    return_start = return_end = token.next_expr
                    while return_end.next_expr.type != ";":
                        return_end = return_end.next_expr
                    f[token:return_end] = (
                        "EXIT_LISP_FRAME (" + f[return_start:return_end] + ")"
                    )


def Main():
    os.chdir(sys.path[0])
    # for path in ["src/window.h"]:
    for path in sorted(glob.glob("src/*.[ch]")):
        if path in ["src/scheme_lisp.c", "src/alloc.c"]:
            continue
        print(path)
        f = SourceFile(path)
        f.Load()
        for expr in f.exprs:
            if expr.type == "expr{" and expr.col == 0:
                ProcessFunction(f, expr)
        if f.HasChanges():
            print(path, "updated")
            f.Save()


Main()
