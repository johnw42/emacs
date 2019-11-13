#! /usr/bin/env python3
import re
import os
import sys
import glob


def MoveBrackets(lines):
    for i in range(len(lines)):
        lines[i] = lines[i].replace("\t", " " * 8).rstrip("\n")
    i = 0
    while i + 1 < len(lines):
        indent = re.match(r" *", lines[i])[0]
        if lines[i + 1] == indent + "{":
            lines[i] += " {"
            del lines[i + 1]
        else:
            i += 1


VAR_PATTERN = "(?: *\**[a-zA-Z_]+(?: *\[[^][]\])*)"


class App:
    def __init__(self):
        self.typedefs = {}
        self.member_types = {}

        os.chdir(sys.path[0])
        for path in sorted(glob.glob("src/*.h")):
            print(path)
            self.lines = open(path, encoding="Latin-1").readlines()
            MoveBrackets(self.lines)
            self.line_index = -1
            self.ReadDecls("", None)

        for key, mtypes in self.member_types.items():
            self.member_types[key] = set(map(self.ResolveTypedef, mtypes))

        # for key in sorted(self.member_types):
        #     print(key)
        #     for mtype in sorted(self.member_types[key]):
        #         print("  " + mtype)

        for key in self.member_types:
            if self.HasLispObject(key):
                print(key)

        # print(self.HasLispObject("struct bidi_it"))

    def HasLispObject(self, struct_name):
        if struct_name not in self.member_types:
            return False
        for mtype in self.member_types[struct_name]:
            if mtype == struct_name:
                pass
            elif mtype == "Lisp_Object":
                return True
            elif self.HasLispObject(mtype):
                return True
        return False

    def ResolveTypedef(self, type_name):
        return self.typedefs.get(type_name, type_name)

    def NextLine(self):
        while True:
            self.line_index += 1
            if self.line_index < len(self.lines):
                self.line = self.lines[self.line_index]
                if not self.line.startswith("#"):
                    return
            else:
                self.line = None
                return

    def Match(self, pattern):
        self.match = None
        if self.line is not None:
            self.match = re.match(pattern, self.line)
        return self.match is not None

    def ReadDecls(self, indent, parent_struct):
        while True:
            self.NextLine()
            if self.line is None or not self.line.startswith(indent):
                return
            if self.Match(r"typedef ((?:struct|union) [a-zA-Z_]+) \**([a-zA-Z_]+);"):
                struct_name = self.match[1]
                typedef_name = self.match[2]
                self.typedefs[typedef_name] = struct_name
            elif self.Match(indent + r" *((?:struct|union) [a-zA-Z_]+) \{"):
                struct_name = self.match[1]
                self.ReadDecls(indent + "  ", struct_name)
            elif parent_struct and self.Match(
                indent
                + r" *((?:struct|union)? *[a-zA-Z_]+)(?:"
                + VAR_PATTERN
                + ",)*"
                + VAR_PATTERN
                + r";"
            ):
                member_type = self.match[1]
                self.member_types.setdefault(parent_struct, set()).add(member_type)


App()
