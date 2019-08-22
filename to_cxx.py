#! /usr/bin/env python3
import os
import sys
import glob


def Main():
    os.chdir(sys.path[0])
    for path in glob.glob("src/*.c"):
        print(path)
        os.rename(path, path + "c")
    pass
