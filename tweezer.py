#!/usr/bin/env python

import glob, os

def system(cmd):
    return os.popen(cmd)

def list_exes():
    for path in [line.strip() for line in glob.glob('/usr/bin/id*')]:
        is_file = os.path.isfile(path)
        is_exec = os.access('/usr/bin/id',os.X_OK)
        if is_file and is_exec:
            yield path

print list(list_exes())
