#!/usr/bin/env python

# XXXX: doesnt work

import os, sys

def noext(name):
    return os.path.splitext(name)[0]

for path in os.popen("find /usr/share/emacs/23.1/lisp/progmodes -name '*.el.gz'"):
    path = path.strip()
    group,name = path.split('/')[-2:]
    name = noext(noext(name))
    cmd_fmt = 'zcat %s | etags --parse-stdin=%s --language=lisp --append'
    print group, name
    if os.system(cmd_fmt % (path, path)):
        print "oops"
        print path
        sys.exit(1)
    

