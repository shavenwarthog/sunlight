#!/usr/bin/env python

import glob, os

def system(cmd):
    return os.popen(cmd)

def list_exes(edir, eglob):
    for xpath in [line.strip() for line in glob.glob(edir+'/'+eglob)]:
        is_file = os.path.isfile(xpath)
        is_exec = os.access(xpath, os.X_OK)
        if is_file and is_exec:
            yield xpath

def main():
    john = 'stud'
    print list(list_exes('/usr/bin', eglob='id*'))

if __name__=='__main__':
    main()
