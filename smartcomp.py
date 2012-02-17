#!/usr/bin/env python

'''
smartcomp -- trim testing output
'''

import fileinput, re

def main():
    verbose = False

    notrace_path = re.compile(
        '/usr/')
    decor_pat = re.compile('----')
    fail_pat = re.compile('Failure instance:\s+(.+)]"$')

    skip = 0
    lines = list(fileinput.input())
    while lines:
        line = lines.pop(0)
        if decor_pat.search(line):
            continue
        m = fail_pat.search(line)
        if m:
            flines = m.group(1).split('\\n')
            lines = [ fline+'\n' for fline in flines ] + lines
            continue
        if notrace_path.search(line):
            if verbose:
                print '-- ',line,
            lines.pop(0)            # skip next line
            continue
        print line,


if __name__=='__main__':
    main()
