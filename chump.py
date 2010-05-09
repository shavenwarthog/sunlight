#!/usr/bin/env python

'''
chump -- find functions not used except in selftests
'''

import fileinput, os, re

MARK = '\0177'
objpat = re.compile('^ *(class|def) *(.+)\(.(\d+),(\d+)$')
filepat = re.compile('(.+.py),\d+$')

filename = None
for line in fileinput.input():
    line = line.strip()
    m = objpat.match(line)
    if m:
        print filename,m.groups()
        continue
    m = filepat.search(line)
    if m:
        filename = os.path.basename(m.group(1))

print 'done'
