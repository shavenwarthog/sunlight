#!/usr/bin/env python2.6

import os, re

# hash sourceline resultline numlines
startpat = re.compile('(\S{40}) (\d+) (\d+) (\d+)')
keyvalpat = re.compile('(\S+)\s+(.+)')

def calcage():
    start = None
    hashinfo = {}
    info = {}
    for line in os.popen('git blame --incremental piemacs.el'):
        m = startpat.match(line)
        if m:
            start = m.groups()
            info = {}
            continue
        m = keyvalpat.match(line)
        if m:
            key,val = m.groups()
            if key=='filename':
                if info:
                    hashinfo[start[0]] = info
                # print start[1:], hashinfo[start[0]]['committer-time']
                yield int(start[1]), int(start[2]), int(hashinfo[start[0]]['committer-time'])
                continue
            info[m.group(1)] = m.group(2)

print list(calcage())
