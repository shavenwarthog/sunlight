#!/usr/bin/env python

import re
from itertools import ifilter, imap

def match_iter(matchfunc, source):
    return ifilter(None, imap(matchfunc, source))

SAMPLE = '''
# comment ignored
beer=tasty
john=stud
'''.split('\n')

keyval = re.compile('(\w+)=(\w+)')
for mat in match_iter(keyval.match, SAMPLE):
    print mat.groups()

def beer_iter(miter):
    groups = (mat.groups() for mat in miter)
    return (value for (key,value) in groups
            if key=='beer')

print beer_iter(match_iter(keyval.match, SAMPLE))

print list(beer_iter(match_iter(keyval.match, SAMPLE)))

# ./ex-functional.py
# ('beer', 'tasty')
# ('john', 'stud')
# <generator object <genexpr> at 0x26d1a00>
# ['tasty']
