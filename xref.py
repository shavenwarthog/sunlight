#!/usr/bin/python

'''
xref.py -- postprocess Etags database into call graph
'''

import os, re
from itertools import imap
from nose.tools import eq_ as eq
from operator import itemgetter

tagsdata = os.popen('ctags -f - zoot.py')

symbols = {}
for name,path,_,rest in imap(lambda line: line.split('\t',3), tagsdata):
    symbols[name] = rest[0]     # symbol type

# defs = list( imap(itemgetter(1), def_pat.findall(tagsdata)) )
defs = symbols.keys()
print 'defs:',defs

defpat = re.compile(
    '(def|class)\s+(\w+)',
    )
callpat = re.compile(
    '(%s)\s*\(' % '|'.join(defs)
    )

# os.system('egrep -n . zoot.py')
# print

# etags format:
#	{tag_definition_text}<\x7f>{tagname}<\x01>{line_number},{byte_offset}
#
def symref(tagdef, tagname, lineno, offset):
    return '%s\x7f%s\x01%d,%d' % (tagdef, tagname, lineno, offset)

def enum_pos(lines):
    pos = 0
    for num,line in enumerate(lines):
        nextpos = pos+len(line)+1
        yield num, line, pos,nextpos
        pos = nextpos

def test_enum_pos():
    eq( list(enum_pos('tasty beer'.split(' '))),
        [(0, 'tasty', 0, 6), (1, 'beer', 6, 11)],
        )

def main(paths):
    tagsf = open('calls.tags', 'w')
    for path in paths:
        source = open(path, 'r').read()
        print >> tagsf, '\f'
        print >> tagsf, '%s,%s' % (path, len(source))      # X?
        print >> tagsf, '\x7f%s\x011,0' % path      # X?

        in_def = None
        for num,line,begpos,_ in enum_pos(source.split('\n')):
            print '%2d %3d %s' % (num+1, begpos, line)
            m = defpat.search(line)
            if m:
                in_def = m.group(2), num+1, begpos
                print 'def', in_def
            else:
                for m in callpat.finditer(line):
                    print '\t', m.group(1), '=>', in_def[0]
                    print >> tagsf, symref(
                        tagdef=line,
                        tagname=m.group(1),
                        lineno=num+1, 
                        offset=begpos,
                        )
    tagsf.close()

main(['zoot.py'])
