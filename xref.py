#!/usr/bin/python

'''
xref.py -- postprocess Etags database into call graph
'''

import os, re, sys
from itertools import imap
from nose.tools import eq_ as eq
from operator import itemgetter

verbose = 2

if 0:
    tagsdata = os.popen('ctags -f - zoot.py')
    symbols = {}
    for name,path,_,rest in imap(lambda line: line.split('\t',3), tagsdata):
        symbols[name] = rest[0]     # symbol type
    # defs = list( imap(itemgetter(1), def_pat.findall(tagsdata)) )
    defs = symbols.keys()

defpat = re.compile(
    '(def|class)\s+(\w+)',
    )
# callpat = re.compile(
#     '(%s)\s*\(' % '|'.join(defs)
#     )
callpat = re.compile(
    # '(%s)\s*(\(.*\)?)',
    '([A-Za-z_][A-Za-z0-9_]*)\s*\(',
    )


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

PRIMLIST = set('set int len range reduce type __init__'.split())

class TagsFile(list):
    def append(self, path, tagdef, tagname, lineno, offset):
        self += (path, tagdef, tagname, lineno, offset)
    def writeto(self, outpath):
        outf = open(outpath, 'w')

def main(paths):
    tagsf = open('calls.tags', 'w')
    for path in paths:
        if not path.endswith('.py'):
            continue
        if '/lib/python' in path:
            continue
        print '%s:' % path
        source = open(path, 'r').read()
        print >> tagsf, '\f'
        print >> tagsf, '%s,%s' % (path, len(source))      # X?
        print >> tagsf, '\x7f%s\x011,0' % path      # X?

        in_def = None
        for num,line,begpos,_ in enum_pos(source.split('\n')):
            if verbose:
                print '%2d %3d %s' % (num+1, begpos, line)
            m = defpat.search(line)
            if m:
                in_def = m.group(2), num+1, begpos
                # print 'def', in_def
                continue
            for m in callpat.finditer(line):
                calls = m.group(1)
                if calls in PRIMLIST:
                    continue
                if not in_def:
                    continue    # XXX?
                if verbose:
                    print '- %s: calls %s%s' % (in_def[0], calls, '()')
                print >> tagsf, symref(
                    tagdef=line,
                    tagname=m.group(1),
                    lineno=num+1, 
                    offset=begpos,
                    )
    tagsf.close()

if __name__=='__main__':
    main(sys.argv[1:])
    # main([os.path.abspath('zoot.py')])
