#!/home/johnm/local/bin/python2.6

'''
xref.py -- postprocess Etags database into call graph
'''

import optparse, os, re, sys
from itertools import imap
from nose.tools import eq_ as eq
from operator import itemgetter


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


def callname_shouldskip(callname):
    IGNORELIST = set('format int len range reduce set super type __init__'.split())
    if callname in IGNORELIST:
        return True
    magicpat = re.compile('^__[^_]+__$')
    if magicpat.match(callname):
        return True
    return False

def test_callname_shouldskip():
    eq( callname_shouldskip('__argh__'), True )
    eq( callname_shouldskip('beer'), False )



class TagsFile(list):
    def append(self, path, tagdef, tagname, lineno, offset):
        super(TagsFile,self).append( (path, tagdef, tagname, lineno, offset) )

    def writeto(self, outpath):
        outf = open(outpath, 'w')
        lastpath = None
        for (path, tagdef, tagname, lineno, offset) in self:
            if path is not lastpath:
                print >> outf, '\f'
                print >> outf, '%s,%s' % (path, 0) # XX?
                print >> outf, '\x7f%s\x011,0' % path      # XX?
                lastpath = path
            print >> outf, symref(
                    tagdef=tagdef, 
                    tagname=tagname,
                    lineno=lineno,
                    offset=offset,
                    )
        outf.close()


def main(argv):
    parser = optparse.OptionParser()
    parser.add_option("-f", "--file", dest="filename",
                      help="write report to FILE", metavar="FILE", default='xref.dat',
                      )
    parser.add_option("-v", "--verbose",
                      action="store_true", dest="verbose", default=False,
                      )

    (options, paths) = parser.parse_args()
    if options.filename == '-':
        options.filename = '/dev/stdout'
    defpat = re.compile(
        '^\s* (def|class)\s+(\w+)',
        re.VERBOSE,
        )
    callpat = re.compile(
        # '(%s)\s*(\(.*\)?)',
        '([A-Za-z_][A-Za-z0-9_]*)\s*\(',
        )

    tagsf = TagsFile()
    testf = TagsFile()
    dirs = set()
    for path in paths:
        if not path.endswith('.py'):
            continue
        if '/lib/python' in path:
            continue
        pathdir = os.path.dirname(path)
        if options.verbose:
            if pathdir not in dirs:
                print pathdir
                dirs.add(pathdir)
        source = open(path, 'r').read()

        in_def = None
        for num,line,begpos,_ in enum_pos(source.split('\n')):
            if options.verbose:
                print '%2d %3d %s' % (num+1, begpos, line)
            m = defpat.match(line) # XX
            if m:
                in_def = m.group(2), num+1, begpos
                continue
            for m in callpat.finditer(line):
                callname = m.group(1)
                if callname_shouldskip(callname):
                    continue
                if not in_def:
                    continue    # XXX?
                if options.verbose:
                    print '- %s: callname %s%s' % (in_def[0], callname, '()')
                tagsf.append(
                    path=path,
                    tagdef=line,
                    tagname=m.group(1),
                    lineno=num+1, 
                    offset=begpos,
                    )
    tagsf.writeto( options.filename )

if __name__=='__main__':
    main(sys.argv)
