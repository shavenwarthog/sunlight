#!/home/johnm/local/bin/python2.6

'''
xref.py -- postprocess Etags database into call graph
'''

import optparse, os, re, sys
from itertools import ifilterfalse


IGNORELIST = frozenset(
    'format int len range reduce set super type __init__'.split()
    )

# etags format:
#	{tag_definition_text}<\x7f>{tagname}
#		<\x01>{line_number},{byte_offset}
#
def symref(tagdef, tagname, lineno, offset):
    return '%s\x7f%s\x01%d,%d' % (tagdef, tagname, lineno, offset)

def enum_pos(lines):
    pos = 0
    for num,line in enumerate(lines):
        nextpos = pos+len(line)+1
        yield num, line, pos,nextpos
        pos = nextpos


def c_shouldskip(callname):
    if callname in IGNORELIST:
        return True
    magicpat = re.compile('^__[^_]+__$')
    if magicpat.match(callname):
        return True
    return False



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

def get_srcpaths(paths, verbose):
    def boringpath(path):
        return (not path.endswith('.py')) or ('/lib/python' in path)

    dirs = set()
    for path in ifilterfalse(boringpath, paths):
        if verbose:
            pathdir = os.path.dirname(path)
            if pathdir not in dirs:
                print pathdir
                dirs.add(pathdir)
        yield path

def main(argv):
    parser = optparse.OptionParser()
    parser.add_option(
        "-f", "--file", dest="filename",
        help="write report to FILE", metavar="FILE", 
        default='callers.tags',
        )
    parser.add_option(
        "-v", "--verbose",
        action="store_true", dest="verbose", 
        default=False,
        )

    (options, paths) = parser.parse_args()
    if options.filename == '-':
        options.filename = '/dev/stdout'

    tagsf = TagsFile()
    testf = TagsFile()

    for path in get_srcpaths(paths, options.verbose):
        source = open(path, 'r').read()
        for num, callee, caller, begpos in src_callers(
            source=source, verbose=options.verbose):
            if options.verbose:
                print '- %s calls %s' % (
                    caller[0].groups(), callee)
            tagsf.append(
                path=path,
                tagdef=caller[0].group(0),
                tagname=callee,
                lineno=num+1, 
                offset=begpos,
                )
    tagsf.writeto( options.filename )


def src_callers(source, verbose):
    callpat = re.compile(
        # '(%s)\s*(\(.*\)?)',
        '([A-Za-z_][A-Za-z0-9_]*)\s*\(',
        )
    for line,num,caller,begpos in sourcelines(source, verbose):
        if not caller:
            continue    # XXX?
        for m in callpat.finditer(line):
            callee = m.group(1)
            if c_shouldskip(callee):
                continue
            yield num, callee, caller, begpos


def sourcelines(source, verbose):
    defpat = re.compile(
        '^\s* (def|class) \s+ (\w+)',
        re.VERBOSE,
        )
    caller = None
    for num,line,begpos,_ in enum_pos(source.split('\n')):
        if verbose:
            print '%2d %3d %s' % (num+1, begpos, line)
        m = defpat.match(line) # XX
        if m:
            caller = m, num+1, begpos
            continue
        yield line, num, caller, begpos

if __name__=='__main__':
    main(sys.argv)
