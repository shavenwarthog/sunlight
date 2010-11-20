#!/usr/bin/env python2.6

import glob, itertools, re, sys, time, token, tokenize
from nose.tools import eq_ as eq

def fastcheck(source, filename='<string>'):
    try:
        compile(source, filename, 'exec')
        return
    except SyntaxError, exc:
        # print repr(exc)
        msg,(_,errline,errpos,src) = exc
        del exc
        del source
        del _
        return locals()

def test_eof():
    eq( fastcheck('x='), 
        {'errline': 1, 'msg': 'unexpected EOF while parsing', 'errpos': 2, 'src': 'x='})
    eq( fastcheck(' x=5'), 
        {'errline': 1, 'src': ' x=5', 'errpos': 1, 'msg': 'unexpected indent'} )
    eq( fastcheck('x=5\nif 1: y=3'), None)
    eq( fastcheck('x=5\n y=3'),
        {'errline': 2, 'src': ' y=3', 'errpos': 1, 'msg': 'unexpected indent'})
    eq( fastcheck('x=5)'), 
        {'errline': 1, 'src': 'x=5)', 'errpos': 4, 'msg': 'unexpected EOF while parsing'})

def test_time():
    source = open('../flynote/big.py').read()
    loops = 10
    t = time.time()
    for _ in range(loops):
        fastcheck(source)
    elapsed = time.time()-t
    print int((4734*loops) / elapsed), 'lines per second'

def check():
    source = sys.stdin.read()
    res = fastcheck(source, filename='<stdin>')
    if res:
        print '(fastcheck-err {errline} {errpos} "{msg}")'.format(**res)

# http://pymacs.progiciels-bpi.ca/pymacs.html
# ">2\t.\n"  length = data plus newline

cmdpat = re.compile('^>(\d+)\t(.+\n)')

def server(fd):
    for line in fd:
        print line
        if not line.startswith('>'):
            print '?',line,
            continue
        m = cmdpat.match(line)
        if not m:
            print 'cmd?', line,
            continue
        total = int(m.group(1))
        source = [m.group(2)]
        remaining = total - len(source[0])
        if remaining:
            source.append( fd.read(remaining) )
        print "yay: size=%d, source=%s" % (total, ''.join(source))

if __name__=='__main__':
    server(sys.stdin)

