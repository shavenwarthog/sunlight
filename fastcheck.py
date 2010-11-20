#!/usr/bin/env python2.6

import glob, itertools, sys, time, token, tokenize
from nose.tools import eq_ as eq

def fastcheck(source):
    try:
        code = compile(source, '<string>', 'exec')
        # print code
        return
    except SyntaxError, exc:
        # print repr(exc)
        msg,(_,errline,errpos,src) = exc
        del exc
        del source
        del _
        return locals()
        # return '(fastcheck-err %s %s "%s")' % (errline, errpos, msg)

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

def test_big():
    source = open('../flynote/big.py').read()
    loops = 10
    t = time.time()
    for _ in range(loops):
        fastcheck(source)
    elapsed = time.time()-t
    print int((4734*loops) / elapsed), 'lines per second'

if 0:
    def test_timer():
        t = timeit.Timer('fastcheck()',
                         'from __main__ import fastcheck')
        print t.timeit(number=100000)
