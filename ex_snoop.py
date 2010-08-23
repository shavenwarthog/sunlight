
import sys

def sillyfunc(num, name='x'):
    num += 1
    return name*num

def test_sillyfunc():
    rep = 1
    res = sillyfunc(rep+1, name='john')
    return res

def test_pythonver():
    ver = sys.version
    
def test_tracelocals():
    a = 1
    longline = \
        'testme'
    john = 'stud'
    def inner(arg=None):
        xi = 2
        return xi + 1
    testfunc = inner(a)
    a += a
    out = john*a

def test_os_system():
    import os
    return os.popen('id').read()
