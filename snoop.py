'''
snoop.py -- log local vars at end of unit test
'''

import copy, inspect, keyword, logging, sys, tokenize
from nose import inspector
from nose.inspector import Expander as nose_Expander

from nose.tools import eq_

logging.basicConfig(stream=sys.stderr, level=logging.DEBUG)


# :::::::::::::::::::::::::::::::::::::::::::::::::: USING EXCEPTION + EXPANDER

def test_silly():
    a = 1
    john = 'stud'
    a += a
    out = john*a
    eq_( out, 'studstud' )
    assert 0,'woo'


class MyExpander(nose_Expander):
    def __call__(self, ttype, tok, start, end, line):
        logging.info('exp: %s', [ttype, tok, start, end, line])
        oldsrc = self.expanded_source
        nose_Expander.__call__(self, ttype, tok, start, end, line)
        newsrc = self.expanded_source 
        logging.info('+ %s',newsrc[len(oldsrc):])

class MyExpander2(object):
    class NoValue(object):
        pass

    def __init__(self, localvars, globalvars):
        self.locals = localvars
        self.globals = globalvars
        self.expanded_source = ''

    def value(self, varname):
        return self.locals.get(varname, self.globals.get(varname, self.NoValue))

    def __call__(self, ttype, tok, start, end, line):
        if ttype != tokenize.NAME:
            return
        val = self.value(tok)
        if 0:
            logging.debug('\t%s', [ttype, tok, start, end, line, val, type(val).__name__])
        if val is self.NoValue or type(val).__name__ in ['function',]:
            return
        logging.info(':%d:%d-%d %s %s', start[0], start[1], end[1], tok, val)

inspector.Expander = MyExpander2 # XX monkeypatch


def test_detail():
    excinfo = None
    try:
        test_silly()
    except AssertionError:
        excinfo = sys.exc_info()
    tb = excinfo[2]
    print 'traceback', tb
    
    tbinfo = inspector.inspect_traceback(tb)
    print tbinfo

# :::::::::::::::::::::::::::::::::::::::::::::::::: USING TRACE
import trace

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

class TraceLocals(trace.Trace):
    def __init__(self):
        trace.Trace.__init__(self)
        self.globaltrace = self.globaltrace_tl
        self.verbose = False
        self.outvars = None
        self.lines = None
        self.linerange = None

    def globaltrace_tl(self, frame, why, arg):
        if self.verbose:
            print frame,why,arg
        return self.localtrace_tl

    def localtrace_tl(self, frame, why, arg):
        if self.verbose:
            print '  ', frame,why,arg
            print '\t',sorted(frame.f_locals.iteritems())
        if why == 'return':
            self.outvars = copy.copy(frame.f_locals)
            lines,lineno = inspect.findsource(frame)
            self.lines = lines
            self.linerange = lineno, frame.f_lineno
        return self.localtrace_tl

def test_vartrace():
    t = TraceLocals()
    t.runfunc(test_tracelocals)
    del t.outvars['inner']   # a function
    eq_( t.outvars, {'a': 2, 'john': 'stud', 'testfunc': 3, 'out': 'studstud'} )
    print
    print '-'.join(t.lines[t.linerange[0]:t.linerange[1]])


class NoValue(object):
    pass

def vars_used(localvars, source, linest, lineend):
    def get(name):
        return localvars.get(name, NoValue)

    region = source[linest : lineend]
    for ttype, tok, start, end, _ in tokenize.generate_tokens(
        readline=lambda: region.pop(0) if region else ''
        ):
        if not (ttype == tokenize.NAME and not keyword.iskeyword(tok)):
            continue
        yield (start[0], start[1], end[1], tok, get(tok))



def test_annotate():
    t = TraceLocals()
    t.runfunc(test_tracelocals)
    eq_( list(vars_used(
        localvars=t.outvars,
        source=t.lines,
        linest=t.linerange[0],
        lineend=t.linerange[1],
        ))[:2],
         [(1, 4, 20, 'test_tracelocals', NoValue), 
          (2, 4, 5, 'a', 2),
          ]
         )

