'''
snoop.py -- log local vars at end of unit test
'''

import copy, inspect, keyword, logging, os, sys, tokenize, trace
# from functools import partial
from nose.tools import eq_

logging.basicConfig(stream=sys.stderr, level=logging.DEBUG)


class TraceLocals(trace.Trace):
    def __init__(self):
        trace.Trace.__init__(self)
        self.globaltrace = self.globaltrace_tl
        self.verbose = True
        self.outvars = None
        self.lines = None
        self.linerange = None

    def globaltrace_tl(self, frame, why, arg):
        if self.verbose:
            print frame,why,(arg or '')
        return self.localtrace_tl

    def localtrace_tl(self, frame, why, arg):
        if self.verbose:
            print '  %2d %-6s %s %s' % (
                frame.f_lineno, why, (arg or ''),
                sorted(frame.f_locals.iteritems())
                )
        if why == 'return':
            # XX: following *replaces* vars/source
            # => only valid for outermost function traced
            self.outvars = copy.copy(frame.f_locals)
            lines,lineno = inspect.findsource(frame)
            self.filename = frame.f_code.co_filename
            self.lines = lines
            self.linerange = lineno, frame.f_lineno
        return self.localtrace_tl

def test_vartrace():
    t = TraceLocals()
    _mod = __import__('ex_snoop')
    t.runfunc(_mod.test_tracelocals)
    del t.outvars['inner']   # a function
    eq_( t.outvars, {'a': 2, 'longline': 'testme', 'john': 'stud', 'testfunc': 3, 'out': 'studstud'} )
    print
    print '-'.join(t.lines[t.linerange[0]:t.linerange[1]])


class NoValue(object):
    pass

def vars_used(localvars, source, linest, lineend):
    def get(name):
        return localvars.get(name, NoValue)

    region = source[linest : lineend]
    for ttype, tok, start, end, _ in tokenize.generate_tokens(
        readline=iter(region).next,
        ):
        if not (ttype == tokenize.NAME and not keyword.iskeyword(tok)):
            continue
        yield (start[0]+linest, start[1], end[1], tok, get(tok))

def tvars_used(t):
    return vars_used(
        localvars=t.outvars,
        source=t.lines,
        linest=t.linerange[0],
        lineend=t.linerange[1],
        )

def test_annotate():
    t = TraceLocals()
    _mod = __import__('ex_snoop')
    t.runfunc(_mod.test_tracelocals)
    eq_( list(vars_used(
        localvars=t.outvars,
        source=t.lines,
        linest=t.linerange[0],
        lineend=t.linerange[1],
        ))[:2],
         [(6, 4, 20, 'test_tracelocals', NoValue), 
          (7, 4, 5, 'a', 2)]
         )

LOG = []                        # global XX

class WrapFunc(object):
    pass

class WrapModule(object):
    def __init__(self, deleg):
        self._deleg = deleg

    def __getattribute__(self, key):
        if key.startswith('_'):
            return object.__getattribute__(self, key)
        global LOG
        LOG.append( [key] )
        res = getattr(self._deleg, key)
        LOG[-1].append( res )
        return res

def logwrap(obj):
    print 'LOGWRAP:',obj
    return WrapModule(deleg=obj)
    
def test_traceglobals():
    t = TraceLocals()
    # t.verbose = True
    _mod = __import__('ex_snoop')
    _mod.sys = logwrap(sys)
    t.runfunc(_mod.test_pythonver)
    assert list(tvars_used(t))[1][-1].startswith('2.6.5 ') 

def test_ctxglobals():
    t = TraceLocals()
    _mod = __import__('ex_snoop')
    t.runctx(_mod.test_pythonver.func_code, globals=dict(sys=sys) )
    assert list(tvars_used(t))[1][-1].startswith('2.6.5 ') 

def test_tvars():
    t = TraceLocals()
    _mod = __import__('ex_snoop')
    t.runctx(_mod.test_sillyfunc.func_code,
             globals={'sillyfunc':_mod.sillyfunc},
             )
    out = list(tvars_used(t))
    print '\n'.join( (str(row) for row in out) )


def checkpath(path, srcpath=None):
    # cadged from json library:
    BASICTYPES = (str, unicode, int, long, float, bool, None) 
    t = TraceLocals()
    _mod = __import__('ex_snoop')
    t.runctx(_mod.test_sillyfunc.func_code,
             globals={'sillyfunc':_mod.sillyfunc},
             )
    def remap(info):
        lineno, varst, varend, varname, value = info
        if value is NoValue:
            return None 
        if type(value) not in BASICTYPES:
            value = repr(value) # ?
        return lineno, 'varvalue', [varst, varend, varname, value]

    return filter(None, [ remap(info) for info in tvars_used(t) ])
    
    
def test_checkpath():
    res = checkpath(None)
    print '\n'.join( (str(row) for row in res) )
