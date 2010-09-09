'''
snoop.py -- log local vars at end of unit test
'''

import copy, inspect, keyword, logging, os, sys, tokenize, trace
from nose.tools import eq_

logging.basicConfig(stream=sys.stderr, level=logging.DEBUG)


class TraceLocals(trace.Trace):
    def __init__(self):
        trace.Trace.__init__(self)
        self.globaltrace = self.globaltrace_tl
        self.verbose = False
        self.outvars = None
        self.lines = None
        self.linerange = None
        self.filename = None

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
    if 0:
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
         [(16, 4, 20, 'test_tracelocals', NoValue), 
          (17, 4, 5, 'a', 2),
          ]
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
    # print 'LOGWRAP:',obj
    return WrapModule(deleg=obj)
    
def test_traceglobals():
    t = TraceLocals()
    _mod = __import__('ex_snoop')
    _mod.sys = logwrap(sys)
    t.runfunc(_mod.test_pythonver)
    assert list(tvars_used(t))[1][-1].startswith('2.6.') 

def test_ctxglobals():
    t = TraceLocals()
    _mod = __import__('ex_snoop')
    t.runctx(_mod.test_pythonver.func_code, globals=dict(sys=sys) )
    assert list(tvars_used(t))[1][-1].startswith('2.6.') 

def test_tvars():
    t = TraceLocals()
    _mod = __import__('ex_snoop')
    t.runctx(_mod.test_sillyfunc.func_code,
             globals={'sillyfunc':_mod.sillyfunc},
             )
    out = list(tvars_used(t))
    if 0:
        print '\n'.join( (str(row) for row in out) )
    eq_( out, [
            (8, 4, 18, 'test_sillyfunc', NoValue),
            (9, 4, 7, 'rep', 1), 			(10, 4, 7, 'res', 'johnjohnjohn')
            , (10, 10, 19, 'sillyfunc', NoValue), 	(10, 20, 23, 'rep', 1),
            (10, 27, 31, 'name', NoValue), 		(11, 11, 14, 'res', 'johnjohnjohn'),
            ] )

def remap(info):
    # cadged from json library:
    BASICTYPES = (str, unicode, int, long, float, bool, None) 
    lineno, varst, varend, varname, value = info
    if value is NoValue:
        return None 
    if type(value) not in BASICTYPES:
        value = repr(value) # ?
    return [lineno, 'varvalue', [varst, varend, varname, value]]


def checkpath(path, srcpath=None):
    """Run each test_method(), capturing local variables of each.
    """
    modname = os.path.basename( os.path.splitext(path)[0] ) # XX
    logging.error(modname)
    _mod = __import__(modname)                              # XX
    info = []
    for fname, fobj in _mod.__dict__.iteritems():
        if not (fname.startswith('test_') and inspect.isfunction(fobj)):
            continue
        # XX: copy module before possibly modifying it?
        t = TraceLocals()
        t.runctx( fobj.func_code, globals=_mod.__dict__ )
        info += tvars_used(t)

    return filter(None, [ remap(rec) for rec in info ])
    
    
def test_checkpath():
    res = checkpath('ex_snoop.py')
    if 01:
        print '\n'.join( (str(row) for row in res) )
    eq_( len(res), 19 )
    eq_( res[0], [9, 'varvalue', [4, 7, 'rep', 1]] )
    eq_( res[-2], [26, 'varvalue', [15, 16, 'a', 2]] )
