'''
snoop.py -- log local vars at end of unit test
'''

import copy, inspect, keyword, logging, sys, tokenize, trace
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

def test_traceglobals():
    t = TraceLocals()
    _mod = __import__('ex_snoop')
    t.runfunc(_mod.test_pythonver)
    assert list(tvars_used(t))[1][-1].startswith('2.6.5 ') 

def test_ctxglobals():
    t = TraceLocals()
    _mod = __import__('ex_snoop')
    t.runctx(_mod.test_pythonver.func_code, globals=dict(sys=sys) )
    assert list(tvars_used(t))[1][-1].startswith('2.6.5 ') 


import imp

class WrappingImporter(object):
    '''http://www.python.org/dev/peps/pep-0302/            
    '''
    def find_module(self, fullname, pathname=None):
        return self
    def load_module(self, fullname):
        print 'LOADING MODULE %s' % fullname
        ispkg, code = self._get_code(fullname)
        mod = sys.modules.setdefault(fullname, imp.new_module(fullname))
        mod.__file__ = "<%s>" % self.__class__.__name__
        mod.__loader__ = self
        if ispkg:
            mod.__path__ = []
        exec code in mod.__dict__
        return mod

def test_modglobals():
    t = TraceLocals()
    _mod = __import__('ex_snoop')
    xglobals = TraceDict(dict(sys=sys))
    xlocals = TraceDict(dict())
    try:
        t.runctx(_mod.test_pythonver.func_code, globals=xglobals, locals=xlocals)
    finally:
        print 'globals: %s' % xglobals._log
        print 'locals: %s' % xlocals._log
    assert list(tvars_used(t))[1][-1].startswith('2.6.5 ') 


# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: via numpy

# http://orestis.gr/blog/2008/12/20/python-import-hooks/

def loader(path):
    class Loader(object):
        def load_module(self, name):
            if name not in sys.modules:
                _mapper.LoadModule(path, name)
                module = _mapper.GetModule(name)
                module.__file__ = path
                sys.modules[name] = module
                if '.' in name:
                    parent_name, child_name = name.rsplit('.', 1)
                    setattr(sys.modules[parent_name], child_name, module)
            return sys.modules[name]
    return Loader()

class MetaImporter(object):
    def find_module(self, fullname, path=None):
        if fullname == 'numpy' or fullname.startswith('numpy.'):
            _mapper.PerpetrateNumpyFixes()
        if fullname in ('_hashlib', 'ctypes'):
            raise ImportError('%s is not available in ironclad yet' % fullname)

        lastname = fullname.rsplit('.', 1)[-1]
        for d in (path or sys.path):
            pyd = os.path.join(d, lastname + '.pyd')
            if os.path.exists(pyd):
                return loader(pyd)

        return None

def test_modglobals2():
    eq_( 
