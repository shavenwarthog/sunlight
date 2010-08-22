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
            print 'load',name
            if name not in sys.modules:
                _mapper.LoadModule(path, name)
                module = _mapper.GetModule(name)
                module.__file__ = path
                sys.modules[name] = __import__(name)
                if '.' in name:
                    parent_name, child_name = name.rsplit('.', 1)
                    setattr(sys.modules[parent_name], child_name, module)
            return sys.modules[name]
    return Loader()

class MetaImporter(object):
#    CAPTURE = ['ex_snoop']

    def find_module(self, fullname, path=None):
        print 'find %s %s' % (fullname, path)
        return loader(fullname)
        # lastname = fullname.rsplit('.', 1)[-1]
        # for d in (path or sys.path):
        #     py = os.path.join(d, lastname + '.py')
        #     if os.path.exists(py):
        #         return loader(py)

        # return None

def test_modglobals2():
    sys.meta_path = [MetaImporter()] # XX
    t = TraceLocals()
    print 'woo'
    try:
        _mod = __import__('ex_snoop')
        t.runctx(_mod.test_os_system.func_code, globals=dict(sys=sys))
    finally:
        pass
    # print 'globals: %s' % xglobals._log
    #     print 'locals: %s' % xlocals._log
#    assert list(tvars_used(t))[1][-1].startswith('2.6.5 ') 
    # eq_( 


# 


# http://nullege.com/codes/show/src@Python-2.6.4@Lib@test@test_importhooks.py/224/sys.path_hooks.append

class ImpLoader(object):
    def __init__(self, file, filename, stuff):
        self.file = file
        self.filename = filename
        self.stuff = stuff
  
    def load_module(self, fullname):
        mod = imp.load_module(fullname, self.file, self.filename, self.stuff)
        if self.file:
            self.file.close()
        mod.__loader__ = self  # for introspection
        # print 'load %s => %s, loader=%s' % (fullname,mod,mod.__loader__)
        return mod

class ImpWrapper(object):
    LoaderClass = ImpLoader
    def __init__(self, path=None):
        if path is not None and not os.path.isdir(path):
            raise ImportError
        self.path = path
  
    def find_module(self, fullname, path=None):
        # print 'find',fullname
        subname = fullname.split(".")[-1]
        if subname != fullname and self.path is None:
            return None
        if self.path is None:
            path = None
        else:
            path = [self.path]
        try:
            file, filename, stuff = imp.find_module(subname, path)
        except ImportError:
            return None
        return self.LoaderClass(file, filename, stuff)
  
    
def testImpWrapper():
    i = ImpWrapper()
    sys.meta_path.append(i)
    sys.path_hooks.append(ImpWrapper)
    mnames = ("colorsys", "urlparse", "distutils.core")
    for mname in mnames:
        parent = mname.split(".")[0]
        for n in sys.modules.keys():
            if n.startswith(parent):
                del sys.modules[n]
    for mname in mnames:
        m = __import__(mname, globals(), locals(), ["__dummy__"])
        assert hasattr(m,'__loader__'), m  # to make sure we actually handled the import

def testImpWrapper2():
    class LogModule(object):
        LOG = []                # class attribute

        def __init__(self, deleg):
            self._deleg = deleg

        def __getattribute__(self, key):
            GET = partial(object.__getattribute__, self)
            LOG = LogModule.LOG
            try:
                LOG.append( [self._deleg, key] )
                res = getattr(self._deleg, key)
            except Exception as exc:
                # print '- EXC:',self._deleg,exc
                LOG[-1].append( repr(exc) )
                raise
            # print '- res:',res
            LOG[-1].append( res )
            return res

    class ImpLoader2(ImpLoader):
        def load_module(self, fullname):
            mod = super(ImpLoader2,self).load_module(fullname)
            return LogModule(mod)

    i = ImpWrapper()
    i.LoaderClass = ImpLoader2
    sys.meta_path.append(i)
    sys.path_hooks.append(ImpWrapper)
    mnames = "ex_snoop", 'os'
    for mname in mnames:
        parent = mname.split(".")[0]
        for n in sys.modules.keys():
            if n.startswith(parent):
                del sys.modules[n]
    for mname in mnames:
        m = __import__(mname, globals(), locals(), ["__dummy__"])
        assert hasattr(m,'__loader__'), m  # to make sure we actually handled the import
    
    _mod = __import__('ex_snoop')
    t = TraceLocals()
    t.runctx(_mod.test_os_system.func_code)
    if 0:
        print 'VARS:'
        print '\n'.join( (str(row) for row in tvars_used(t)) )
    # assert list(tvars_used(t))[1][-1].startswith('2.6.5 ') 
    print 'LOG:'
    print '\n'.join( (str(row) for row in LogModule.LOG) )
