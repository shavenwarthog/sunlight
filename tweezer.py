#!/usr/bin/env python

'''
tweezer.py -- run a function, log all external calls and return values
'''
import inspect
import logging
import sys
import trace

logging.basicConfig(stream=sys.stderr, level=logging.DEBUG)

def code_ignore(code):
    return 'lib/python' in code.co_filename

def locvars_large(loc):
    return loc.has_key('__builtins__') or len(loc) > 30

def fmt_locals(frame):
    loc =  frame.f_locals
    if loc.has_key('__builtins__') or len(loc) > 30:
        loc = '<large>'
    del frame
    return 'locals={0}'.format(loc)

def fmt_line(frame):
    return '{0}:{1}'.format(frame.f_code.co_filename, frame.f_lineno)

class Report(list):
    pass


class Tweeze(trace.Trace):
    def __init__(self):
        trace.Trace.__init__(self, ignoredirs=[sys.prefix, sys.exec_prefix,])
        self.globaltrace = self.globaltrace_tracecalls2
        self.out = Report()
        # self.globaltrace = self.globaltrace_dumpstack

    def globaltrace_tracecalls(self, frame, why, arg):
        if why != 'call':
            return
        fparent,fcurrent = frame.f_back, frame
        if code_ignore(fparent.f_code) and code_ignore(fcurrent.f_code):
            return
        print '%s > %s %s' % (
            fmt_line(frame.f_back), fmt_line(frame), frame.f_code.co_name)
        print '  %s' % fmt_locals(fcurrent)
        del fparent, fcurrent
        # code = frame.f_code

    def globaltrace_tracecalls2(self, frame, why, arg):
        if why != 'call':
            return
        fparent,fcurrent = frame.f_back, frame
        if code_ignore(fparent.f_code) and code_ignore(fcurrent.f_code):
            return
        # print '{0}:{1}'.format(frame.f_code.co_filename, frame.f_lineno)
        if not fcurrent.f_locals or locvars_large(fcurrent.f_locals):
            return
        self.out.append([
            fcurrent.f_lineno,
            'vars',
            dict(fcurrent.f_locals),
            ])
        del fparent, fcurrent

    def globaltrace_dumpstack(self, frame, why, arg):
        if why != 'call':
            return
        if code_ignore(frame.f_code):
            return
        stack = list(inspect.getouterframes(frame))
        print ' - '.join( 
            (fname
             for fr,path,line,fname,_,_ 
             in reversed(stack[:2]))
            )

# # create a Trace object, telling it what to ignore, and whether to
# # do tracing or line-counting or both.
# tracer = trace.Trace(
#     ignoredirs=[sys.prefix, sys.exec_prefix],
#     trace=0,
#     count=1)

# run the new command using the given tracer
tracer = Tweeze()
tracer.run('from ex_tweezer import main ; main()')
for row in tracer.out:
    print row

if 1:
    r = tracer.results()
    r.write_results(show_missing=True, coverdir=".")
