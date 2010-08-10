#!/usr/bin/env python

'''
tweezer.py -- run a function, log all external calls and return values
'''
import sys
import logging
import trace

logging.basicConfig(stream=sys.stderr, level=logging.DEBUG)

def code_ignore(path):
    return 'lib/python' in path

class Tweeze(trace.Trace):
    def __init__(self):
        trace.Trace.__init__(self, ignoredirs=[sys.prefix, sys.exec_prefix,])
        self.globaltrace = self.globaltrace_tracecalls

    def globaltrace_tracecalls(self, frame, why, arg):
        if why != 'call' or code_ignore(frame.f_code.co_filename):
            return
        loc =  frame.f_locals
        if loc.has_key('__builtins__'):
            loc = '<large>'
        logging.info('%s', [frame.f_code, loc])
    
# # create a Trace object, telling it what to ignore, and whether to
# # do tracing or line-counting or both.
# tracer = trace.Trace(
#     ignoredirs=[sys.prefix, sys.exec_prefix],
#     trace=0,
#     count=1)

# run the new command using the given tracer
tracer = Tweeze()
tracer.run('from ex_tweezer import main ; main()')

r = tracer.results()
r.write_results(show_missing=True, coverdir=".")
