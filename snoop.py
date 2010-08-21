'''
snoop.py -- log local vars at end of unit test
'''

import logging, re, sys, tokenize
from nose import inspector
# .inspector import inspect_traceback, tbsource, find_inspectable_lines
from nose.inspector import Expander as nose_Expander

from nose.tools import eq_

logging.basicConfig(stream=sys.stderr, level=logging.DEBUG)


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
    def __init__(self, locals, globals):
        self.locals = locals
        self.globals = globals
        self.expanded_source = ''

    def value(self, varname):
        return self.locals.get(varname, self.globals.get(varname, self.NoValue))

    class NoValue(object):
        pass

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

