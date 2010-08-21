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


def var_value(tok, locals, globals):
    # Clean this junk up
    try:
        val = locals[tok]
        if callable(val):
            val = tok
        else:
            val = repr(val)
    except KeyError:
        try:
            val = globals[tok]
            if callable(val):
                val = tok
            else:
                val = repr(val)
        except KeyError:
            val = tok
    return val


class MyExpander(nose_Expander):
    def __call__(self, ttype, tok, start, end, line):
        logging.info('exp: %s', [ttype, tok, start, end, line])
        oldsrc = self.expanded_source
        nose_Expander.__call__(self, ttype, tok, start, end, line)
        newsrc = self.expanded_source 
        logging.info('+ %s',newsrc[len(oldsrc):])

class MyExpander2(object):
    """Simple expression expander. Uses tokenize to find the names and
    expands any that can be looked up in the frame.
    """
    def __init__(self, locals, globals):
        self.locals = locals
        self.globals = globals
        self.lpos = None
        self.expanded_source = ''
         
    def __call__(self, ttype, tok, start, end, line):
        if ttype != tokenize.NAME:
            return
        val = var_value(tok, self.locals, self.globals)
        logging.info('%s = %s', tok, val)

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

