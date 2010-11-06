# nosetests test_xref.py

import time
from nose.tools import eq_ as eq
from xref import c_shouldskip, enum_pos

def test_callname_shouldskip():
    eq( c_shouldskip('__argh__'), True )
    eq( c_shouldskip('beer'), False )

def test_enum_pos():
    eq( list(enum_pos('tasty beer'.split(' '))),
        [(0, 'tasty', 0, 6), (1, 'beer', 6, 11)],
        )

# def test_sleep():
#     time.sleep(1)
