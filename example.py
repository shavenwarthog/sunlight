from nose.tools import eq_ as eq


def testme():
    return 'zoot'

def test_okay():
    eq(1,1)

def test_boom():
    eq(testme(),'boom')
