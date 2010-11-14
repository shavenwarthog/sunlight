#!/usr/bin/env python2.6

import os, re, sys
from itertools import ifilter, imap
from nose.tools import eq_ as eq


PYLINT_RANK = 'CIRWEF'           # ordered by importance, last=highest

def summary(stat):
    out = []
    for rank in PYLINT_RANK:
        if not stat.has_key(rank):
            continue
        out.append(rank + str(stat[rank]))
    return ' '.join(out)

class Stat(dict):
    def add(self, mline, mstatus):
        self[mstatus] = self.get(mstatus,0) + 1 # overall status count 
        try:
            self[mline] = max(self.get(mline, -1), PYLINT_RANK.index(mstatus))
        except ValueError as exc:
            raise ValueError(mstatus+': '+str(exc))

    def status_important(self, mline, mstatus):
        "return True if status is higher priority that any other messages on this line"
        return PYLINT_RANK.index(mstatus) > self.get(mline, -1)

def test_stat_important():
    s = Stat()
    s.add('1', 'W')
    eq( s.status_important('1', 'F'), True )
    eq( s.status_important('1', 'W'), False )
    eq( s.status_important('1', 'C'), False )
    try:
        s.status_important('1', 'bogus')
        assert 0, 'Should raise ValueError'
    except ValueError:
        pass

def test_stat_summary():
    eq( summary( Stat({'C':1, 'W':2}) ), 'C1 W2' )

class Pylint(object):
    def __init__(self, path):
        self.path = path
        # "sunfudge.py:1: [C0111, Fake] Missing docstring"
        self.pat = re.compile(
            '(.+?):(\d+):\s+'   # path, lineno
            '.(.)(\d+)'         # status, code
            '(.*?)\]'           # object (optional)
            '\s(.+)'            # message
            )

    def __iter__(self, lines=None):
        if lines is None:
            cmd = 'pylint'
            if 'geodelic' in os.path.abspath(path):
                cmd = '/home/johnm/src/geodelic/bin/pylint'
            lines = os.popen('%s -fparseable -iy %s' % (cmd, self.path))
        return (m.groups() for m in ifilter(None, imap(self.pat.match, lines)))


def test_parse():
    lint = Pylint(None)
    eq( list( lint.__iter__(lines='''
sunfudge.py:1: [C0111] Missing docstring
sunfudge.py:1: [C0111, Fake] Missing docstring
sunfudge.py:1: [E0602, Fake] Undefined variable 'fudge'
'''.split('\n')) ),
        [('sunfudge.py', '1', 'C', '0111', '', 'Missing docstring'), 
         ('sunfudge.py', '1', 'C', '0111', ', Fake', 'Missing docstring'), 
         ('sunfudge.py', '1', 'E', '0602', ', Fake', "Undefined variable 'fudge'"),
         ]
        )
        


def ppylint(path):
    _cmd(path)


    stat = Stat()
    yield '(piemacs-remove-overlays)'
    lines = os.popen('%s -fparseable -iy %s' % (cmd, path))
    for m in ifilter(None, imap(pylint_pat.match, lines)):
        mname,mline,mstatus,mcode,mmsg = m.groups()
        stat.add(mline, mstatus)
        if mstatus in 'C':
            continue
        if not stat.status_important(mline, mstatus):
            continue
        face = 'piemacs-pylint-error'
        yield '(piemacs-ov :lineno %s :message "%s" :face \'%s)' % (
            mline, mmsg, face)

    yield '(piemacs-status "%s")' % summary(stat)

def main(path):
    for line in ppylint(path):
        print line

if __name__=='__main__':
    main(sys.argv[1])
