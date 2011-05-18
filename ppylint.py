#!/usr/bin/env python2.6

import re, subprocess, sys
from itertools import ifilter, imap
try:
    from nose.tools import eq_ as eq
except ImportError:
    pass


PYLINT_RANK = 'CIRWEF'           # ordered by importance, last=highest


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

    def summary(self):
        out = []
        for rank in PYLINT_RANK:
            if not self.has_key(rank):
                continue
            out.append(rank + str(self[rank]))
        return ' '.join(out)

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
    eq( Stat({'C':1, 'W':2}).summary(), 'C1 W2' )

def print_iter(myiter):
    for obj in myiter:
        print obj,
        yield obj

class Pylint(object):
    def __init__(self, path):
        self.err = None
        self.pipe = None
        self.path = path
        # "sunfudge.py:1: [C0111, Fake] Missing docstring"
        self.pat = re.compile(
            '(.+?):(\d+):\s+'   # path, lineno
            '.(.)(\d+)'         # status, code
            '[ ,]*(.*?)\]'           # object (optional)
            '\s(.+)'            # message
            )

    # pylint: disable=W0612
    def cmd_iter(self):
        cmd = 'pylint'
        args = '--disable=C,I,R --include-ids=y --reports=n -fparseable'
        path = self.path
        self.pipe = subprocess.Popen(
            '{cmd} {args} {path}'.format(**locals()),
            shell=True, stdout=subprocess.PIPE,
            stderr=subprocess.PIPE)
        out, self.err = self.pipe.communicate()
        if not out:
            return iter([])
        return iter(str(out).split('\n'))

    def __iter__(self, lines=None):
        if lines is None:
            lines = self.cmd_iter()
        return (m.groups() for m in ifilter(None, imap(self.pat.match, lines)))


def test_parse():
    lint = Pylint(None)
    eq( list( lint.__iter__(lines='''
sunfudge.py:1: [C0111] Missing docstring
sunfudge.py:1: [C0111, Fake] Missing docstring
sunfudge.py:1: [E0602, Fake] Undefined variable 'fudge'
'''.split('\n')) ),
        [('sunfudge.py', '1', 'C', '0111', '', 'Missing docstring'), 
         ('sunfudge.py', '1', 'C', '0111', 'Fake', 'Missing docstring'), 
         ('sunfudge.py', '1', 'E', '0602', 'Fake', "Undefined variable 'fudge'"),
         ]
        )
        
# pylint: disable=W0612
def ppylint(path):
    stat = Stat()
    yield '(piemacs-remove-overlays)'
    pylint = Pylint(path)
    for _mname,mline,mstatus,mcode,_mobj,mmsg in pylint:
        stat.add(mline, mstatus)
        if mstatus in 'CRI': # or not stat.status_important(mline, mstatus):
            continue
        face = 'piemacs-pylint-error'
        if mcode:
            mmsg += ' [{mstatus}{mcode}]'.format(**locals())
        yield '(piemacs-ov :lineno {mline} :message "{mmsg}" :face \'{face})'.format(
            **locals())
    if not stat and pylint.pipe.returncode != 0:
        yield '(piemacs-status "pylint: err=%d: %s")' % (
            pylint.pipe.returncode, str(pylint.err).strip())
    else:
        yield '(piemacs-status "pylint: {0}")'.format(stat.summary())

def test_ppylint():
    errs = list(ppylint('sunfudge.py'))
    eq( errs[:2],
        ['(piemacs-remove-overlays)', 
         '(piemacs-ov :lineno 1 :message "Undefined variable \'fudge\' [E0602]"'
         ' :face \'piemacs-pylint-error)']
        )
    eq( errs[-1],  '(piemacs-status "pylint: E14")' )

def main(path):
    for line in ppylint(path):
        print line

if __name__=='__main__':
    main(sys.argv[1])
