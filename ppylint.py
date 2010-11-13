#!/usr/bin/env python2.6

import os, re, sys
from itertools import ifilter, imap
from nose.tools import eq_ as eq


PYLINT_RANK = 'CRWEF'           # ordered by importance, last=highest

def summary(stat):
    return ' '.join( (mstatus+str(len(hits))
                      for mstatus,hits in stat.iteritems()) )

class Stat(dict):
    def add(self, mline, mstatus):
        self[mstatus] = self.get(mstatus,0) + 1 # overall status count 
        self[mline] = max(self.get(mline, -1), PYLINT_RANK.index(mstatus))

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

def main(path):
    # "ppylint.py:7: [E0001] invalid syntax"

    pylint_pat = re.compile(
        '(.+?):(\d+):\s+'
        '.(.)(\d+)'
        '.*?\]'
        '\s(.+)'
        )

    cmd = 'pylint'
    if 'geodelic' in os.path.abspath(path):
        cmd = '/home/johnm/src/geodelic/bin/pylint'
    # '%s --disable-msg-cat=c -fparseable -iy %s'

    stat = Stat()
    print '(piemacs-remove-overlays)'
    lines = os.popen('%s -fparseable -iy %s' % (cmd, path))
    for m in ifilter(None, imap(pylint_pat.match, lines)):
        mname,mline,mstatus,mcode,mmsg = m.groups()
        stat.append(mline, mstatus)
        if mstatus in 'C':
            continue
        if not stat.status_important(mline, mstatus):
            continue
        face = 'piemacs-pylint-error'
        print '(piemacs-ov :lineno %s :message "%s" :face \'%s)' % (
            mline, mmsg, face)

    print '(piemacs-status "%s")' % summary(stat)

if __name__=='__main__':
    main(sys.argv[1])
