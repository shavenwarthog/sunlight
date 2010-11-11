#!/usr/bin/env python2.6

import os, re, sys
from itertools import ifilter, imap
from nose.tools import eq_ as eq

# FAIL: testRegi (openpub.apps.users.tests.big.RegTest)
stat_pat = re.compile(
    '(?P<status>FAIL):\s+(?P<function>\S+)\s+\((?P<modulepath>.+?)\)'
    )

class Traceback(dict):
    def __init__(self, *args, **kwargs):
        super(Traceback,self).__init__(*args, **kwargs)
        self['stack'] = []
        self.statefunc = self.s_start
        self.tb_line_pat = re.compile('\S+File "(.+?)", line (\d+), in (\S+)')

    def s_header(self, line):
        if line.startswith('Traceback '):
            return self.s_stack
    s_start = s_header

    def s_stack(self, line):
        m = self.tb_line_pat.match(line)
        if m:
            self['stack'].append(m.groups())
            return
        if not line.startswith(' '):
            self['exception'] = [line]
            return self.s_capture

    def s_capture(self, line):
        if line.startswith('-----'):
            return self.s_start
        self['exception'].append(line)

    def append(self, line):
        newstatefunc = self.statefunc(line)
        if newstatefunc:
            self.statefunc = newstatefunc
            return (newstatefunc == self.s_start)     # parse done

def test_traceback():
    lines = ['FAIL: beer: (parent.mod)', 'Traceback ',
             ' File "/path.py", line 1, in testFunc',
             ' detail1',
             'AssertionError:',
             'detail',
             '-'*20]
    tb = Traceback()
    for line in lines:
        tb.append(line)
        print tb

def main(path):
    lines = open('unittest-err.txt')
    tracebacks = []
    tb = None
    for line in lines:
        if tb:
            done = tb.append(line)
            if done:
                tracebacks.append(tb)
                tb = None
        m = stat_pat.match(line)
        if m:
            tb = Traceback(m.groupdict())

#    print '(piemacs-status "%s")' % summary(stat)

if __name__=='__main__':
    main(sys.argv[1])
