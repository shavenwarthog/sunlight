#!/usr/bin/env python2.6

import os, re, sys
from itertools import chain, ifilter, imap
from nose.tools import eq_ as eq

# "ppylint.py:7: [E0001] invalid syntax"

nosetests_command = 'nosetests --with-coverage -v 2>&1'
test_pat = re.compile(
    '(?P<label>	.+?)'
    '\s\.\.\.\s'
    '(?P<status> .+)'
    , re.VERBOSE)

coverage_pat = re.compile(
    '.*'
    '(?P<cover>	\d+)%\s+'
    '(?P<missing> .+)'
    , re.VERBOSE)

# Name    Stmts   Exec  Cover   Missing
# xref       86     24    27%   20, 32, 42, 45-59, 62-72, 75-112, 116-127, 131-143, 146


def p_nosetest(info):
    return [ '(piemacs-nosetest "%s" \'%s)' % (
            info['label'].split('.')[-1], 
            "piemacs-face-okay" if info['status']=='ok' else "piemacs-face-error",
            )
             ]

def test_nosetest():
    info = test_pat.match("test_xref.test_sleep ... ok").groupdict()
    eq(p_nosetest(info),
       ['(piemacs-nosetest "test_sleep" \'piemacs-face-okay)']
       )
       

def p_coverage(info):
    def mklines():
        for arg in (arg.strip() for arg in info['missing'].split(',')):
            if '-' in arg:
                yield arg.split('-')
            else:
                yield arg,arg
    return [ "(piemacs-ovs :lineranges '({0})"
             " :face 'piemacs-coverage-missing)".format(
        ' '.join(chain(*mklines())))
             ]

def test_coverage():
    eq(p_coverage({'missing': '1, 2-3'}), 
       ["(piemacs-ovs :lineranges '(1 1 2 3) :face 'bold)"],
       )

if __name__=='__main__':
    print '(piemacs-remove-overlays)'
    for line in os.popen(nosetests_command + ' ' + sys.argv[1]):
        m = test_pat.match(line)
        if m:
            print ';;',line,
            print '\n'.join( p_nosetest(m.groupdict()) )
            continue
        m = coverage_pat.match(line)
        if m:
            print ';;',line,
            print  '\n'.join( p_coverage(m.groupdict()) )
