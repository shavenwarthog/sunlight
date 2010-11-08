#!/usr/bin/env python2.6

import os, re, sys
from itertools import chain, ifilter, imap
# from nosetests import eq_ as eq

# "ppylint.py:7: [E0001] invalid syntax"

nosetests_command = 'nosetests --with-coverage -v 2>&1'
test_pat = re.compile('(.+?)\s\.\.\.\s(.+)')
# "test_xref.test_sleep ... ok"

coverage_pat = re.compile(
    '.*'
    '(?P<cover>	\d+)%\s+'
    '(?P<missing> .+)'
    , re.VERBOSE)

# Name    Stmts   Exec  Cover   Missing
# xref       86     24    27%   20, 32, 42, 45-59, 62-72, 75-112, 116-127, 131-143, 146


def p_test(testlabel, testresult):
    print ';; (piemacs-nosetest "%s" \'%s)' % (
        testlabel.split('.')[-1], 
        "piemacs-face-okay" if testresult=='ok' else "piemacs-face-error",
        )

def p_coverage(info):
    def mklines():
        for arg in (arg.strip() for arg in info['missing'].split(',')):
            if '-' in arg:
                yield arg.split('-')
            else:
                yield arg,arg
    return "(piemacs-ovs :lineranges '({0}) :face 'bold)".format(
        ' '.join(chain(*mklines())))

# "(piemacs-coverage-missing '({0}))".format(' '.join(chain(*mklines())))
def test_coverage():
    eq(p_coverage({'missing': '1 2-3'}), 'blam')

print '(piemacs-remove-overlays)'
for line in os.popen(nosetests_command + ' ' + sys.argv[1]):
    m = test_pat.match(line)
    if m:
        p_test(*m.groups())
        continue
    m = coverage_pat.match(line)
    if m:
        print p_coverage(m.groupdict())
