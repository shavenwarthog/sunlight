#!/usr/bin/env python2.6

import os, re, sys
from itertools import ifilter, imap


def summary(stat):
    return ' '.join( (mstatus+str(len(hits))
                      for mstatus,hits in stat.iteritems()) )

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

    stat = {}
    print '(piemacs-remove-overlays)'
    lines = os.popen('%s -fparseable -iy %s' % (cmd, path))
    for m in ifilter(None, imap(pylint_pat.match, lines)):
        mname,mline,mstatus,mcode,mmsg = m.groups()
        stat.setdefault(mstatus, []).append('')
        if mstatus in 'C':
            continue
        face = 'piemacs-pylint-error'
        print '(piemacs-ov :lineno %s :message "%s" :face \'%s)' % (
            mline, mmsg, face)

    print '(piemacs-status "%s")' % summary(stat)

if __name__=='__main__':
    main(sys.argv[1])
