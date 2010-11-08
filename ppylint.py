#!/usr/bin/env python2.6

import os, re, sys
from itertools import ifilter, imap

# "ppylint.py:7: [E0001] invalid syntax"

pylint_pat = re.compile(
    '(.+?):(\d+):\s+'
    '.(.)(\d+)'
    '.*?\]'
    '\s(.+)'
    )

cmd = 'pylint'
path = sys.argv[1]
if 'geodelic' in os.path.abspath(path):
    cmd = '/home/johnm/src/geodelic/bin/pylint'

print '(piemacs-remove-overlays)'
for m in ifilter(None, imap(pylint_pat.match, 
              os.popen('%s --disable-msg-cat=c -fparseable -iy %s 2> /dev/null' % (cmd, path)))):
    mname,mline,mstatus,mcode,mmsg = m.groups()
    if mstatus != 'E':
        continue
    print '(piemacs-ov %s "%s")' % (mline, mmsg)
