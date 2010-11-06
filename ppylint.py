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

print '(piemacs-remove-overlays)'
for m in ifilter(None, imap(pylint_pat.match, 
              os.popen('pylint --disable-msg-cat=c -fparseable -iy %s 2> /dev/null' % sys.argv[1]))):
    mname,mline,mstatus,mcode,mmsg = m.groups()
    if mstatus != 'E':
        continue
    print '(piemacs-ov %s "%s")' % (mline, mmsg)
