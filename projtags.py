#!/usr/bin/env python2.6

import os, sys
from itertools import imap

os.chdir('../geodelic')

alldirs = [ line.strip() for line in 
            os.popen('/bin/ls -1 */*.py | xargs -n1 dirname | sort -u') ]
open('.alldirs','w').write('\n'.join(alldirs))

newfiles = [ line.strip() for line in 
             os.popen('find {alldirs} -cnewer TAGS -name "*.py"'.format(
            alldirs = ' '.join(alldirs)))
             ]
if not newfiles:
    sys.exit(0)
print 'Rebuilding'
ret = os.system('etags -o TAGS `find {alldirs} -name "*.py"`'.format(**locals()))
sys.exit(ret)
