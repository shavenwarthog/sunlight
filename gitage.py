#!/usr/bin/env python2.6

import os, re, sys
from datetime import datetime, timedelta
from itertools import ifilter, imap


WEEK = 7.
MONTH = 30.

def vc_info(path):
    # "git blame" format:
    # => hash (name date time tz lineno) line
    linepat = re.compile(
        '\S+ \((.+?)\s+([0-9: -]+?)\s-0800\s+([0-9]+)\).(.*)')
    return (m.groups() for m in ifilter(None, imap(linepat.match, os.popen('git blame %s'%path))))

def age_label(delt):
    if delt.days:
        if delt.days > MONTH:
            return '%.1fm' % (delt.days/MONTH)
        elif delt.days > WEEK:
            return '%.1fw' % (delt.days/WEEK)
        return '%2dd' % delt.days
    return '%.1f' % (delt.seconds/3600.)

def age_face(delt):
    if delt.days > WEEK:
        return 'vcage-oldest'
    elif delt.days:
        return 'vcage-old'
    return None

def calcrange(vc_iter):
    now = datetime.now()
    prev = None
    for name,datestr,lineno,line in vc_iter:
        if prev != datestr:
            date = datetime.strptime(datestr, '%Y-%m-%d %H:%M:%S')
            prev = datestr
        yield now-date, lineno, line

def main(path):
    for age_td,lineno,line in calcrange(vc_info(path)):
        if 0:
            print '%-3s %s' % (age_label(age_td), line)
        else:
            face = age_face(age_td)
            if face:
                print '(piemacs-ov :lineno %s :face \'%s)' % (lineno, face)

if __name__=='__main__':
    main(sys.argv[1])
