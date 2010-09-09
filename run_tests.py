#!/usr/bin/env python

import logging, os, re, sys, time
try:
    from nose.tools import eq_
except ImportError:
    pass

from asyncproc import Process

logging.basicConfig(stream=sys.stderr, level=logging.DEBUG,
                    format='%(asctime)s %(funcName)s %(message)s',
                    )

def eq(result, goodvalue):
    eq_( list(result), goodvalue )

def blockiter(proc):
    while True:
        if proc.wait(os.WNOHANG) != None:
            break
        block = proc.read()
        if block == '':
            time.sleep(0.25)
            continue
        yield block
    yield proc.read()

    

def test_nextblock():
    eq( blockiter(Process('echo 1; echo 2', shell=True)),
         ['1\n2\n'] 
         )
def test_nextblock2():
    eq( list(Readlines(Process('echo 1; sleep 1 ; echo 2', shell=True)).nextblock()),
         ['1\n', '2\n'],
         )
del test_nextblock2

def lineiter(blockiter):
    prev = ''
    for block in blockiter:
        lines = (prev+block).splitlines(True)
        if not lines:
            break
        prev = '' if (lines[-1].endswith('\n')) else lines.pop()
        for line in lines:    # XX gauche
            yield line
    if prev:
        yield prev

def test_lineiter():
    eq( lineiter(blockiter(Process('echo 1; echo 2', shell=True))),
         ['1\n', '2\n'],
         )

def get_cmd(args):
    cmd = "/home/johnm/src/geodelic/bin/run_tests -v 2 " + ' '.join(args)
    if 01:
        cmd += ' 2>&1'
    else:
        cmd += ' 2>/dev/null'
    return cmd


# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

class Timer(object):
    def __init__(self, formatstr):
        self.formatstr = formatstr
        self.start = time.time()
        self.task = None

    def sync(self):
        self.task = time.time()

    def format(self, formatstr=None):
        try:
            return (formatstr or self.formatstr).format(
                  elapsed=int(time.time()-self.start), # XX
                  el_task=time.time() - (self.task or self.start),
                  )
        finally:
            self.task = time.time()

def test_timer():
    t = Timer(formatstr='{elapsed:03} {el_task:.3f}')
    time.sleep(0.1)
    eq_( t.format(), '000 0.100')
    time.sleep(0.1)
    eq_( t.format(), '000 0.200')
del test_timer

# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

def main(args=None):
    timer = Timer(formatstr='{elapsed:3} {el_task:5.1f} ')
    outf = sys.stdout
    outf.write( timer.format('{elapsed:3} - run_tests.py\n') )

    if args is None:
        args = sys.argv[1:]
    print args
    boringp = re.compile(
        '(' +
        '|'.join(
                'Trying Adding Checking Creat Destroy Install Loading Process Running'
                .split()) +
        '|\\+'
        '|No \S+ fixture'
        '|No fixtures found'
        '|No custom'
        '|.+/bin/psql -d test_geodelic'
        # XX: weird:
        '|rience model$'
        '|.+geobuilder/field_att_mergers.sql'
        ')').match

    warnpat = re.compile(
         '^(.+\) \.\.\. )WARN.+$',
         )
    # "server.apps.geopoi.tests.t_banner.TestGeodelicImageBanner"
    # => 'geopoi', 'TestGeodelicImageBanner'
    simplifypat = re.compile('\S+\.apps\.(\S+)\.(\S+)\)')

    myProc = Process(get_cmd(args),
                     shell=True,
                     )
    outf.write( timer.format() + ' bin/run_tests\n' )
    timer.sync()

    for line in lineiter(blockiter(myProc)):
        if boringp(line):
            continue
        m = warnpat.match(line)
        if m:
            line = m.group(1)
        m = simplifypat.search(line)
        if m:
            line = re.sub(m.group(0), '%s.%s' % (m.group(1), m.group(2)), line)
        if ' ... ' in line: # startswith('test'):
            outf.write( timer.format() )
        outf.write(line)
    outf.write( timer.format('{elapsed:03}s end\n') )
    sys.exit( myProc.wait() / 256 ) # XX


if __name__=='__main__':
    main()

