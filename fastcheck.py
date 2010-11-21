#!/usr/bin/env python2.6

import glob, itertools, logging, optparse, re, sys, time, token, tokenize
from nose.tools import eq_ as eq

logging.basicConfig(
    filename='/tmp/fastcheck.log', 
    level=logging.DEBUG,
    format="%(asctime)-15s %(levelname)s %(message)s",
    )

def fastcheck(source, filename='<string>'):
    try:
        logging.info("< %d bytes: %s", len(source), source[:50])
        compile(source, filename, 'exec')
        logging.info("> ok")
        return
    except SyntaxError, exc:
        # print repr(exc)
        msg,(_,errline,errpos,src) = exc
        del exc
        del source
        del _
        logging.info("> err: %s", locals())
        return locals()

def test_eof():
    eq( fastcheck('x='), 
        {'errline': 1, 'msg': 'unexpected EOF while parsing', 'errpos': 2, 'src': 'x='})
    eq( fastcheck(' x=5'), 
        {'errline': 1, 'src': ' x=5', 'errpos': 1, 'msg': 'unexpected indent'} )
    eq( fastcheck('x=5\nif 1: y=3'), None)
    eq( fastcheck('x=5\n y=3'),
        {'errline': 2, 'src': ' y=3', 'errpos': 1, 'msg': 'unexpected indent'})
    eq( fastcheck('x=5)'), 
        {'errline': 1, 'src': 'x=5)', 'errpos': 4, 'msg': 'unexpected EOF while parsing'})

def test_time():
    source = open('../flynote/big.py').read()
    loops = 10
    t = time.time()
    for _ in range(loops):
        fastcheck(source)
    elapsed = time.time()-t
    print int((4734*loops) / elapsed), 'lines per second'

def check():
    source = sys.stdin.read()
    res = fastcheck(source, filename='<stdin>')
    if res:
        print '(fastcheck-err {errline} {errpos} "{msg}")'.format(**res)

# http://pymacs.progiciels-bpi.ca/pymacs.html
# ">2\t.\n"  length = data plus newline

cmdpat = re.compile('^>(\d+)\t(.+\n)')

# Pymacs style:
def pm_send(data):
    logging.debug('send: %s', data)
    print '<%d\t%s' % (len(data)+1, data) # +1 for newline
def send(data):
    logging.debug('send: %s', data)
    print data

def server(fd):
    send('(piemacs-status "fastcheck started")')
    logging.info('started')
    while 1:
        line = fd.readline()    # line buffered
        if not line:
            break
        logging.debug('< %s', line.rstrip())
        if not line.startswith('>'):
            logging.info('cmd?: %s',line.rstrip())
            continue
        m = cmdpat.match(line)
        if not m:
            logging.info('detail?: %s',line.rstrip())
            continue
        total = int(m.group(1))
        source = [m.group(2)]
        remaining = total - len(source[0])
        if remaining:
            source.append( fd.read(remaining) )

        source = ''.join(source)
        # print "yay: size=%d, source=%s" % (total, source)
        res = fastcheck(source, filename='<stdin>')
        if not res:
            send('(piemacs-status "fastcheck ok")')
            continue
        send('(fastcheck-err {errline} {errpos} "{msg}")'.format(**res))

if __name__=='__main__':
    if sys.argv[1:] == ['--server']:
        try:
            server(sys.stdin)
        except Exception as exc:
            logging.critical('stopping: %s', exc)
    else:
        check()
