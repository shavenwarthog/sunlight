#!/usr/bin/env python2.6

import glob, itertools, logging, optparse, re, sys, time, token, tokenize
from nose.tools import eq_ as eq
from StringIO import StringIO

logging.basicConfig(
    filename='/tmp/fastcheck.log', 
    level=logging.DEBUG,
    format="%(asctime)-15s %(levelname)s %(message)s",
    )

VERBOSE = False

def fastcheck(source, filename='<string>'):
    try:
        logging.info("< %d bytes: %s", len(source), source[:50])
        compile(source, filename, 'exec')
        logging.info("> ok")
        return
    except SyntaxError, exc:
        msg,(_,errline,errpos,src) = exc
        # XXXX:
        errinfo = dict( [(key,locals()[key]) for key in 'msg errline errpos src'.split()] )
        logging.info("> err: %s", errinfo)
        return errinfo

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
del test_time

def send(data):
    # logging.debug('send: %s', data)
    print data

def pymacs_iter(fd):
    cmdpat = re.compile('^>(\d+)\t(.*\n)')

    while 1:
        line = fd.readline()    # line buffered
        if not line:
            break 
        logging.debug('recv: "%s"', line.rstrip())
        m = cmdpat.match(line)
        if not m:
            logging.warn('message?: %s',line.rstrip())
            continue
        total = int(m.group(1))
        source = [m.group(2)]
        remaining = total - len(source[0])
        if remaining:
            source.append( fd.read(remaining) )
        yield ''.join(source)

        # start_tm = time.time()

def source_fd(text):
    return StringIO('>{0}\t{1}\n'.format(len(text)+1, text))

# def test_pymacs_iter():
#     eq( list(pymacs_iter(source_fd('x=5\n'))), 'blam')

def server(fd):
    # http://pymacs.progiciels-bpi.ca/pymacs.html
    # ">2\t.\n"  length = data plus newline
    OVERLAY_FMT = ('(piemacs-ov-pos :lineno {errline} :col {errpos}'
                   ' :message "{msg}" :face \'piemacs-fastcheck)')

    yield '(piemacs-status "fastcheck started")'
    logging.info('start')
    # start_tm = None
    for source in pymacs_iter(fd):
        res = fastcheck(source, filename='<stdin>')
        if 0:
            logging.info('%d chars in %.2f seconds', total, time.time()-start_tm)
        if not res:
            if VERBOSE:
                yield '(piemacs-status "fastcheck ok")'
            continue
        # be careful highlighting after the code
        if len(res['src']) == res['errpos']:
            res['errpos'] -= 1
        yield OVERLAY_FMT.format(**res)

def test_server():
    eq( list( server(source_fd('x=')) )[-1],
        '(piemacs-ov-pos :lineno 1 :col 2 :message "invalid syntax" :face \'piemacs-fastcheck)'
        )
def test_server2():
    source = 'x=5'
    eq( list( server(source_fd(source)) ),
        ['(piemacs-status "fastcheck started")'],
        )

# ::::::::::::::::::::::::::::::::::::::::::::::::::


if __name__=='__main__':
    if sys.argv[1:] == ['--server']:
        try:
            map(send, server(sys.stdin))
        except Exception as exc:
            logging.critical('stopping', exc_info=True)
        logging.info('stop')
    else:
        check()
