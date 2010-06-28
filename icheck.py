#!/usr/bin/python

'''
icheck.py -- run checker when Python source file changes
'''

import logging, os, subprocess, sys, time
import pyinotify

logging.basicConfig(
    level=logging.DEBUG,
    format='%(asctime)s %(levelname)s %(message)s',
)

def modify_cb(event):
    print 'ding',event .__dict__

def run_test(path):
    logging.debug('modified %s', path)
    if '/geopoi/' not in path:
        logging.debug('(not geopoi, ignoring)')
        return
    cmd = 'bin/run_tests geopoi'
    logging.debug('=> %s', cmd)
    start_tm = time.time()
    proc = subprocess.Popen(
        'cd %s; %s' % ('~/src/geodelic', cmd), 
        shell=True, stderr=subprocess.STDOUT,
        )
    if proc.wait():
        logging.error('=> %s', proc.returncode)
    logging.debug('completed in %.2f seconds', time.time()-start_tm)
    # logging.debug( proc.stdout.read() )


class RunTest(object):

    def check_path(self, path):
        # skip temp files from Emacs, Flynote, and Vim
        if '#' in path or '/fn_' in path or path.endswith('~'):
            return False
        if os.path.splitext(path) in ('.pyc',):
            return False
        return True
        
    def __call__(self, event):
        path = os.path.abspath(os.path.join(event.path, event.name))
        if self.check_path(path):
            run_test(path)

def main():
    wm = pyinotify.WatchManager()
    notifier = pyinotify.Notifier(wm)

    wm.add_watch(
        path='.', 
        mask=pyinotify.EventsCodes.IN_CLOSE_WRITE,
        rec=True, 
        auto_add=True,
        proc_fun=RunTest(),
        )

    logging.info('%s: starting', os.path.basename(sys.argv[0]))
    while True:
        try:
            notifier.process_events()
            if notifier.check_events():
                notifier.read_events()
        except KeyboardInterrupt():
            notifier.stop()
            break
    logging.info('%s: done', os.path.basename(sys.argv[0]))

if __name__=='__main__':
    main()
