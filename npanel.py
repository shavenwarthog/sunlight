#!/usr/bin/env python

# bin/nosetests --exclude=jm
# bin/nosetests --exclude=jm -v

import fileinput, logging, re
import pyinotify

logging.basicConfig(
    level=logging.DEBUG,
    format='%(asctime)s %(levelname)s %(message)s',
    # filename='/tmp/myapp.log',
    # filemode='w')
)

def modify_cb(notifier):
    print 'ding',notifier.__dict__

def main():
    wm = pyinotify.WatchManager()
    notifier = pyinotify.Notifier(wm)
    mask = pyinotify.EventsCodes.ALL_FLAGS.get('IN_CLOSE_WRITE')

    logging.info('start')

    wm.add_watch('.', mask)
    # Loop forever (until sigint signal get caught)
    notifier.loop(callback=modify_cb)
    logging.info('end')

if __name__=='__main__':
    main()
