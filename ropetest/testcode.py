import os

class Beer(object):
    def __init__(self):
        self.arg = 1

    def incr(self):
        self.arg += 1

    def format(self, count=2):
        ding(msg='arg%d' % self.arg, count=count)
        return 'arg: %(arg)s' % self.__dict__

def ding(msg, count=1):
    print 'ding! - %s' % (msg * count)

ding('x')

