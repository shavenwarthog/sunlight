import os

class Beer(object):
    def __init__(self):
        self.arg = 1

    def format(self):
        return 'arg: %(arg)s' % self.__dict__

