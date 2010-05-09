#!/usr/bin/env python

'''
simplifyp -- simplify Nosetest output
'''

import fileinput, re, sys
from parse import CallCode

class ParseAssert(dict):
    PAT = ('AssertionError: '
           '#(?P<leftnum>\d+) fake:'
           '(?P<left_code>.+?) was unexpected; Expected: '
           '(?P<rights_str>.+), end'
           )
    NUMCALL = '#(?P<num>\d+) fake:(?P<code>.+)'

    def __init__(self, line):
        super(ParseAssert,self).__init__()
        self.pat = re.compile(self.PAT)
        self.parse(line)

    def parse(self, line):
        m = self.pat.match(line)
        if not m:
            return
        self.update(m.groupdict())

def simpargs(args):
    # XX: rstrip right paren:
    return ((item['arg'], item['v_code'].rstrip(')')) for item in args)

class UnexpectedWithArgs(ParseAssert):
    PAT = ('AssertionError: fake:'
           '(?P<left_code>.+?)'
           '[^)]* was called unexpectedly with args ' # zap optional "[0]" (XX?)
           '(?P<right_args>.+)'
           )

    def parse(self,line):
        super(UnexpectedWithArgs,self).parse(line)
        if not self:
            return
        self['left_cc'] = CallCode(self['left_code'])
        self['right_cc'] = CallCode('%s%s' % (
                self['left_cc'].name,
                self['right_args']))

class Diff(list):
    def __init__(self, cc1, cc2):
        super(Diff,self).__init__()
        self.cc1 = cc1
        self.cc2 = cc2
        self.diff()

    def diff(self):
        left = dict(simpargs(self.cc1))
        right = dict(simpargs(self.cc2))
        diffkv = set(left.iteritems()) ^ set(right.iteritems())
        for arg in sorted(set( (key for key,_ in diffkv) )):
            self.append( [
                    arg, left.get(arg,None), right.get(arg,None)
                    ] )
def termwidth():
    import os
    if 0:
        # http://stackoverflow.com/questions/566746/how-to-get-console-window-width-in-python
        rows, columns = os.popen('stty size', 'r').read().split()
        return columns
    else:
        return int(os.environ.get('COLUMNS') or '80') # XX

def handle_withargs(line):
    exp = UnexpectedWithArgs(line)
    diff = Diff(exp['left_cc'], exp['right_cc'])

    colwidth = int((termwidth() - 18) / 2)
    print '%s() called with different args:' % exp['left_cc'].name
    for arg,left,right in diff:
        FMT = '** %-10s %-'+str(colwidth)+'s %-'+str(colwidth)+'s'
        print FMT % (
            arg[:10],
            str(left or '<undef>')[:colwidth],
            str(right or '<undef>')[:colwidth],
            )
    diffargs = zip(*diff)[0] # pylint: disable-msg=W0142
    for arg,code in sorted( simpargs(exp['left_cc']) ):
        if arg in diffargs:
            continue            # XX
        print '%-13s %-64s' % (
            arg[:13],
            code[:64],
            )

def main(paths):
    line = None
    for line in fileinput.input(files=paths):
        if 'with args' in line:
            try:
                handle_withargs(line)
            except KeyError as exc:
                print "KeyError(%s): handle_withargs()" % exc
                print line,
        else:
            print line,

    # print Diff( exp['left_cc'], exp['rights'][0]['cc'] )
    # print exp['left_code']
    # print
    # calls = [CallCode(exp['left'])]
    # calls += imap(CallCode, (info['code'] for info in exp['rights']))
    # for call in calls:
    #     print call.name
    #     for arg in sorted(call):
    #         print arg['arg'], arg['v_code']
    #     print

if not sys.argv[1:]:
    print 'usage'
    sys.exit(2)
main(sys.argv[1:])



