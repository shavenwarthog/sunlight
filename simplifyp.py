#!/usr/bin/env python

import fileinput, operator, re
from itertools import imap, izip_longest
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
        # self['left_cc'] = CallCode(self['left_code'])
        # self['rights'] = [
        #     m.groupdict() for m in re.finditer(self.NUMCALL, self['rights_str'])
        #     ]
        # for item in self['rights']:
        #     item['cc'] = CallCode(item['code'])

def simpargs(args):
    # XX: rstrip right paren:
    return ((item['arg'], item['v_code'].rstrip(')')) for item in args)

class UnexpectedWithArgs(ParseAssert):
    PAT = ('AssertionError: fake:'
           '(?P<left_code>.+?)'
           '\[0\] was called unexpectedly with args '
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
        for arg,vcode in sorted( set(left.iteritems()) ^ set(right.iteritems()) ):
            self.append( [
                    arg, left.get(arg,None), right.get(arg,None)
                    ] )

# def threecol(left,mid,right, widths):
#     for a,b,c in izip_longest(left,mid,right):
#         yield '%\

def main():
    line = None
    for line in fileinput.input(files=['err3.txt']):
        if 'with args' in line:
            break

    exp = UnexpectedWithArgs(line)
    print line
    # print exp
    # print exp['left_cc']
    # print
    # print exp['right_cc']
    # print
    diff = Diff(exp['left_cc'], exp['right_cc'])
    print '%s() called with different args:' % exp['left_cc'].name
    for arg,left,right in diff:
        print '** %-10s %-32s %-32s' % (
            arg[:10],
            str(left or '<undef>')[:32],
            str(right or '<undef>')[:32],
            )
    diffargs = zip(*diff)[0] # pylint: disable-msg=W0142
    for arg,code in sorted( simpargs(exp['left_cc']) ):
        if arg in diffargs:
            continue            # XX
        print '%-13s %-64s' % (
            arg[:13],
            code[:64],
            )


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

main()
