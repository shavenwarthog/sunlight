#!/usr/bin/env python2.6

import glob, itertools, sys, token, tokenize


def show(sourcefunc):
    ERR = token.ERRORTOKEN
    try:
        for info in tokenize.generate_tokens(sourcefunc):
            print '\t',token.tok_name[info[0]], info[1:]
            if info[0]==ERR:
                _ttype,tstring,_st,_end,_tline = info
                print _st, tstring 
                return
    except tokenize.TokenError, exc:
        print exc[1], exc[0]    # "EOF in multi-line statement"

SRC='''
def zoot():
 x = 5)
y = 3
'''

def timetest():
    big = itertools.chain( ((path,open(path).readline) for path in glob.glob('ex-*.py')) )
    for path,func in big:
        print path
        show(func)

if __name__=='__main__':
    try:
        print compile('x=', '<string>', 'exec')
    except SyntaxError, exc:
        print repr(exc)
        
    sys.exit(1)
