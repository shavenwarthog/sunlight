#!/usr/bin/env python2.6

import sys, token, tokenize

def mytokens(path):
    for ttype,tstring,(srow,_),_,tline in tokenize.generate_tokens(open(path).readline):
        yield (ttype, tstring, srow)

def find_calls(tokiter):
    pin = []
    for (ttype,tstring,srow) in tokiter:
        pin.append( (ttype,tstring,srow) )
        if (ttype==token.OP and tstring=='('):
            yield pin[-3], pin[-2]

def procpath(path):
    defs = []
    calls = []
    for (_,pstr,row),(_,name,_) in find_calls(mytokens('example.py')):
        if pstr=='def':
            defs.append( (row,name) )
        else:
            calls.append( (row,name,defs[-1][-1]) )
    for row,name in defs:
        print name,path,row
    for row,name,calledby in calls:
        print name,path,row,calledby


if __name__=='__main__':
    for path in sys.argv[1:]:
        procpath(path)



