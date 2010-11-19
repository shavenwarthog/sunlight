#!/usr/bin/env python2.6

import os, sys, token, tokenize


def find_callsdefs(path):
    pin = []
    for info in tokenize.generate_tokens(open(path).readline):
        ttype,tstring,_st,_end,_line = info
        if (ttype==token.OP and tstring=='('
            and pin and pin[-1][0]==token.NAME):
            yield pin[-2:]
            pin = []
        else:
            pin.append(info)

def epath(path):
    return '{path},0'.format(**locals())

def etag(tagdef,name,lineno,offset):
    return '{tagdef}\x7f{name}\x01>{lineno},{offset}'.format(**locals())

def procpath(path):
    if os.path.splitext(path)[-1] != '.py':
        return

    defs = []
    calls = []
    for info in find_callsdefs(path):
        if info[-1][1] in ('def','class'):
            defs.append( info )
        else:
            calledby = defs[-1][-1] if defs else '' # global
            calls.append( list(info) + [calledby] )
            print calls[-1]; sys.exit(1)
    return defs,calls

    # if 0:
    #     for row,name in defs:
    #         print name,path,row
    #     for row,name,calledby in calls:
    #         print name,path,row,calledby
    # else:
def mketags(outf, path, infos):
    print >>outf, epath(path)
    for info in infos:
        _ttype,tstring,st,_end,line = info[:4]
        print >>outf, etag(tagdef=tstring)
# name,path,row
#         for row,name,calledby in calls:
#             print name,path,row,calledby


if __name__=='__main__':
    for path in sys.argv[1:]:
        defs,calledby = procpath(path)
        mketags( sys.stdout, path, calledby )




