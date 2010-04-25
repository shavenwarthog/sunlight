#!/usr/bin/env python

ERR='''get_slice(column_parent=ColumnParent(column_family='words', super_column=None), keyspace='demo-terms', consistency_level=1, predicate=SlicePredicate(column_names=None, slice_range=SliceRange(count=3, start='food', reversed=False, finish='')), key='f')'''
ERR2='''get_slice(arg=call(c1='c1', c2=None), key='ding')'''

import ast, re
from itertools import ifilter

code=ERR

class CallCode(list):
    def __init__(self, code):
        super(CallCode,self).__init__()
        self.name = None
        self.parse(code)
        self.valuepos = list(self.findvalues(self.mod))

    def findvalues(self, topnode):
        def expr_order(topnode):
            for node in ifilter(
                lambda node: hasattr(node, 'lineno'), 
                ast.walk(topnode)):
                yield (node.lineno, node.col_offset)
        return sorted(expr_order(topnode))

    def parse(self, code):
        self.code = code
        self.mod = ast.parse(code)
        expr = ast.iter_child_nodes(self.mod).next()
        call = ast.iter_child_nodes(expr).next()
        for arg in ast.iter_child_nodes(call):
            if type(arg) is ast.Name:
                self.name = arg.id
                continue
            self.append( dict(
                    arg=arg.arg,
                    v_lineno=arg.value.lineno,
                    v_offset=arg.value.col_offset,
                    ) )
        maxpos = -1
        for arg in reversed(self):
            arg['v_code'] = self.code[arg['v_offset']:maxpos]
            maxpos = code.rfind(',',None,arg['v_offset'])
            

if 01:
    print '*'*40
    print code
    print
    cc = CallCode(code)
    for c in cc:
        print c['arg'],'\t',c['v_code']


