#!/usr/bin/env python

ERR='''unnamed.get_slice(column_parent=ColumnParent(column_family='words', super_column=None), keyspace='demo-terms', consistency_level=1, predicate=SlicePredicate(column_names=None, slice_range=SliceRange(count=3, start='food', reversed=False, finish='')), key='f')'''

import ast


def printme(indent, node):
    for kw in sorted(node.keywords, cmp=lambda a,b: cmp(a.arg,b.arg)):
        print ' '*indent*2, kw.arg
        if hasattr(kw, 'value'):
            printme(indent+1, [kw.value])
        else:
            try:
                print ' '*(indent+1)*2, kw.s
            except:
                print '? %s' % kw

        printme(1, [node])

class MyVisitor(ast.NodeVisitor):
    # def visit_Call(self, node, indent=None):
    #     indent = indent or 0
    #     print ' '*(indent*2), node, getattr(node,'keywords','n/a')
    #     for node in node.keywords:
    #         if 'ast.Call' in str(node):
    #             self.visit_Call(node, indent+1)
    #     # printme(1, node)

    def generic_visit(self, node):
        # print type(node).__name__, node.__dict__
        print ast.dump(node, annotate_fields=True, include_attributes=False)
        super(MyVisitor,self).generic_visit(node)

if 0:
    code = 'dict(a=5, b=unused(3))'
else:
    code = ERR
a = ast.parse(code)
print code
print
MyVisitor().generic_visit(a)

'''>>> dict(a=5, b=unused(3))

Module {'body': [<_ast.Expr object at 0x1b35410>]}
Expr {'lineno': 1, 'value': <_ast.Call object at 0x1b35a90>, 'col_offset': 0}
Call {'col_offset': 0, 'starargs': None, 'args': [], 'lineno': 1, 'func': <_ast.Name object at 0x1b35ad0>, 'kwargs': None, 'keywords': [<_ast.keyword object at 0x1b35490>, <_ast.keyword object at 0x1b35b90>]}
Name {'ctx': <_ast.Load object at 0x7f980b02d5d0>, 'id': 'dict', 'col_offset': 0, 'lineno': 1}
Load {}
keyword {'value': <_ast.Num object at 0x1b35590>, 'arg': 'a'}
Num {'lineno': 1, 'col_offset': 7, 'n': 5}
keyword {'value': <_ast.Call object at 0x1b35450>, 'arg': 'b'}
Call {'col_offset': 12, 'starargs': None, 'args': [<_ast.Num object at 0x1b35c90>], 'lineno': 1, 'func': <_ast.Name object at 0x1b35c50>, 'kwargs': None, 'keywords': []}
Name {'ctx': <_ast.Load object at 0x7f980b02d5d0>, 'id': 'unused', 'col_offset': 12, 'lineno': 1}
Load {}
Num {'lineno': 1, 'col_offset': 19, 'n': 3}
'''
