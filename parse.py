#!/usr/bin/env python

import ast
class MyVisitor(ast.NodeVisitor):
    def 
    def generic_visit(self, node):
        print type(node).__name__, node.__dict__
        super(MyVisitor,self).generic_visit(node)

code = 'dict(a=5, b=unused(3))'
a = ast.parse(code)
print code
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
