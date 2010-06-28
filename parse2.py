#!/usr/bin/env python

import ast

ERR="""outerfunc(column_parent=ColumnParent(column_family='words', super_column=None), keyspace='demo-terms', consistency_level=1, predicate=SlicePredicate(column_names=None, slice_range=SliceRange(count=3, start='food', reversed=False, finish='')), key='f')"""

print ast.literal_eval('{"beer":["tasty",1,2,3.14]}')
# {'beer': ['tasty', 1, 2, 3.1400000000000001]}

mod = ast.parse(ERR)
expr = ast.iter_child_nodes(mod).next()
print list(ast.iter_child_nodes(expr))


ast.parse('x=5')
list(ast.walk(x))
