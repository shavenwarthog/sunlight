#!/usr/bin/env python

import operator, re, sys

# Function: <b>consp</b><var> object<a name="index-consp-293"></a></var><br>
# X: also Command, User Option, Variable
defpat = re.compile(
    '(?:Function|Macro).+'      # symbol type
    '<b>(.+?)</b>(.+?)'         # name and definition
    '<a name="(.+?)"',          # reference
    )
if 0:
    nodepat = re.compile(
        'class="node".+?\n<a name="(.+?)"></a>',
        )
    sections = [ m.group(1) for m in nodepat.finditer(doc) ]

doc = open('elisp.html', 'r').read()

defs = sorted([ m.groups() for m in defpat.finditer(doc) ])
letter_def = {}
for name,_,ref in defs:
    first = name[0]
    if first>='a' and not letter_def.has_key(first):
        letter_def[first] = (name, ref)

print '''<html> 
<head> 
<link rel="STYLESHEET" href="elisp.css" type='text/css' />
<body>
'''
for letter,(_,ref) in sorted(letter_def.iteritems()):
    print '<a href="#%(ref)s">%(letter)s</a>' % locals()

print '<HR/>'

firstnames = set( (ref for _,ref in letter_def.values()) )

for name,signature,ref in defs:
    signature = re.sub('<.+?>', ' ', signature)
    styles = 'def'
    if name in firstnames:      # XXX doesnt work
        styles = 'def,first'
    print ('<span class="%(styles)s">'
           '<a href="elisp.html#%(ref)s" name="%(ref)s">'
           '%(name)s</a> %(signature)s'
           '</span>'
           '<br/>'
            % locals()
           )

        

