RECENT_PY := $(shell ls -1t *.py | egrep -v _flymake | head -1)

all:

xref:
	./xref.py -f - ex-xref.py

.PHONY: elisp-doc.html
elisp-doc.html:
	./elisp.py > $@
	head -100 $@ 

# --regex-swine=/^def[ \t]*([a-zA-Z0-9_]+)/\1/d,definition/
# xref:
# 	ctags --version | head -1
# 	ctags −−langdef=pycall zoot.py
#  --langmap=pycall:.py	\
# 	'--regex-pycall=/([a-zA-Z0-9_]+)\(/\1/c,call/'	\
# 	zoot.py

test-simplifyp:
	./simplifyp.py err4.txt

.PHONY: tags
tags:
	etags *.py

test-chump:
	./chump.py /tmp/project.tags



clean:
	-$(RM) fn_*[0-9][0-9]*.py

test-npanel.py:
	./npanel.py /tmp/z

test:
	cat err3.txt | ./$(RECENT_PY) -


test-update:
	./update-tags ~/src/*/setup.py $(PWD)

