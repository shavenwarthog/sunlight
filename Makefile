RECENT_PY := $(shell ls -1t *.py | egrep -v _flymake | head -1)

all:

# .PHONY: src-paths.dat
# src-paths.dat:
# time find ../geodelic/packages -name '*.py' > $@
# head $@

# 	etags -o - *py

.PHONY: tags
tags TAGS:
	etags *.py

snoop:
	nosetests -d ./snoop.py
# 	nosetests --version
# 	nosetests -p

tweeze:
	./tweezer.py
#	python -m trace --report --coverdir=/tmp
xref:
	@echo "zoot calls ding"
	@echo
	./xref.py -v -f xref.dat ex-xref.py
	cat xref.dat
#	./xref.py -v -f - ex-xref.py

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
