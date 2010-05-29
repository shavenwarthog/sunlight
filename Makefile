RECENT_PY := $(shell ls -1t *.py | egrep -v _flymake | head -1)

all:	test-chump

test-simplifyp:
	./simplifyp.py err4.txt

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

