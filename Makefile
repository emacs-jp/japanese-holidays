.PHONY : test

EMACS ?= emacs

test:
	$(EMACS) -Q -batch -L . \
		-l japanese-holidays-tests.el \
		-f ert-run-tests-batch-and-exit

