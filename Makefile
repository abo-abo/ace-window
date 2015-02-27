EMACS = emacs
# EMACS = emacs-24.3

LOAD = -l avy.el -l avy-test.el

.PHONY: all test clean

all: test

test:
	$(EMACS) -batch $(LOAD) -f ert-run-tests-batch-and-exit

clean:
	rm -f *.elc
