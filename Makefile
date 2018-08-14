emacs ?= emacs

update:
	$(emacs) -batch -l test/make-update.el

compile: clean
	$(emacs) -batch -l test/elpa.el -l test/make-compile.el

plain:
	$(emacs) -Q -l test/elpa.el -l test/make-plain

clean:
	rm -f *.elc

.PHONY: update compile clean
