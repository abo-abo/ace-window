emacs ?= emacs
CASK = ~/.cask/bin/cask

.PHONY: all clean

all: compile

cask:
	$(shell EMACS=$(emacs) $(CASK) --verbose --debug)

compile:
	$(CASK) exec $(emacs) -batch --eval "(byte-compile-file \"ace-window.el\")"

clean:
	rm -f *.elc
