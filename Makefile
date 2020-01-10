.POSIX:
EMACS = emacs

.PHONY: all install compile init update clean clean-all

all: init

install:
	$(EMACS) --batch -l init.el

compile:
	rm -f *.elc lisp/*.elc
	$(EMACS) --batch -f package-initialize -f batch-byte-compile init.el lisp/*.el

init: install compile

update: clean install compile

clean:
	rm -f *.elc lisp/*.elc
	rm -rf elpa

clean-all: clean
	rm -rf var
