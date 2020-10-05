.POSIX:
EMACS = emacs
RUNNER = $(EMACS) --batch
COMPILER = $(RUNNER) -f package-initialize -f batch-byte-compile

.PHONY: all install compile init update clean distclean

all: init

install:
	$(RUNNER) -l init.el

compile:
	rm -f *.elc lisp/*.elc
	$(COMPILER) init.el lisp/*.el

init: install compile

update: clean install compile

clean:
	rm -f *.elc lisp/*.elc
	rm -rf elpa

distclean: clean
	rm -rf var
