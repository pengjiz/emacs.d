.POSIX:
EMACS = emacs

.PHONY: all install compile init update clean clean-all

all: init

install:
	@echo "==> Installing packages..."
	$(EMACS) --batch -l init.el

compile:
	@echo "==> Removing outdated compiled files..."
	rm -f *.elc lisp/*.elc
	@echo "==> Compiling user lisp files..."
	$(EMACS) --batch -f package-initialize -f batch-byte-compile init.el lisp/*.el

init: install compile

update: clean install compile

clean:
	@echo "==> Removing compiled files..."
	rm -f *.elc lisp/*.elc
	@echo "==> Removing installed packages..."
	rm -rf elpa

clean-all: clean
	@echo "==> Removing user data files..."
	rm -rf var
