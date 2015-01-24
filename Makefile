EMACS ?= emacs
EMACS_CMD := $(EMACS) -Q -batch -L .
EL  := $(shell find . -type f -name '*.el' -print)
ELC := $(EL:.el=.elc)

test: clean
	$(EMACS_CMD) -l edep-test.el -f ert-run-tests-batch-and-exit

dist: parser-create autoload

clean:
	rm -f $(ELC)

compile: $(ELC)

%.elc: %.el
	$(EMACS_CMD) -f batch-byte-compile $<

autoload:
	$(EMACS_CMD) -l edep-test.el -f edep-generate-autoloads

parser-test:
	$(EMACS_CMD) -l edep-test.el -f semantic-php-batch-scan-project

parser-create:
	$(EMACS_CMD) -l edep-test.el -f edep-compile-grammar

.PHONY: clean compile test parse dist autoload
