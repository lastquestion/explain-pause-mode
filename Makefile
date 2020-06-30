EMACS=emacs

# the test case that need full emacs driver
case-driver=tests/cases/driver.el
cases:=$(filter-out $(case-driver), $(wildcard tests/cases/*.el))

# root project directory. use absolute paths so we can use any emacs
# note this has a trailing /
ROOT_DIR:=$(dir $(realpath $(lastword $(MAKEFILE_LIST))))

# all the test cases don't generate output so they need to be PHONY
.PHONY: test case-tests $(cases)

print-emacs-version:
	@echo $(ROOT_DIR)
	@echo "Emacs version under test: " `$(EMACS) --version | head -n 1`

case-tests: $(cases)

$(cases): %.el: print-emacs-version
	$(EMACS) -batch -Q -f toggle-debug-on-error -l $(case-driver) -l $@ -f "run-test"

unit-tests: print-emacs-version
	EMACSLOADPATH=$(BUTTERCUP_DIR): $(EMACS) -batch -Q -l explain-pause-mode.el -l buttercup.el -f buttercup-run-discover $(ROOT_DIR)tests/unit
	$(EMACS) -batch -Q -l explain-pause-mode.el -l tests/unit/manual-test-command-logging.el

tests: unit-tests case-tests
