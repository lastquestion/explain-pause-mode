# TODO add multiple emacs versions, don't depend on package-initialize, use -Q

EMACS=emacs

# the test case that need full emacs driver
case-driver=tests/cases/driver.el
cases:=$(filter-out $(case-driver), $(wildcard tests/cases/*.el))

# all the test cases don't generate output so they need to be PHONY
.PHONY: test case-tests $(cases)

case-tests: $(cases)

$(cases): %.el:
	emacs -batch -Q -f toggle-debug-on-error -l $(case-driver) -l $@ -f "run-test"

unit-tests:
	$(EMACS) -batch -Q -f package-initialize -l explain-pause-mode.el -f buttercup-run-discover tests
	$(EMACS) -batch -Q -l explain-pause-mode.el -l tests/manual-test-command-logging.el

tests: unit-tests case-tests
