# override this for different emacs
EMACS=emacs

# this should point to the emacs that has packages installed
EMACS_PACKAGES=emacs

# turn off garbage collection while running tests
EMACS_OPTS=--eval="(setq gc-cons-threshold most-positive-fixnum)"

# the test case that need full emacs driver
case-driver=tests/cases/driver.el
cases:=$(filter-out $(case-driver), $(wildcard tests/cases/*.el))

# the unit tests
unit-tests:=$(filter-out tests/unit/manual-test-command-logging.el, $(wildcard tests/unit/*.el))

# root project directory. use absolute paths so we can use any emacs
# note this has a trailing /
ROOT_DIR:=$(dir $(realpath $(lastword $(MAKEFILE_LIST))))

# whitespace matters here!
find_buttercup_from_packages=$(shell $(EMACS_PACKAGES) $(EMACS_OPTS) --batch -f package-initialize --eval='(princ (file-name-directory (locate-library "buttercup")))')
# find it, and save it, so we cache it for next time
find_set_buttercup_from_packages=$(eval BUTTERCUP_DIR:=$(find_buttercup_from_packages))$(BUTTERCUP_DIR)
get_buttercup=$(if $(BUTTERCUP_DIR),$(BUTTERCUP_DIR),$(info Buttercup not specified; finding buttercup from emacs packages...)$(find_set_buttercup_from_packages))

TARGET:=explain-pause-mode.el

# all the test cases don't generate output so they need to be PHONY
.PHONY: test case-tests $(cases) $(unit-tests) print-emacs-version

all: byte-compile tests

byte-compile:
	$(EMACS) $(EMACS_OPTS) -batch --eval '(setq byte-compile-error-on-warn t)' -f batch-byte-compile explain-pause-mode.el
	$(eval TARGET:=explain-pause-mode.elc)

print-emacs-version:
	@echo "Emacs version under test: " `$(EMACS) --version | head -n 1`

case-tests: $(cases)

$(cases): %.el: print-emacs-version
	TARGET=$(TARGET) $(EMACS) $(EMACS_OPTS) -batch -Q -f toggle-debug-on-error -l $(case-driver) -l $@ -f run-test

$(unit-tests): %.el: print-emacs-version
	EMACSLOADPATH=$(call get_buttercup): $(EMACS) $(EMACS_OPTS) -batch -Q -l $(TARGET) -l buttercup.el -l $@ -f buttercup-run

tests/unit/manual-test-command-logging.el: print-emacs-version
	$(EMACS) $(EMACS_OPTS) -batch -Q -l $(TARGET) -l tests/unit/manual-test-command-logging.el

unit-tests: tests/unit/manual-test-command-logging.el print-emacs-version
	EMACSLOADPATH=$(call get_buttercup): $(EMACS) $(EMACS_OPTS) -batch -Q -l $(TARGET) -l buttercup.el -f buttercup-run-discover $(ROOT_DIR)tests/unit

tests: unit-tests case-tests

up-version:
	@read -p "New version: " VERSION; \
	find . -name "*.el" -exec sed -i '' "s/^;; Version: .*$$/;; Version: $$VERSION/g" {} + \
	sed -i '' "s/^(defconst explain-pause-version .*$$/(defconst explain-pause-version $$VERSION/g" explain-pause-mode.el
