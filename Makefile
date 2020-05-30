# TODO add multiple emacs versions, don't depend on package-initialize, use -Q

EMACS=emacs

.PHONY: test

test:
	$(EMACS) -batch -f package-initialize -l explain-pause-mode.el -f buttercup-run-discover tests
	$(EMACS) -batch -l explain-pause-mode.el -l tests/manual-test-command-logging.el
