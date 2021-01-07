
EMACS=emacs --batch

.PHONY: tests
check:
	$(EMACS) -l gnu-elpa-autoloads.el -l gnu-elpa--tests.el \
	         -f ert-run-tests-batch-and-exit

.PHONY: refresh
refresh:
	$(EMACS) -l gnu-elpa-maint.el -f gnu-elpa--make-features
