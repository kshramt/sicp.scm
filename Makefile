# Configurations
.SUFFIXES:
.DELETE_ON_ERROR:
.SECONDARY:
.ONESHELL:
export SHELL := /bin/bash
export SHELLOPTS := pipefail:errexit:nounset:noclobber


# Tasks
.PHONY: all
.DEFAULT_TARGET: all
all:


.PHONY: check
check: my-scheme-lib-test.scm.done


%-test.scm.done: %-test.scm %.scm
	GUILE_LOAD_PATH="$(CURDIR)":"$${GUILE_LOAD_PATH:-}" GUILE_AUTO_COMPILE=0 guile $<
	touch $@
