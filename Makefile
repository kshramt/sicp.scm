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
check:
	gosh scheme_test.scm
