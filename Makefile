.PHONY : build
build :
	@dune build --no-print-directory

TEST ?= test/expect/http
ROOT := $(shell [ -f ../dune-workspace ] && echo .. || echo .)

.PHONY : test
test :
	@find $(ROOT) -name '*.coverage' | xargs rm -f
	@dune build --no-print-directory \
	  --instrument-with bisect_ppx --force @$(TEST)/runtest
	@bisect-ppx-report html
	@bisect-ppx-report summary
	@echo See _coverage/index.html

.PHONY : test-watch
test-watch :
	@dune build --no-print-directory -w @$(TEST)/runtest

.PHONY : promote
promote :
	dune promote
	@make --no-print-directory test
