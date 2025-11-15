SHELL := bash
.ONESHELL:
.SHELLFLAGS := -euc
.DELETE_ON_ERROR:
MAKEFLAGS += --warn-undefined-variables
MAKEFLAGS += --no-builtin-rules

## General Rules

all: compile doc
.PHONY: all
.NOTPARALLEL: all

compile:
	@rebar3 compile
.PHONY: compile

clean:
	@rebar3 clean -a
.PHONY: clean

check: check-formatted xref find-unused-code lint dialyzer
.NOTPARALLEL: check
.PHONY: check

test: eunit ct cover
.NOTPARALLEL: test
.PHONY: test

format:
	@rebar3 fmt
.PHONY: format

## Tests

ct:
	@rebar3 ct
.PHONY: ct

eunit:
	@rebar3 eunit
.PHONY: eunit

cover:
	@rebar3 cover
.PHONY: cover

## Checks

check-formatted:
	@if rebar3 plugins list | grep '^erlfmt\>' >/dev/null; then \
		rebar3 fmt --check; \
	else \
		echo >&2 "WARN: skipping rebar3 erlfmt check"; \
	fi
.PHONY: check-formatted

dialyzer:
	@rebar3 as test dialyzer
.PHONY: dialyzer

xref:
	@rebar3 as test xref
.PHONY: xref

lint:
	@if rebar3 plugins list | grep '^rebar3_lint\>' >/dev/null; then \
		rebar3 lint; \
	else \
		echo >&2 "WARN: skipping rebar3_lint check"; \
	fi
.PHONY: lint

find-unused-code:
	@if rebar3 plugins list | grep '^rebar3_hank\>' >/dev/null; then \
		rebar3 hank; \
	else \
		echo >&2 "WARN: skipping rebar3_hank check"; \
	fi
.PHONY: lint

## Shell, docs and publication

shell: export ERL_FLAGS = +pc unicode
shell:
	@rebar3 as test shell
.PHONY: shell

doc:
	@rebar3 hex build
.PHONY: doc

publish:
	@rebar3 hex publish
.PHONY: publish
