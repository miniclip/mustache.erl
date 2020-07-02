SHELL := bash
.ONESHELL:
.SHELLFLAGS := -euc
.DELETE_ON_ERROR:
MAKEFLAGS += --warn-undefined-variables
MAKEFLAGS += --no-builtin-rules

all:
.PHONY: all

clean:
	rebar3 clean -a
.PHONY: clean

compile:
	rebar3 compile
.PHONY: compile

check: xref dialyzer
.NOTPARALLEL: check
.PHONY: check

xref:
	rebar3 xref
.PHONY: xref

dialyzer:
	rebar3 dialyzer
.PHONY: dialyzer

test: eunit cover
.NOTPARALLEL: check
.PHONY: test

eunit:
	rebar3 eunit
.PHONY: eunit

cover:
	rebar3 cover
.PHONY: cover

shell:
	rebar3 shell
.PHONY: shell
