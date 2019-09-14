PROJECT = $(notdir $(shell pwd))

CUR_DIR = $(shell pwd)

include erlang.mk

test : tests

