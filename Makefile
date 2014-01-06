# Project specific variables

LIBNAME=statfs

SOURCE_LIB=$(shell ls lib/*.ml*)
SOURCE_MAIN=$(shell ls src/*.ml*)
SOURCE_TEST=$(shell ls test/*.ml*)

LIB_OUT=_build/lib/statfs.cmxs
TEST_OUT=_build/test/statfs_test.byte
#MAIN_OUT=$(shell ls _build/src/*.byte) $(shell ls _build/src/*.native)

include oasis.mk

# Test program in C

test/statfs: test/statfs.c
	cc -g $^ -o $@
