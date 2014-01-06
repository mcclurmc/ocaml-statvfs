# Generic oasis driver.

DESTDIR?=$(shell ocamlc -where)
CONFIG?=

SETUP=setup.ml setup.bin setup.data

.PHONY: default all build clean distclean test install uninstall

default: build

all: build

setup.ml: _oasis
	oasis setup

setup.bin: setup.ml
	ocamlc -o $@ $<
	rm -f setup.o setup.cmo setup.cmx setup.cmi

setup.data: setup.bin
	./setup.bin -configure --enable-tests --destdir $(DESTDIR) $(CONFIG)

build: $(SETUP) $(LIB_OUT) $(MAIN_OUT) $(TEST_OUT)

$(LIB_OUT): $(SETUP) $(SOURCE_LIB)
	./setup.bin -build

$(MAIN_OUT): $(SETUP) $(SOURCE_MAIN) $(SOURCE_LIB)
	./setup.bin -build

$(TEST_OUT): $(SETUP) $(SOURCE_TEST) $(SOURCE_LIB)
	./setup.bin -build

clean: setup.bin
	./setup.bin -clean

distclean: setup.bin
	./setup.bin -distclean
	@rm -f lib/*.clib lib/*.mlpack lib/*.mllib
	@rm -f setup.* myocamlbuild.ml _tags lib/META

uninstall:
	ocamlfind remove $(LIBNAME)

install: $(SETUP) $(LIB_OUT)
	OCAMLFIND_DESTDIR=$(DESTDIR) \
	OCAMLFIND_LDCONF=$(DESTDIR)/ld.conf \
	./setup.bin -install

reinstall: uninstall install

test: $(SETUP) $(TEST_OUT)
	./setup.bin -test
