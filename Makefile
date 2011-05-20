# A basic Makefile for building and installing Batteries Included
# It punts to ocamlbuild for all the heavy lifting.

NAME = batteries

VERSION := $(shell cat VERSION)
uname_S := $(shell sh -c 'uname -s 2>/dev/null || echo not')

# Define variables and export them for mkconf.ml
DOCROOT ?= /usr/share/doc/ocaml-batteries
export DOCROOT
BROWSER_COMMAND ?= x-www-browser
export BROWSER_COMMAND

ifdef DESTDIR
OCAMLFIND_DEST += -destdir $(DESTDIR)
endif

OCAMLBUILD ?= ocamlbuild

ifeq ($(uname_S),Darwin)
  BATTERIES_NATIVE ?= yes
  BATTERIES_NATIVE_SHLIB ?= no
else 
  BATTERIES_NATIVE ?= yes
  BATTERIES_NATIVE_SHLIB ?= $(BATTERIES_NATIVE)
endif

INSTALL_FILES = _build/META _build/src/*.cma \
	battop.ml _build/src/*.cmi _build/src/*.mli \
	_build/src/batteriesHelp.cmo \
	_build/src/syntax/pa_comprehension/pa_comprehension.cmo \
	_build/src/syntax/pa_strings/pa_strings.cma \
	_build/src/syntax/pa_llist/pa_llist.cmo
NATIVE_INSTALL_FILES = _build/src/*.cmx _build/src/*.a _build/src/*.cmxa

# What to build
TARGETS = syntax.otarget byte.otarget src/batteriesHelp.cmo META
BENCH_TARGETS = benchsuite/bench_int.native benchsuite/bench_map.native
TEST_TARGET = test-byte

ifeq ($(BATTERIES_NATIVE_SHLIB), yes)
  EXT = native
  MODE = shared
  TARGETS += shared.otarget
  TEST_TARGET = test-native
  INSTALL_FILES += $(NATIVE_INSTALL_FILES) _build/src/*.cmxs
else ifeq ($(BATTERIES_NATIVE), yes)
  EXT = native
  MODE = native
  TARGETS += native.otarget
  TEST-DEPS = test-native
  INSTALL_FILES += $(NATIVE_INSTALL_FILES)
else
  EXT = byte
  MODE = bytecode
endif

.PHONY: all clean doc install uninstall reinstall test qtest camfail camfailunk

all: 
	@echo "Build mode:" $(MODE)
	${RM} src/batteriesConfig.ml
	$(OCAMLBUILD) $(TARGETS)

clean:
	${RM} apidocs
	${RM} qtest/*_t.ml qtest/test_mods.mllib
	$(OCAMLBUILD) -clean

doc:
	$(OCAMLBUILD) batteries.docdir/index.html
	test -e apidocs || ln -s _build/batteries.docdir apidocs

install: all uninstall_packages
	ocamlfind install $(OCAMLFIND_DEST) estring \
		libs/estring/META \
		_build/libs/estring/*.cmo \
		_build/libs/estring/*.cmi \
		_build/libs/estring/*.mli
	ocamlfind install $(OCAMLFIND_DEST) $(NAME) $(INSTALL_FILES)

uninstall_packages:
	ocamlfind remove $(OCAMLFIND_DEST) estring
	ocamlfind remove $(OCAMLFIND_DEST) $(NAME)

uninstall: uninstall_packages
	${RM} -r $(DOCROOT)

install-doc: doc
	mkdir -p $(DOCROOT)
	cp -r doc/batteries/* $(DOCROOT)
# deal with symlink that will break
	${RM} $(DOCROOT)/html/batteries_large.png
	cp -f doc/batteries_large.png $(DOCROOT)/html
	mkdir -p $(DOCROOT)/html/api
	cp apidocs/* $(DOCROOT)/html/api
	cp LICENSE README FAQ VERSION $(DOCROOT)

reinstall:
	$(MAKE) uninstall
	$(MAKE) install

#List of source files that it's okay to try to test
DONTTEST=src/batteriesHelp.ml
TESTABLE=$(filter-out $(DONTTEST), $(wildcard src/*.ml))

TESTDEPS = $(patsubst src/%.ml,qtest/%_t.ml, $(TESTABLE)) qtest/test_mods.mllib

compile-test-byte: $(TESTDEPS)
	$(OCAMLBUILD) syntax.otarget byte.otarget src/batteriesHelp.cmo META testsuite/main.byte qtest/test_runner.byte

compile-test-native: $(TESTDEPS)
	$(OCAMLBUILD) syntax.otarget byte.otarget src/batteriesHelp.cmo META testsuite/main.byte qtest/test_runner.byte testsuite/main.native qtest/test_runner.native

run-test-byte:
	_build/testsuite/main.byte
	_build/qtest/test_runner.byte

run-test-native: run-test-byte
	_build/testsuite/main.native
	_build/qtest/test_runner.native

test-byte: compile-test-byte run-test-byte
test-native: compile-test-native run-test-byte
test: $(TEST_TARGET)

bench: 
	$(OCAMLBUILD) $(TARGETS) $(BENCH_TARGETS)
	$(foreach BENCH, $(BENCH_TARGETS), _build/$(BENCH); )

release: test
	git archive --format=tar --prefix=batteries-$(VERSION)/ HEAD \
	  | gzip > batteries-$(VERSION).tar.gz

##
## Magic for test target - auto-generated test files from source comments
##

_build/build/make_suite.$(EXT): build/make_suite.mll
	$(OCAMLBUILD) -no-links make_suite.$(EXT)

#convert a source file to a test suite by filtering special comments
qtest/%_t.ml: src/%.ml _build/build/make_suite.$(EXT)
	_build/build/make_suite.$(EXT) $< > $@

#put all the testing modules in a library
qtest/test_mods.mllib: $(TESTABLE)
	/bin/echo -n "Quickcheck Tests " > $@
	echo $(patsubst src/%.ml,%_t, $(TESTABLE)) >> $@

