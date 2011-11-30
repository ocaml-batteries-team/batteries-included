# A basic Makefile for building and installing Batteries Included
# It punts to ocamlbuild for all the heavy lifting.

NAME = batteries

VERSION := $(shell grep "^Version:" _oasis | cut -c 15-)
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
OCAMLBUILDFLAGS ?= -no-links

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
OPT_INSTALL_FILES = _build/src/*.cmx _build/src/*.a _build/src/*.cmxa \
	_build/src/*.cmxs

# What to build
TARGETS = syntax.otarget
TARGETS += src/batteries.cma
TARGETS += src/batteriesHelp.cmo
TARGETS += src/batteriesThread.cma
TARGETS += META
BENCH_TARGETS = benchsuite/bench_int.native benchsuite/bench_map.native
TEST_TARGET = test-byte

ifeq ($(BATTERIES_NATIVE_SHLIB), yes)
  EXT = native
  MODE = shared
  TARGETS += src/batteries.cmxs src/batteriesThread.cmxs
  TEST_TARGET = test-native
else 
ifeq ($(BATTERIES_NATIVE), yes)
  EXT = native
  MODE = native
  TARGETS += src/batteries.cmxa src/batteriesThread.cmxa
  TEST-DEPS = test-native
else
  EXT = byte
  MODE = bytecode
endif
endif

.PHONY: all clean doc install uninstall reinstall test qtest camfail camfailunk

all: 
	@echo "Build mode:" $(MODE)
	$(OCAMLBUILD) $(OCAMLBUILDFLAGS) $(TARGETS)

clean:
	${RM} apidocs src/batteriesConfig.ml META
	${RM} qtest/*_t.ml qtest/test_mods.mllib
	$(OCAMLBUILD) -clean

batteries.odocl: src/batteries.mllib src/batteriesThread.mllib
	cat $^ > $@

doc: batteries.odocl
	$(OCAMLBUILD) batteries.docdir/index.html

install: all uninstall_packages
	ocamlfind install $(OCAMLFIND_DEST) estring \
		libs/estring/META \
		_build/libs/estring/*.cmo \
		_build/libs/estring/*.cmi \
		_build/libs/estring/*.mli
	ocamlfind install $(OCAMLFIND_DEST) $(NAME) $(INSTALL_FILES) \
		-optional $(OPT_INSTALL_FILES)

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
TESTABLE ?= $(filter-out $(DONTTEST), $(wildcard src/*.ml))

TESTDEPS = $(patsubst src/%.ml,qtest/%_t.ml, $(TESTABLE)) qtest/test_mods.mllib

_build/testsuite/main.byte: $(TESTDEPS)
	$(OCAMLBUILD) $(OCAMLBUILDFLAGS) testsuite/main.byte
_build/testsuite/main.native: $(TESTDEPS)
	$(OCAMLBUILD) $(OCAMLBUILDFLAGS) testsuite/main.native

_build/qtest/test_runner.byte: $(TESTDEPS)
	$(OCAMLBUILD) $(OCAMLBUILDFLAGS) qtest/test_runner.byte
_build/qtest/test_runner.native: $(TESTDEPS)
	$(OCAMLBUILD) $(OCAMLBUILDFLAGS) qtest/test_runner.native

#qtest only targets, for quicker test iteration
qtest-byte: _build/qtest/test_runner.byte
	_build/qtest/test_runner.byte

qtest-native: _build/qtest/test_runner.native
	_build/qtest/test_runner.native

# all tests 
test-byte: _build/testsuite/main.byte _build/qtest/test_runner.byte
	_build/testsuite/main.byte 
	_build/qtest/test_runner.byte

test-native: _build/testsuite/main.native _build/qtest/test_runner.native
	_build/testsuite/main.byte 
	_build/qtest/test_runner.byte
	_build/testsuite/main.native
	_build/qtest/test_runner.native

test: $(TEST_TARGET)

bench: 
	$(OCAMLBUILD) $(OCAMLBUILDFLAGS) $(TARGETS) $(BENCH_TARGETS)
	$(foreach BENCH, $(BENCH_TARGETS), _build/$(BENCH); )

release: setup.ml doc test 
	git archive --format=tar --prefix=batteries-$(VERSION)/ HEAD \
	  | gzip > batteries-$(VERSION).tar.gz

setup.ml: _oasis
	oasis setup
	git commit setup.ml -m"Update setup.ml based on _oasis"

##
## Magic for test target - auto-generated test files from source comments
##

_build/build/make_suite.$(EXT): build/make_suite.mll
	$(OCAMLBUILD) $(OCAMLBUILDFLAGS) make_suite.$(EXT)

#convert a source file to a test suite by filtering special comments
qtest/%_t.ml: src/%.ml _build/build/make_suite.$(EXT)
	_build/build/make_suite.$(EXT) $< > $@

#put all the testing modules in a library
qtest/test_mods.mllib:
	/bin/echo -n "Quickcheck Tests " > $@
	echo $(patsubst src/%.ml,%_t, $(TESTABLE)) >> $@

.PHONY: qtest/test_mods.mllib
