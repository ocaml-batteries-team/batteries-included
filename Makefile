# A basic Makefile for building and installing Batteries Included
# It punts to ocamlbuild for all the heavy lifting.

NAME = batteries

VERSION := $(shell cat VERSION)
OCAML_MAJOR_VERSION := $(firstword $(subst ., , $(shell ocamlfind ocamlc -version)))
uname_S := $(shell sh -c 'uname -s 2>/dev/null || echo not')

# Define variables and export them for mkconf.ml
DOCROOT ?= /usr/share/doc/ocaml-batteries
export DOCROOT
BROWSER_COMMAND ?= x-www-browser
export BROWSER_COMMAND

OCAMLBUILD ?= ocamlbuild
OCAMLBUILDFLAGS ?= -no-links -use-ocamlfind

ifeq ($(uname_S),Darwin)
  BATTERIES_NATIVE ?= yes
  BATTERIES_NATIVE_SHLIB ?= no
else
  BATTERIES_NATIVE ?= yes
  BATTERIES_NATIVE_SHLIB ?= $(BATTERIES_NATIVE)
endif

# Directory where to build the qtest suite
QTESTDIR ?= qtest

ifeq ($(shell ocamlfind query -p-format qcheck),qcheck)
	QTESTPKG = qcheck
else
	QTESTPKG = QTest2Lib
endif

INSTALL_FILES = _build/META _build/src/*.cma _build/src/*.cmi _build/src/*.mli \
	toplevel/battop.ml _build/toplevel/*.cmi _build/toplevel/*.mli \
# Note: we do not currently install
#       _build/toplevel/*.cma
# as there are no such files. If you create a proper library for batteries-help, you need to add *.cma
# to INSTALL_FILES.

INSTALL_FILES += \
	_build/src/batteriesConfig.cmo _build/src/batteriesPrint.cmo _build/toplevel/batteriesHelp.cmo \
	toplevel/ocamlinit build/ocaml

# the bin_annot flag in _tags is not handled by versions of ocamlbuild < 4.01.0
# hence we only install *.cmt{i} files if they were produced
ifneq ($(wildcard _build/src/*.cmt),)
	INSTALL_FILES += _build/src/*.cmt
	INSTALL_FILES += _build/toplevel/*.cmt
endif

ifneq ($(wildcard _build/src/*.cmti),)
	INSTALL_FILES += _build/src/*.cmti
	INSTALL_FILES += _build/toplevel/*.cmti
endif

OPT_INSTALL_FILES = \
	_build/src/*.cmx _build/src/*.cmxa _build/src/*.cmxs \
	_build/src/*.a _build/src/*.lib \
	_build/toplevel/*.cmx _build/toplevel/*.cmxa _build/toplevel/*.cmxs \
	_build/toplevel/*.a _build/toplevel/*.lib

ifneq ($(QTEST_SEED),)
	QTEST_SEED_FLAG = --seed $(QTEST_SEED)
else
	QTEST_SEED_FLAG =
endif

# What to build
TARGETS  = src/batteries.cma
TARGETS += toplevel/batteriesHelp.cmo
TARGETS += src/batteriesThread.cma
TARGETS += META
BENCH_TARGETS  = benchsuite/bench_int.native
BENCH_TARGETS += benchsuite/flip.native
BENCH_TARGETS += benchsuite/deque.native
BENCH_TARGETS += benchsuite/lines_of.native
BENCH_TARGETS += benchsuite/bitset.native
BENCH_TARGETS += benchsuite/bench_map.native
BENCH_TARGETS += benchsuite/bench_nreplace.native
BENCH_TARGETS += benchsuite/bench_set_to_seq.native
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
  TEST_TARGET = test-native
else
  EXT = byte
  MODE = bytecode
endif
endif

.PHONY: all clean deps doc install uninstall reinstall test qtest qtest-clean camfail camfailunk man test_install

all:
	@echo "Build mode:" $(MODE)
	$(OCAMLBUILD) $(OCAMLBUILDFLAGS) $(TARGETS)

clean:
	@${RM} src/batteriesConfig.ml src/batUnix.mli batteries.odocl \
	  bench.log $(QTESTDIR)/all_tests.ml src/batteries_compattest.ml
	@${RM} -r man/
	@$(OCAMLBUILD) $(OCAMLBUILDFLAGS) -clean
	@echo " Cleaned up working copy" # Note: ocamlbuild eats the first char!

batteries.odocl: src/batteries.mllib src/batteriesThread.mllib
	cat $^ > $@

doc: batteries.odocl
	$(OCAMLBUILD) $(OCAMLBUILDFLAGS) batteries.docdir/index.html

PREFILTER_BIN = _build/build/prefilter.byte
# compute human-readable dependencies between modules
deps: $(PREFILTER_BIN)
	ocamldep -modules -all -one-line -ml-synonym .mlv -mli-synonym .mliv \
	  -pp $(PREFILTER_BIN) src/*.ml src/*.mlv src/*.mliv src/*.mli > dependencies.txt

man: all batteries.odocl
	-mkdir man
	ocamlfind ocamldoc -package threads.posix -sort -man -hide-warnings -d man -I _build/libs -I _build/src libs/uniclib.mli src/*.mli

install: all uninstall_packages
	ocamlfind install $(NAME) $(INSTALL_FILES) \
		-optional $(OPT_INSTALL_FILES)

test_install:
	./scripts/test_install.sh

uninstall_packages:
	ocamlfind remove $(NAME)

uninstall: uninstall_packages
	${RM} -r $(DOCROOT)

install-doc: doc
	mkdir -p $(DOCROOT)
	mkdir -p $(DOCROOT)/html/api
	cp _build/batteries.docdir/* $(DOCROOT)/html/api
	cp LICENSE README.md FAQ $(DOCROOT)

install-man: man
	install man/* /usr/local/man/man3/

reinstall:
	$(MAKE) uninstall
	$(MAKE) install

###############################################################################
#      Pre-Processing of Source Code
###############################################################################

# the prefilter logic has moved to myocamlbuild.ml
# we keep the two void rules below for backward-compatibility for now
# (devs may have scripts calling them)
#
# For the record, and the ease of porting the build system to
# something else, the "prefilter" step preprocessed each file whose
# extension ends with a 'v', for example '.mliv', using the command
#     build/prefilter.byte foo.mliv > foo.mli

prefilter:
clean-prefilter:

###############################################################################
#	BUILDING AND RUNNING UNIT TESTS
###############################################################################

### List of source files that it's okay to try to test

# TESTABLE contains the source files as the user sees them,
# as a mix of .ml and .mlv files in the src/ directory

# TESTDEPS represents the file whose changes Makefile should watch to
# decide to reprocess the test results. It is identical to TESTABLE.

# TESTFILES contains the OCaml source files as `qtest` wants to see
# them, that is after preprocessing. We ask ocamlbuild to build the
# $(TESTFILES) from $(TESTABLE), and pass them to qtest from the
# `_build` directory.

DONTTEST=src/batteries_compattest.mlv \
	 src/batConcreteQueue_402.ml src/batConcreteQueue_403.ml
TESTABLE ?= $(filter-out $(DONTTEST),\
   $(wildcard src/*.ml) $(wildcard src/*.mlv))
TESTDEPS = $(TESTABLE)

TESTFILES = $(TESTABLE:.mlv=.ml)

### Test suite: "offline" unit tests
##############################################

_build/testsuite/main.byte: $(TESTDEPS) $(wildcard testsuite/*.ml)
	$(OCAMLBUILD) $(OCAMLBUILDFLAGS) testsuite/main.byte
_build/testsuite/main.native: $(TESTDEPS) $(wildcard testsuite/*.ml)
	$(OCAMLBUILD) $(OCAMLBUILDFLAGS) testsuite/main.native

### qtest: "inline" unit tests
##############################################


# extract all qtest unit tests into a single ml file
$(QTESTDIR)/all_tests.ml: $(TESTABLE)
	$(OCAMLBUILD) $(OCAMLBUILDFLAGS) $(TESTFILES)
	(cd _build; qtest -o ../$@ --shuffle \
	  --preamble-file ../qtest/qtest_preamble.ml \
	  extract $(TESTFILES))

_build/$(QTESTDIR)/all_tests.byte: $(QTESTDIR)/all_tests.ml
	$(OCAMLBUILD) $(OCAMLBUILDFLAGS) -cflags -warn-error,+26\
		-pkg oUnit,$(QTESTPKG) $(QTESTDIR)/all_tests.byte
_build/$(QTESTDIR)/all_tests.native: $(QTESTDIR)/all_tests.ml
	$(OCAMLBUILD) $(OCAMLBUILDFLAGS) -cflags -warn-error,+26\
		-pkg oUnit,$(QTESTPKG) $(QTESTDIR)/all_tests.native


### qtest: quick run of inline unit tests
##############################################
# $ make qtest TESTABLE=foo.ml
# will only test the module Foo.

qtest-byte-clean:
	@${RM} $(QTESTDIR)/all_tests.ml
	@${MAKE} _build/$(QTESTDIR)/all_tests.byte

qtest-byte: qtest-byte-clean
	@_build/$(QTESTDIR)/all_tests.byte $(QTEST_SEED_FLAG)

qtest-native-clean:
	@${RM} $(QTESTDIR)/all_tests.ml
	@${MAKE} _build/$(QTESTDIR)/all_tests.native $(QTEST_SEED_FLAG)

qtest-native: prefilter qtest-native-clean
	@_build/$(QTESTDIR)/all_tests.native $(QTEST_SEED_FLAG)

qtest-clean:
	@${RM} $(QTESTDIR)/all_tests.ml
	@${MAKE} _build/$(QTESTDIR)/all_tests.$(EXT)

qtest: qtest-clean
	@_build/$(QTESTDIR)/all_tests.$(EXT) $(QTEST_SEED_FLAG)


### run all unit tests
##############################################

testsuite-only-byte: _build/testsuite/main.byte
	@_build/testsuite/main.byte
	@echo "" # newline after "OK"

testsuite-only-native: _build/testsuite/main.native
	@_build/testsuite/main.native
	@echo "" # newline after "OK"

test-byte: qtest-byte testsuite-only-byte

test-native: qtest-native testsuite-only-native

full-test: $(TEST_TARGET)

$(PREFILTER_BIN): build/prefilter.ml
	$(OCAMLBUILD) build/prefilter.byte
# FIXME: Should probably be done by ocamlbuild somehow:
src/batteries_compattest.ml: src/batteries_compattest.mlv $(PREFILTER_BIN)
	$(PREFILTER_BIN) < $< > $@

test-compat: src/batteries_compattest.ml
	$(OCAMLBUILD) $(OCAMLBUILDFLAGS) src/batteries_compattest.byte

test-build-from-install:
	$(MAKE) -C test-build

test: test-byte test-compat

###############################################################################
#	BENCHMARK SUITE
###############################################################################

bench:
	$(OCAMLBUILD) $(OCAMLBUILDFLAGS) $(TARGETS) $(BENCH_TARGETS)
	$(RM) bench.log
	$(foreach BENCH, $(BENCH_TARGETS), _build/$(BENCH) | tee -a bench.log; )
	@echo "Benchmarking results are written to bench.log"


###############################################################################
#	PREPARING RELEASE FILES
###############################################################################

release:
	$(MAKE) clean
	git stash save "stashing local modifications before release"
	$(MAKE) release-cleaned

# assumes irreproachably pristine working directory
release-cleaned: doc test-native
	git archive --format=tar --prefix=batteries-$(VERSION)/ HEAD \
	  | gzip -9 > batteries-$(VERSION).tar.gz

# uploads the current documentation to github hdoc2/ directory
upload-docs:
	make doc && \
	rm -rf /tmp/batteries.docdir && \
	cp -a _build/batteries.docdir /tmp/ && \
	git checkout gh-pages && \
	rm -f hdoc2/*.html && \
	cp /tmp/batteries.docdir/*.html hdoc2/ && \
	git add hdoc2/*.html && \
	git commit hdoc2 -m "Update ocamldoc to latest release" && \
	git push origin gh-pages && \
	git checkout master
