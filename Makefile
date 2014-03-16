# OASIS_START
# DO NOT EDIT (digest: 7b2408909643717852b95f994b273fee)

SETUP = ocaml setup.ml

build: setup.data
	$(SETUP) -build $(BUILDFLAGS)

doc: setup.data build
	$(SETUP) -doc $(DOCFLAGS)

test: setup.data build
	$(SETUP) -test $(TESTFLAGS)

all:
	$(SETUP) -all $(ALLFLAGS)

install: setup.data
	$(SETUP) -install $(INSTALLFLAGS)

uninstall: setup.data
	$(SETUP) -uninstall $(UNINSTALLFLAGS)

reinstall: setup.data
	$(SETUP) -reinstall $(REINSTALLFLAGS)

clean:
	$(SETUP) -clean $(CLEANFLAGS)

distclean:
	$(SETUP) -distclean $(DISTCLEANFLAGS)

setup.data:
	$(SETUP) -configure $(CONFIGUREFLAGS)

.PHONY: build doc test all install uninstall reinstall clean distclean configure

# OASIS_STOP

###############################################################################
#	Pre-Processing of Source Code
###############################################################################

PREPROCESSED_FILES = src/batMarshal.mli src/unix/batUnix.mli \
		     src/full/batInnerPervasives.ml src/batHashtbl.ml \
		     src/batFun.ml src/batFun.mli \
		     src/batPrintexc.mli src/batSys.mli \
		     src/bigarray/batBigarray.mli

prefilter: $(PREPROCESSED_FILES)

# Ocaml 4.00 can benefit strongly from some pre-processing to expose
# slightly different interfaces
.SUFFIXES: .mli .mliv .ml .mlv

# Look for lines starting with ##Vx##, and delete just the tag or the
# whole line depending whether the x matches the ocaml major version
.mliv.mli:
	ocaml str.cma build/prefilter.ml < $^ > $@

.mlv.ml:
	ocaml str.cma build/prefilter.ml < $^ > $@

###############################################################################
#	BUILDING AND RUNNING UNIT TESTS
###############################################################################

OCAMLBUILD ?= ocamlbuild
OCAMLBUILDFLAGS ?= -no-links

### List of source files that it's okay to try to test

DONTTEST=src/batteriesHelp.ml src/yojson/batYojson.ml \
	 src/sexp/batSexp.ml src/bencode/batBencode.ml
TESTABLE ?= $(wildcard src/bat[^t]*.ml) \
	    $(filter-out $(DONTTEST), $(wildcard src/**/*.ml))
TESTDEPS = prefilter $(TESTABLE)

### Test suite: "offline" unit tests
##############################################

_build/testsuite/main.byte: $(TESTDEPS) $(wildcard testsuite/*.ml)
	$(OCAMLBUILD) $(OCAMLBUILDFLAGS) testsuite/main.byte
_build/testsuite/main.native: $(TESTDEPS) $(wildcard testsuite/*.ml)
	$(OCAMLBUILD) $(OCAMLBUILDFLAGS) testsuite/main.native

### qtest: "inline" unit tests
##############################################


# Directory where to build the qtest suite
QTESTDIR ?= qtest

# extract all qtest unit tests into a single ml file
$(QTESTDIR)/all_tests.ml: $(TESTABLE)
	@echo "TESTABLE: " $(TESTABLE)
	qtest -o $@ --preamble-file qtest/qtest_preamble.ml \
	    extract $(TESTABLE)

	#qtest -o $@ --shuffle --preamble-file qtest/qtest_preamble.ml \

QTEST_INCLUDES = -I src -I src/full \
		 -I src/thread -I src/num -I src/unix -I src/bigarray
QTEST_OPTIONS = -cflags -warn-error,+26,-thread -lflags -thread -use-ocamlfind \
		$(QTEST_INCLUDES) -pkg oUnit,QTest2Lib,num,threads,str,bigarray

_build/$(QTESTDIR)/all_tests.byte: $(QTESTDIR)/all_tests.ml
	$(OCAMLBUILD) $(OCAMLBUILDFLAGS) $(QTEST_OPTIONS) \
	    $(QTESTDIR)/all_tests.byte
_build/$(QTESTDIR)/all_tests.native: $(QTESTDIR)/all_tests.ml
	$(OCAMLBUILD) $(OCAMLBUILDFLAGS) $(QTEST_OPTIONS) \
	    $(QTESTDIR)/all_tests.native

EXT=native

qtest-clean:
	@${RM} $(QTESTDIR)/all_tests.ml
	@${MAKE} _build/$(QTESTDIR)/all_tests.$(EXT)

qtest: prefilter qtest-clean
	@_build/$(QTESTDIR)/all_tests.$(EXT)

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
release-cleaned: setup.ml doc test
	git archive --format=tar --prefix=batteries-$(VERSION)/ HEAD \
	  | gzip > batteries-$(VERSION).tar.gz

setup.ml: _oasis
	oasis setup
	git commit setup.ml -m"Update setup.ml based on _oasis"


# uploads the current documentation to github hdoc2/ directory
upload-docs:
	make doc && git checkout gh-pages && rm -f hdoc2/*.html && cp _build/batteries.docdir/*.html hdoc2/ && git add hdoc2/*.html && git commit -a -m"Update to latest documentation" && git push github gh-pages

###############################################################################
#	CODE COVERAGE REPORTS
###############################################################################

coverage/index.html: $(TESTDEPS) $(QTESTDIR)/all_tests.ml
	$(OCAMLBUILD) -use-ocamlfind coverage/index.html

coverage: coverage/index.html


.PHONY: prefilter qtest qtest-clean 
