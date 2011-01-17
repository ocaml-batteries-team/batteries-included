# A basic Makefile for building and installing Batteries Included
# It punts to ocamlbuild for all the heavy lifting.

NAME = batteries

# This is also defined in the VERSION file
VERSION := $(shell cat VERSION)

# Define variables and export them for mkconf.ml
DOCROOT ?= /usr/share/doc/ocaml-batteries
export DOCROOT
BROWSER_COMMAND ?= x-www-browser %s
export BROWSER_COMMAND

ifdef DESTDIR
OCAMLFIND_DEST += -destdir $(DESTDIR)
endif

OCAMLBUILD ?= ocamlbuild

BATTERIES_NATIVE ?= yes
BATTERIES_NATIVE_SHLIB ?= yes

INSTALL_FILES = _build/META _build/src/*.cma \
	battop.ml _build/src/*.cmi _build/src/*.mli \
	_build/src/batteries_help.cmo \
	_build/src/syntax/pa_comprehension/pa_comprehension.cmo \
	_build/src/syntax/pa_strings/pa_strings.cma
NATIVE_INSTALL_FILES = _build/src/*.cmx _build/src/*.a _build/src/*.cmxa

# What to build
TARGETS = syntax.otarget byte.otarget src/batteries_help.cmo META
TEST_TARGETS = testsuite/main.byte

ifeq ($(BATTERIES_NATIVE_SHLIB), yes)
  TARGETS += shared.otarget
  TEST_TARGETS += testsuite/main.native
  INSTALL_FILES += $(NATIVE_INSTALL_FILES) _build/src/*.cmxs
else ifeq ($(BATTERIES_NATIVE), yes)
  TARGETS += native.otarget
  TEST_TARGETS += testsuite/main.native
  INSTALL_FILES += $(NATIVE_INSTALL_FILES)
endif

.PHONY: all clean doc install uninstall reinstall test qtest fix_camomile camomile82 camomile81 camomile7 camfail

all: fix-camomile
	test ! -e src/batteries_config.ml || rm src/batteries_config.ml
	$(OCAMLBUILD) $(TARGETS)

clean:
	rm -f apidocs
	$(OCAMLBUILD) -clean

doc:
	$(OCAMLBUILD) batteries.docdir/index.html
	test -e apidocs || ln -s _build/batteries.docdir apidocs

install: all
	ocamlfind install $(OCAMLFIND_DEST) estring \
		libs/estring/META \
		_build/libs/estring/*.cmo \
		_build/libs/estring/*.cmi \
		_build/libs/estring/*.mli
	ocamlfind install $(OCAMLFIND_DEST) $(NAME) $(INSTALL_FILES)

uninstall:
	ocamlfind remove $(OCAMLFIND_DEST) estring
	ocamlfind remove $(OCAMLFIND_DEST) $(NAME)
	rm -rf $(DOCROOT)

install-doc: doc
	mkdir -p $(DOCROOT)
	cp -r doc/batteries/* $(DOCROOT)
	# deal with symlink that will break
	rm -f $(DOCROOT)/html/batteries_large.png
	cp -f doc/batteries_large.png $(DOCROOT)/html
	mkdir -p $(DOCROOT)/html/api
	cp apidocs/* $(DOCROOT)/html/api
	cp LICENSE README FAQ VERSION $(DOCROOT)

reinstall:
	$(MAKE) uninstall
	$(MAKE) install

test: 
	$(OCAMLBUILD) $(TARGETS) $(TEST_TARGETS)
	$(foreach TEST, $(TEST_TARGETS), _build/$(TEST); )

release: test
	git archive --format=tar --prefix=batteries-$(VERSION)/ HEAD \
	  | gzip > batteries-$(VERSION).tar.gz


##
## Magic to detect which version of camomile is installed 
##

CAMVER=$(shell sh -c 'ocamlfind list | grep camomile | grep -o "[0-9\.]*"')
ifeq ($(CAMVER),0.8.2)
	CAMFIX=camomile82
endif
ifeq ($(CAMVER),0.8.1)
	CAMFIX=camomile81
endif
ifeq ($(CAMVER),0.7.2)
	CAMFIX=camomile7
endif
ifeq ($(CAMVER),)
	CAMFIX=camfail
endif

fix-camomile: $(CAMFIX)

#replace batcamomile with a version appropriate for camomile 0.8.2
camomile82:
	cp -f src/batCamomile-0.8.2.ml src/batCamomile.ml

#replace batcamomile with a version appropriate for camomile 0.8.1
camomile81:
	cp -f src/batCamomile-0.8.1.ml src/batCamomile.ml

#replace batcamomile with a version appropriate for camomile 0.7.*
camomile7:
	cp -f src/batCamomile-0.7.ml src/batCamomile.ml

camfail:
	echo "Camomile not detected, cannot compile batteries"
	exit 1

##
## Magic for qtest target - auto-generated test files from source comments
##

#convert a source file to a test suite by filtering special comments
qtest/%_t.ml: src/%.ml
	ruby build/make_suite.rb $^ > $@

#List of source files that it's okay to try to test
DONTTEST=$(wildcard src/batCamomile-*.ml) src/batteries_help.ml
TESTABLE=$(filter-out $(DONTTEST), $(wildcard src/*.ml))
#TESTABLE=src/batString.ml

#put all the testing modules in a library
qtest/test_mods.mllib:
	echo -n "Quickcheck Tests " > $@
	echo $(patsubst src/%.ml,%_t, $(TESTABLE)) >> $@

#compile test_runners which need the test_mods.cm?a library
qtest: $(patsubst src/%.ml,qtest/%_t.ml, $(TESTABLE)) qtest/test_mods.mllib
	ocamlbuild qtest/test_runner.byte qtest/test_runner.native
	./test_runner.byte
	./test_runner.native
