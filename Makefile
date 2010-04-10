# A basic Makefile for building and installing Batteries Included
# It punts to ocamlbuild for all the heavy lifting.

NAME = batteries
VERSION = 1.1.0
DOCROOT ?= /usr/share/doc/ocaml-batteries
BROWSER_COMMAND ?= x-www-browser %s

OCAMLBUILD ?= ocamlbuild

BATTERIES_NATIVE ?= yes

.PHONY: all clean doc

all: build/META
	test ! -e src/batteries_config.ml || rm src/batteries_config.ml
	$(OCAMLBUILD) syntax.otarget byte.otarget src/batteries_help.cmo
	test X$(BATTERIES_NATIVE) != Xyes || $(OCAMLBUILD) native.otarget

clean:
	rm -f build/META apidocs
	$(OCAMLBUILD) -clean

doc:
	$(OCAMLBUILD) batteries.docdir/index.html
	test -e apidocs || ln -s _build/batteries.docdir apidocs

build/META: build/META.in
	sed -e 's|@VERSION@|$(VERSION)|' \
	    build/META.in > build/META

src/batteries_config.ml: src/batteries_config.mlp
	sed -e 's|@VERSION@|$(VERSION)|' \
            -e 's|@DOCROOT@|$(DOCROOT)|' \
            -e 's|@BROWSER_COMMAND@|$(BROWSER_COMMAND)|' \
	    src/batteries_config.mlp >src/batteries_config.ml
