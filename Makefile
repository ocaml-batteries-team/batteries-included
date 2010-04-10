# A basic Makefile for building and installing Batteries Included
# It punts to ocamlbuild for all the heavy lifting.

NAME = batteries
VERSION = 1.1.0

OCAMLBUILD ?= ocamlbuild

NATIVE_ENABLED ?= yes

.PHONY: all clean doc

all: build/META
	$(OCAMLBUILD) syntax.otarget byte.otarget src/batteries_help.cmo
	if [ X$(NATIVE_ENABLED) = Xyes ]; then \
		$(OCAMLBUILD) native.otarget; \
	fi

clean:
	rm -f build/META apidocs
	$(OCAMLBUILD) -clean

doc:
	$(OCAMLBUILD) batteries.docdir/index.html
	[ -e apidocs ] || ln -s _build/batteries.docdir apidocs

build/META: build/META.in
	sed -e 's|@VERSION@|$(VERSION)|' \
	    build/META.in > build/META

