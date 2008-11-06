
# Makefile for OCaml Batteries Included
#
# Copyright (C) 2008 David Teller, LIFO, Universite d'Orleans
# 
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public
# License as published by the Free Software Foundation; either
# version 2.1 of the License, or (at your option) any later version,
# with the special exception on linking described in file LICENSE.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public
# License along with this library; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA


VERSION = 0.20081023
# place where your on-line help files are stored
# if you want on-line help, you will need to invoke make as
#      make DOCDIR="/some/where/"
# Typical value: "/usr/share/doc/batteries" (for Debian systems)
# Typical value: "/usr/local/godi/doc/godi-batteries/doc/batteries" (for GODI systems)
DOCDIR  =  $(shell pwd)/doc/batteries/

#
# command used to trigger the browser
# if your platform browser is different, you will need to invoke make as
#      make BROWSER="some command"
# Typical value: "x-www-browser %S &> /dev/null &" (for Debian/Ubuntu systems)
# Typical value: "htmlview %S &> /dev/null &"      (for Fedora/Red Hat)
# Typical value: "start %S /B"                     (for Windows)
# Typical value: "gnome-open %S &> /dev/null &"    (for Gnome)
BROWSER = gnome-open %S &> /dev/null &

#Flags to pass to OCamlBuild.
#Typical flag: -byte-plugin (if you don't have ocamlopt installed on the machine)
#Typical flag: -classic-display (for more details on the build process)
OBFLAGS =


# findlib destdir, if you need to install in non-standard places invoke make as
#      make install DESTDIR=/some/where/
DESTDIR = 

ifeq ($(DESTDIR),)
DESTDIR_FLAGS =
else
DESTDIR_FLAGS = -destdir $(DESTDIR)
install: install-mkdir
install-mkdir:
	test -d $(DESTDIR) || mkdir -p $(DESTDIR)
endif

DEST_TOP = 
ifeq ($(DESTDIR),)
DEST_TOP = $(shell ocamlfind printconf destdir)/batteries/top.ml
else
DEST_TOP = $(DESTDIR)/batteries/top.ml
endif



OCAMLBUILD=ocamlbuild $(OBFLAGS)
#OCAMLBUILD=ocamlbuild -byte-plugin -classic-display 
#Replace the first one with the second one if you have build-time issue, to help with trouble-shooting

all: byte syntax top

#Useful for testing
reinstall: byte syntax opt uninstall install
rebyte: byte syntax uninstall install
instdoc: doc
	install -D doc/batteries $(DOCDIR)

byte: config
	$(OCAMLBUILD) src/main/threads/batteries.cma     &&\
	$(OCAMLBUILD) src/main/nothreads/batteries.cma

opt: config
	$(OCAMLBUILD) src/main/threads/batteries.cmxa    &&\
	$(OCAMLBUILD) src/main/nothreads/batteries.cmxa

syntax: config
	$(OCAMLBUILD) src/syntax/pa_openin/pa_openin.cmo       &&\
	$(OCAMLBUILD) src/syntax/pa_openin/pa_openin_r.cmo     &&\
	$(OCAMLBUILD) src/syntax/pa_where/pa_where.cmo         &&\
	$(OCAMLBUILD) src/syntax/pa_batteries/pa_batteries.cmo

config: config.post
config.post: config.pre
	cp config.pre config.post                                                      &&\
	echo "let documentation_root = \"$(DOCDIR)\";;"                 >> config.post &&\
	echo "let (browser:(_, _, _) format) = \"$(BROWSER)\";;"        >> config.post


top:
	echo "ocaml -init $(DEST_TOP) \$$@" > src/batteries_toolchain/bocaml &&\
	chmod ugo+rx src/batteries_toolchain/ocaml* &&\
	ocamlbuild src/batteries_toolchain/batteries_help.cmo

install: syntax top
	ocamlfind install $(DESTDIR_FLAGS) batteries \
		build/META \
		_build/src/core/extlib/IO.cmi \
		_build/src/core/extlib/innerIO.cmi \
		_build/src/core/extlib.cmi \
		_build/src/core/batteries_config.cmi \
		_build/src/core/toolchain.cmi \
		_build/src/syntax/pa_openin/pa_openin.cmo \
		_build/src/syntax/pa_openin/pa_openin_r.cmo \
		_build/src/syntax/pa_where/pa_where.cmo \
		_build/src/syntax/pa_batteries/pa_batteries.cmo \
		_build/src/batteries_toolchain/batteries_help.cmo \
		_build/src/batteries_toolchain/batteries_help.cmi \
		src/batteries_toolchain/top.ml \
		src/batteries_toolchain/ocaml*
	ocamlfind install $(DESTDIR_FLAGS) batteries_threads \
		build/threaded/META \
		_build/src/main/threads/batteries.cmi \
		-optional _build/src/main/threads/batteries.cma \
			_build/src/main/threads/batteries.cmxa  \
			_build/src/main/threads/batteries.a
	ocamlfind install $(DESTDIR_FLAGS) batteries_nothreads \
		build/nothreads/META \
		_build/src/main/nothreads/batteries.cmi \
		-optional _build/src/main/nothreads/batteries.cma \
			_build/src/main/nothreads/batteries.cmxa \
			_build/src/main/nothreads/batteries.a

uninstall:
	ocamlfind remove $(DESTDIR_FLAGS) batteries
	ocamlfind remove $(DESTDIR_FLAGS) batteries_threads
	ocamlfind remove $(DESTDIR_FLAGS) batteries_nothreads

doc: byte doc/api.odocl
	\rm -Rf doc/batteries/html/api &&\
	$(OCAMLBUILD) -I src/main/threads doc/api.docdir/index.html &&\
	rm api.docdir &&\
	mv _build/doc/api.docdir/ doc/batteries/html/api

doc/api.odocl: 
	cp src/main/threads/batteries.mllib doc/api.odocl

examples:
	@echo Note: to build the examples, you must first have installed Batteries     &&\
	echo If you haven\'t installed Batteries yet, please use    make byte opt install   &&\
	cd examples &&\
	$(OCAMLBUILD) examples.otarget

clean:
	$(OCAMLBUILD) -clean &&\
	cd examples && \
	$(OCAMLBUILD) -clean &&\
	cd .. &&\
	\rm -f `find . -name "*~" -o -name "*#" -o -name "*odoc"` &&\
	\rm -f META doc/api.odocl doc/batteries/html/api/* config.post

.PHONY: doc/api.odocl batteries.mllib examples
