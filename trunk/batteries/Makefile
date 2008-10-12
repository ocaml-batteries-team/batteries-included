
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


#Flags to pass to OCamlBuild.
#Typical flag: -byte-plugin (if you don't have ocamlopt installed on the machine)
#Typical flag: -classic-display (for more details on the build process)
OBFLAGS =


# findlib destdir, if you need to install in non-standard places invoke make
# as: "make install DESTDIR=/some/where/"
DESTDIR =

ifeq ($(DESTDIR),)
DESTDIR_FLAGS =
else
DESTDIR_FLAGS = -destdir $(DESTDIR)
endif

OCAMLBUILD=ocamlbuild $(OBFLAGS)
#OCAMLBUILD=ocamlbuild -byte-plugin -classic-display 
#Replace the first one with the second one if you have build-time issue, to help with trouble-shooting

all: byte syntax

#Useful for testing
reinstall: byte syntax opt uninstall install

_build/build/odoc_generator_batlib.cmo: build/odoc_generator_batlib.ml
	$(OCAMLBUILD) odoc_generator_batlib.cmo


byte:
	$(OCAMLBUILD) src/main/threads/batteries.cma     &&\
	$(OCAMLBUILD) src/main/nothreads/batteries.cma

opt:
	$(OCAMLBUILD) src/main/threads/batteries.cmxa    &&\
	$(OCAMLBUILD) src/main/nothreads/batteries.cmxa

syntax:
	$(OCAMLBUILD) src/syntax/pa_openin/pa_openin.cmo       &&\
	$(OCAMLBUILD) src/syntax/pa_openin/pa_openin_r.cmo     &&\
	$(OCAMLBUILD) src/syntax/pa_where/pa_where.cmo         &&\
	$(OCAMLBUILD) src/syntax/pa_batteries/pa_batteries.cmo

batteries.mllib:
#	echo $(foreach i, $(basename $(notdir $(shell find src/ -name "*.ml" -o -name "*.mlpack"))), $(shell ocaml build/tools.ml --capit $i)) > batteries.mllib &&\
	sed -i -e "s/ /\n/g" -e "s/Batteries\\nBatteries/Batteries/" \
		batteries.mllib &&\
	cp batteries.mllib src/main/threads       &&\
#	echo $(foreach i, $(basename $(notdir $(shell find src \( -name "*ml" -or -name "*mlpack" \) -not -wholename "*baselib_threads*"))), $(shell ocaml build/tools.ml --capit $i)) > batteries.mllib &&\
	sed -i -e "s/ /\n/g" -e "s/Batteries\\nBatteries/Batteries/" \
		batteries.mllib &&\
	cp batteries.mllib src/main/nothreads

install: syntax
	ocamlfind install $(DESTDIR_FLAGS) batteries \
		build/META \
		_build/src/syntax/pa_openin/pa_openin.cmo \
		_build/src/syntax/pa_openin/pa_openin_r.cmo \
		_build/src/syntax/pa_where/pa_where.cmo \
		_build/src/syntax/pa_batteries/pa_batteries.cmo
	ocamlfind install $(DESTDIR_FLAGS) batteries_threads \
		build/threaded/META \
		_build/src/main/threads/batteries.cmi \
		-optional _build/src/main/threads/batteries.cma \
			_build/src/main/threads/batteries.cmxa \
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
	\rm -f `find . -name "*~" -o -name "*#" -o -name "*odoc"` &&\
	\rm -f META doc/api.odocl doc/batteries/html/api/* 

.PHONY: doc/api.odocl batteries.mllib examples
