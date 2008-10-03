
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

PACKS=$(shell find src -name "*mlpack")
GENERATED_MLI=$(patsubst %.mlpack, %.mli, $(PACKS))


test:
	$(shell echo $(GENERATED_MLI))

all: byte doc

_build/build/odoc_generator_batlib.cmo: build/odoc_generator_batlib.ml
	ocamlbuild odoc_generator_batlib.cmo

#byte: src/main/threaded/batteries.cma src/main/nothreads/batteries.cma
#opt:  src/main/threaded/batteries.cmxa src/main/nothreads/batteries.cmxa

#src/main/threaded/batteries.cma: list
#	ocamlbuild src/main/threaded/batteries.cma

#src/main/threaded/batteries.cmxa: list
#	ocamlbuild src/main/threaded/batteries.cmxa

#src/main/nothreads/batteries.cma: list
#	ocamlbuild src/main/nothreads/batteries.cma

#src/main/nothreads/batteries.cmxa: list
#	ocamlbuild src/main/nothreads/batteries.cmxa


byte:
	ocamlbuild -I src/main/threads   src/main/batteries_threads.cma &&\
	ocamlbuild -I src/main/nothreads src/main/batteries_nothreads.cma

opt:
	ocamlbuild -I src/main/threads   src/main/batteries_threads.cmxa &&\
	ocamlbuild -I src/main/nothreads src/main/batteries_nothreads.cmxa

install:
	ocamlfind install batteries build/META _build/src/main/batteries_threads.cmi _build/src/main/batteries_nothreads.cmi -optional _build/src/main/batteries_threads.cma _build/src/main/batteries_threads.cmxa _build/src/main/batteries_nothreads.cma _build/src/main/batteries_nothreads.cmxa

uninstall:
	ocamlfind remove batteries

#packs: $(GENERATED_MLI)
#packs:
#	ocamlbuild -classic-display $(GENERATED_MLI)

%.mli:%/*
	\rm -f $(EXTLIB_MLI) _build/$(EXTLIB_MLI) _build/src/core/extlib.cmi &&\
	ocamlbuild extlib.cmo &&\
	ocamlbuild build/packdoc.byte &&\
	./packdoc.byte -i _build/src/core/extlib/ -o src/core/extlib.mli      -pack Extlib       &&\
	install -D src/core/extlib.mli   _build/src/core/extlib.mli


doc: src/main/threaded/batteries.cma doc/api.odocl
	\rm -Rf doc/batteries/html/api &&\
	ocamlbuild -I src/main/threaded doc/api.docdir/index.html &&\
	rm api.docdir &&\
	mv _build/doc/api.docdir/ doc/batteries/html/api

doc/api.odocl: list
	@echo $(basename $(notdir $(shell find src/ -name "*.ml" -o -name "*.mlpack"))) > batteries.mllib &&\
	sed -e "s/ /\n/g" -e "s/batlib/Batlib/g" -e "s/batteries\\nbatteries/batteries/" -e "s/batteriesLayer1_nothread//\\n" batteries.mllib > src/main/threaded/batteries.mllib &&\
	echo Extlib >  doc/api.odocl &&\
	cat batteries.mllib >> doc/api.odocl

#
#Generate the list of modules we intend to put into batlib.cma
#
list: src/main/threaded/batteries.mllib src/main/nothreads/batteries.mllib

src/main/threaded/batteries.mllib:
	@echo $(basename $(notdir $(shell find src/ -name "*.ml" -o -name "*.mlpack"))) > batteries.mllib &&\
	sed -e "s/ /\n/g" -e "s/batlib/Batlib/g" -e "s/batteries\\nbatteries/batteries/" -e "s/batteriesLayer1_nothreads\\n//" batteries.mllib > src/main/threaded/batteries.mllib

src/main/nothreads/batteries.mllib:
	@echo $(basename $(notdir $(shell find src/ -name "*.ml" -o -name "*.mlpack"))) > batteries.mllib &&\
	sed -e "s/ /\n/g" -e "s/batlib/Batlib/g" -e "s/batteries\\nbatteries/batteries/" -e "s/batteriesLayer1\\n//" batteries.mllib > src/main/nothreads/batteries.mllib



prepare_syntax_contents:
	@echo $(basename $(notdir $(shell find syntax -name  "*.ml"))) > batsyntax.mllib &&\
	sed -e "s/ /\n/g" -e "s/batlib/Batlib/g"   -i batsyntax.mllib

#install:
#	ocamlfind install batteries build/threaded/META _build/src/main/threaded/batteries.cmi -optional _build/src/main/threaded/batteries.cma  _build/src/main/threaded/batteries.cmxa  _build/src/main/threaded/batteries.a  &&\
#	ocamlfind install batteries_nothreads build/nothreads/META _build/src/main/nothreads/batteries.cmi -optional _build/src/main/nothreads/batteries.cma  _build/src/main/nothreads/batteries.cmxa  _build/src/main/nothreads/batteries.a



#uninstall:
#	ocamlfind remove batteries &&\
#	ocamlfind remove batteries_nothreads

clean:
	ocamlbuild -clean &&\
	\rm -f `find . -name "*~" -o -name "*#"` &&\
	\rm -f META doc/api.odocl doc/batteries/html/api/* 

.PHONY: doc/api.odocl