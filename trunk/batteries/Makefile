
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


byte:
	ocamlbuild src/main/threads/batteries.cma     &&\
	ocamlbuild src/main/nothreads/batteries.cma

opt:
	ocamlbuild src/main/threads/batteries.cmxa    &&\
	ocamlbuild src/main/nothreads/batteries.cmxa

syntax:
	ocamlbuild src/syntax/pa_openin/pa_openin.cmo &&\
	ocamlbuild src/syntax/pa_where/pa_where.cmo

batteries.mllib:
#	echo $(foreach i, $(basename $(notdir $(shell find src/ -name "*.ml" -o -name "*.mlpack"))), $(shell ocaml build/tools.ml --capit $i)) > batteries.mllib &&\
	sed -i -e "s/ /\n/g" -e "s/Batteries\\nBatteries/Batteries/" \
		batteries.mllib &&\
	cp batteries.mllib src/main/threads       &&\
#	echo $(foreach i, $(basename $(notdir $(shell find src \( -name "*ml" -or -name "*mlpack" \) -not -wholename "*baselib_threads*"))), $(shell ocaml build/tools.ml --capit $i)) > batteries.mllib &&\
	sed -i -e "s/ /\n/g" -e "s/Batteries\\nBatteries/Batteries/" \
		batteries.mllib &&\
	cp batteries.mllib src/main/nothreads

#batteries.mllib:
#	echo $(foreach i, $(basename $(notdir $(shell find src/ -name "*.ml" -o -name "*.mlpack"))), $(shell ocaml build/tools.ml --capit $i)) > batteries.mllib &&\
#	sed -i -e "s/ /\n/g" -e "s/batteries\\nbatteries/batteries/" \
#		batteries.mllib &&\
#	cp batteries.mllib src/main/threads       &&\
#	echo $(foreach i, $(basename $(notdir $(shell find src \( -name "*ml" -or -name "*mlpack" \) -not -wholename "*baselib_threads*"))), $(shell ocaml build/tools.ml --capit $i)) > batteries.mllib &&\
#	sed -i -e "s/ /\n/g" -e "s/batteries\\nbatteries/batteries/" \
#		batteries.mllib &&\
#	cp batteries.mllib src/main/nothreads

install: syntax
	ocamlfind install batteries build/META _build/src/syntax/pa_openin/pa_openin.cmo _build/src/syntax/pa_where/pa_where.cmo &&\
	ocamlfind install batteries_threads build/threaded/META _build/src/main/threads/batteries.cmi -optional _build/src/main/threads/batteries.cma _build/src/main/threads/batteries.cmxa _build/src/main/threads/batteries.a  &&\
	ocamlfind install batteries_nothreads build/nothreads/META _build/src/main/nothreads/batteries.cmi -optional _build/src/main/nothreads/batteries.cma _build/src/main/nothreads/batteries.cmxa  _build/src/main/nothreads/batteries.a 

uninstall:
	ocamlfind remove batteries &&\
	ocamlfind remove batteries_threads &&\
	ocamlfind remove batteries_nothreads

#packs: $(GENERATED_MLI)
#packs:
#	ocamlbuild -classic-display $(GENERATED_MLI)

%.mli:%/*
	\rm -f $(EXTLIB_MLI) _build/$(EXTLIB_MLI) _build/src/core/extlib.cmi &&\
	ocamlbuild extlib.cmo &&\
	ocamlbuild build/packdoc.byte &&\
	./packdoc.byte -i _build/src/core/extlib/ -o src/core/extlib.mli      -pack Extlib       &&\
	install -D src/core/extlib.mli   _build/src/core/extlib.mli


doc: byte doc/api.odocl
	\rm -Rf doc/batteries/html/api &&\
	ocamlbuild -I src/main/threads doc/api.docdir/index.html &&\
	rm api.docdir &&\
	mv _build/doc/api.docdir/ doc/batteries/html/api

doc/api.odocl: 
	cp src/main/threads/batteries.mllib doc/api.odocl

prepare_syntax_contents:
	@echo $(basename $(notdir $(shell find syntax -name  "*.ml"))) > batsyntax.mllib &&\
	sed -e "s/ /\n/g" -e "s/batlib/Batlib/g"   -i batsyntax.mllib

clean:
	ocamlbuild -clean &&\
	\rm -f `find . -name "*~" -o -name "*#" -o -name "*odoc"` &&\
	\rm -f META doc/api.odocl doc/batteries/html/api/* 

.PHONY: doc/api.odocl batteries.mllib
