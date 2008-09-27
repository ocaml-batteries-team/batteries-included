
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

all: byte doc

#doc: _build/build/odoc_generator_batlib.cmo byte list
#	 ocamldoc -I _build/batlib `find _build/juice -type d -exec echo -I \{\} \;` -g _build/build/odoc_generator_batlib.cmo -d doc `find . -name "*.ml" -o -name "*.mli"`

_build/build/odoc_generator_batlib.cmo: build/odoc_generator_batlib.ml
	ocamlbuild odoc_generator_batlib.cmo

byte: src/main/threaded/batteries.cma src/main/nothreads/batteries.cma
opt:  src/main/threaded/batteries.cmxa src/main/nothreads/batteries.cmxa

src/main/threaded/batteries.cma: list packs
	ocamlbuild src/main/threaded/batteries.cma

src/main/threaded/batteries.cmxa: list packs
	ocamlbuild src/main/threaded/batteries.cmxa

src/main/nothreads/batteries.cma: list packs
	ocamlbuild src/main/nothreads/batteries.cma

src/main/nothreads/batteries.cmxa: list packs
	ocamlbuild src/main/nothreads/batteries.cmxa

MLI=src/additions/extlib.mli

packs:
	\rm -f $(MLI) _build/$(MLI) _build/src/additions/extlib.cmi &&\
	ocamlbuild extlib.cmo &&\
	ocamlbuild build/packdoc.byte &&\
	./packdoc.byte -i _build/src/additions/extlib/ -o src/additions/extlib.mli      -pack Extlib       &&\
	install -D src/additions/extlib.mli   _build/src/additions/extlib.mli                                        
#	ocamlbuild extlib.cmo         &&\
#	ocamlbuild baselibGlue.cmo    &&\
#	ocamlbuild findlibGlue.cmo    &&\
#	ocamlbuild batteriesGlue.cmo  &&\
#	./packdoc.byte -i _build/src/glue/baselib/     -o src/glue/baselibGlue.mli      -pack BaselibGlue  &&\
#	./packdoc.byte -i _build/src/glue/findlib/     -o src/glue/findlibGlue.mli      -pack FindlibGlue  &&\
#	./packdoc.byte -i _build/src/glue/             -o src/batteriesGlue.mli         -pack BatteriesGlue&&\
#	install -D src/glue/baselibGlue.mli   _build/src/glue/baselibGlue.mli                                        &&\
#	install -D src/glue/findlibGlue.mli   _build/src/glue/findlibGlue.mli                                        &&\
#	install -D src/batteriesGlue.mli      _build/src/batteriesGlue.mli


doc: src/main/threaded/batteries.cma doc/api.odocl
	\rm -Rf doc/batteries/html/api &&\
	ocamlbuild -I src/main/threaded doc/api.docdir/index.html &&\
	rm api.docdir &&\
	mv _build/doc/api.docdir/ doc/batteries/html/api

doc/api.odocl: list
#	echo Batteries > doc/api.odocl
	echo Extlib >  doc/api.odocl
	cat batteries.mllib >> doc/api.odocl

#
#Generate the list of modules we intend to put into batlib.cma
#
list: batteries.mllib

batteries.mllib:
	@echo $(basename $(notdir $(shell find src/ -name  "*.ml"))) > batteries.mllib &&\
	sed -e "s/ /\n/g" -e "s/batlib/Batlib/g" -e "s/batteries\\nbatteries/batteries/" -i batteries.mllib                   &&\
	cp batteries.mllib src/main/threaded                                           &&\
	cp batteries.mllib src/main/nothreads



prepare_syntax_contents:
	@echo $(basename $(notdir $(shell find syntax -name  "*.ml"))) > batsyntax.mllib &&\
	sed -e "s/ /\n/g" -e "s/batlib/Batlib/g"   -i batsyntax.mllib

install:
	ocamlfind install batteries build/threaded/META _build/src/main/threaded/batteries.cmi -optional _build/src/main/threaded/batteries.cma  _build/src/main/threaded/batteries.cmxa  _build/src/main/threaded/batteries.a  &&\
	ocamlfind install batteries_nothreads build/nothreads/META _build/src/main/nothreads/batteries.cmi -optional _build/src/main/nothreads/batteries.cma  _build/src/main/nothreads/batteries.cmxa  _build/src/main/nothreads/batteries.a


uninstall:
	ocamlfind remove batteries &&\
	ocamlfind remove batteries_nothreads

clean:
	ocamlbuild -clean &&\
	\rm -f `find . -name "*~" -o -name "*#"` &&\
	\rm -f META batteries_agnostic.mllib doc/api.odocl doc/batteries/html/api/* src/additions/extlib.mli 

.PHONY: batteries.mllib doc/api.odocl