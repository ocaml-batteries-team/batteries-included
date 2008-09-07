
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

byte: batteries_layer_1.cma batteries.cma
	
opt: batteries_layer_1.cmxa batteries.cmxa
	

batteries_layer_1.cma: batteries_layer_1.mllib
	ocamlbuild batteries_layer_1.cma

batteries.cma: batteries_layer_1.cma
	ocamlbuild batteries.cma

batteries_layer_1.cmxa: batteries_layer_1.mllib
	ocamlbuild batteries_layer_1.cmxa

batteries.cmxa: batteries_layer_1.cmxa
	ocamlbuild batteries.cmxa

doc: doc/api.odocl
	ocamlbuild doc/api.docdir/index.html &&\
	rm api.docdir &&\
	ln -sf ../_build/doc/api.docdir/ -T doc/batteries/html/api

doc/api.odocl: batteries_layer_1.mllib
	echo `cat batteries_layer_1.mllib` Batteries > doc/api.odocl

#
#Generate the list of modules we intend to put into batlib.cma
#
list: batteries_layer_1.mllib
batteries_layer_1.mllib: 
	@echo $(basename $(notdir $(shell find src/layer_1 -name  "*.ml"))) > batteries_layer_1.mllib &&\
	sed -e "s/ /\n/g" -e "s/batlib/Batlib/g"   -i batteries_layer_1.mllib                          

prepare_syntax_contents:
	@echo $(basename $(notdir $(shell find syntax -name  "*.ml"))) > batsyntax.mllib &&\
	sed -e "s/ /\n/g" -e "s/batlib/Batlib/g"   -i batsyntax.mllib

META: build/META
	cp build/META .

install: META
	ocamlfind install batteries META $(shell find _build/ -name "batteries.cma" -o -name "batteries.cmxa" -o -name "batteries.cmi" -o -name "batteries_layer_1.cma" -o -name "batteries_layer_1.cmxa" -o -name "batteries_layer_1.cmi") 

uninstall:
	ocamlfind remove batteries

clean:
	ocamlbuild -clean &&\
	\rm -f `find . -name "*~" -o -name "*#"`