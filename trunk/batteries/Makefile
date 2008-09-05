
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

byte: list
	ocamlbuild batlib.cma

opt: list
	ocamlbuild batlib.cmxa

doc: list
	ocamlbuild batteries.docdir/index.html;
	cp batteries.docdir/Batlib.html batteries.docdir/index.html

list: 
	@echo $(basename $(notdir $(shell find batlib juice -name  "*.ml"))) > batlib.mllib &&\
	sed -e "s/ /\n/g" -e "s/batlib/Batlib/g"   -i batlib.mllib


META: build/META
	cp build/META .

install: META
	ocamlfind install batlib META $(wildcard _build/batlib/batlib.cma) $(wildcard _build/batlib/batlib.cmxa) $(wildcard _build/batlib/batlib.cmi) 

uninstall:
	ocamlfind remove batlib

clean:
	ocamlbuild -clean &&\
	\rm -f `find . -name "*~" -o -name "*#"`