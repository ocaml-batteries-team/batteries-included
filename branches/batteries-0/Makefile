all: byte doc

byte: list
	ocamlbuild batlib.cma

opt: list doc
	ocamlbuild batlib.cmxa

doc: list
	ocamlbuild batlib.docdir/index.html;
	cp batlib.docdir/Batlib.html batlib.docdir/index.html

list: 
	@echo $(basename $(notdir $(shell find batlib glue -name  "*.ml"))) > batlib.mllib &&\
	sed -e "s/ /\n/g" -e "s/batlib/Batlib/g"   -i batlib.mllib


install:
	ocamlfind install batlib META $(wildcard _build/batlib/batlib.cma) $(wildcard _build/batlib/batlib.cmxa) $(wildcard _build/batlib/batlib.cmi) 

uninstall:
	ocamlfind remove batlib

clean:
	ocamlbuild -clean &&\
	\rm -f `find . -name "*~"`