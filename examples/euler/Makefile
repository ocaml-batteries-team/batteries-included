.PHONY: all clean

SOURCES = $(wildcard euler*.ml)
TARGETS = $(SOURCES:.ml=.byte)

all: $(TARGETS)

clean: 
	rm *.byte *.native *.cmi *.cmo

%.byte: mathlib.ml %.ml 
	ocamlfind ocamlc -thread -package threads,batteries -linkpkg $^ -o $@ 

%.native: mathlib.ml %.ml 
	ocamlfind ocamlc -thread -package threads,batteries -linkpkg $^ -o $@ 