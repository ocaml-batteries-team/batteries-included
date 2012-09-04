.PHONY: all clean

SOURCES = $(wildcard euler*.ml)
TARGETS = $(SOURCES:.ml=.byte)
LIBRARIES = mathlib.ml

all: $(TARGETS)

clean:
	rm *.byte *.native *.cmi *.cmo

%.byte: $(LIBRARIES) %.ml
	ocamlfind ocamlc -thread -package threads,batteries -linkpkg $^ -o $@

%.native: $(LIBRARIES) %.ml
	ocamlfind ocamlc -thread -package threads,batteries -linkpkg $^ -o $@