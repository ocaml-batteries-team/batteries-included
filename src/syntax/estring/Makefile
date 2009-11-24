# Makefile
# --------
# Copyright : (c) 2009, Jeremie Dimino <jeremie@dimino.org>
# Licence   : BSD3
#
# This file is a part of estring.

OF = ocamlfind

NAME = estring
VERSION = $(shell head -n 1 VERSION)

.PHONY: all
all: META pa_estring.cmo pa_estring_top.cmo sample/sample

pa_estring.cmi: pa_estring.mli
	ocamlc -I +camlp4 -c pa_estring.mli

pa_estring.cmo: pa_estring.cmi pa_estring.ml
	ocamlc -I +camlp4 -pp camlp4of -c pa_estring.ml

sample/pa_string_list.cmo: pa_estring.cmo sample/pa_string_list.ml
	ocamlc -I +camlp4 -pp camlp4of pa_estring.cmo -c sample/pa_string_list.ml

sample/sample: pa_estring.cmo sample/pa_string_list.cmo sample/sample.ml
	ocamlc -pp 'camlp4o pa_estring.cmo sample/pa_string_list.cmo' sample/sample.ml -o sample/sample

pa_estring_top.cmo: pa_estring.cmo pa_estring_top.ml
	ocamlc -I +camlp4 -c pa_estring_top.ml

META: VERSION META.in
	sed -e 's/@VERSION@/$(VERSION)/' META.in > META

.PHONY: dist
dist:
	DARCS_REPO=$(PWD) darcs dist --dist-name $(NAME)-$(VERSION)

.PHONY: install
install:
	$(OF) install $(NAME) META pa_estring.cmo pa_estring.cmi pa_estring_top.cmo

.PHONY: uninstall
uninstall:
	$(OF) remove $(NAME)

.PHONY: clean
clean:
	rm -f META $(NAME)-*.tar.gz *.cm* sample/*.cm* sample/sample
