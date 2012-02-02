set -e

./make.sh

# qtest -o out.ml --preamble 'open Batteries;;' extract t1.ml t2.ml
qtest -o out.ml extract t1.ml t2.ml

ocamlbuild -cflags -warn-error,+26 -use-ocamlfind -package oUnit out.native

./out.native