set -e # stop on first error
./make.sh
qtest -o footest.ml extract foo.ml
ocamlbuild -cflags -warn-error,+26 -use-ocamlfind -package oUnit footest.native
./footest.native  