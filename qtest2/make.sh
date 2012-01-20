
set -e

name='qtest'
target=$name.native

# -classic-display -use-menhir
# ocamlbuild -I +threads -cflag -thread -libs unix,str,threads $target && ./$target $@
# ocamlbuild -use-ocamlfind -pkg threads -pkg threads -cflag -thread -libs unix,str $target && ./$target $@
ocamlbuild   -libs unix,str $target && ./$target $@

cp $target ~/bin/$name -v

