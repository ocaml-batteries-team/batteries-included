#!/bin/bash

#set -x

temp_dir=`mktemp -d`

cat<<EOF > $temp_dir/install_test.ml
open Batteries
let () =
  assert(List.takedrop 2 [1;2;3;4] = ([1;2], [3;4]));
  Printf.printf "install_test: OK\n"
EOF

make clean # force rebuild next
make install && \
    cd $temp_dir && \
    rm -f install_test.native && \
    ocamlbuild -pkg batteries install_test.native && \
    ./install_test.native

cd - # go back where we were before
rm -rf $temp_dir # clean our mess
