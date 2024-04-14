#!/bin/bash

# if you want to 'make clean && make && make test' batteries under
# a freshly created ocaml compiler switch, this script will install
# all the required dependencies

opam install benchmark camlp-streams dune num ounit qtest
