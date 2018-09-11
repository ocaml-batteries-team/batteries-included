OPAM_DEPENDS="ocamlfind ounit qtest"

case "$OCAML_VERSION" in
3.12.1.1.0) ppa=avsm/ocaml312+opam11 ;;
4.00.1.0.0) ppa=avsm/ocaml40+opam10 ;;
4.00.1.1.0) ppa=avsm/ocaml40+opam11 ;;
4.01.0.0.0) ppa=avsm/ocaml41+opam10 ;;
4.01.0.1.0) ppa=avsm/ppa ;;
4.0[234567].*) ppa=
*) echo Unknown $OCAML_VERSION,$OPAM_VERSION; exit 1 ;;
esac

echo "yes" | sudo add-apt-repository ppa:$ppa
sudo apt-get update -qq
sudo apt-get install -qq opam
export OPAMYES=1
export OPAMVERBOSE=1

opam init --compiler=$OCAML_VERSION
eval `opam config env`

echo "==== Installing $OPAM_DEPENDS ===="
opam install ${OPAM_DEPENDS}

echo "==== Build ===="
make

echo "==== Internal tests ===="
make test-native

echo "==== Install and use test ===="
opam pin add -n -k path batteries .
make test-build-from-install
