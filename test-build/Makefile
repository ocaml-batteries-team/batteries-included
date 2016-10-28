# This test is designed to catch build issues that affect installed
# versions of the library, such as the ones that plagued v2.5.0 and
# v2.5.1 -- forgetting to include a new module in src/batteries.mllib,
# which results in a link-time error when building from an installed
# version.
all:
	ocamlfind ocamlopt -package batteries -o test -linkpkg test.ml
	./test | grep --quiet "0123456789" || exit 2
	rm test.cm* test.o test
