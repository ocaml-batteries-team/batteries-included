TESTS=sexp where_revised where threads nothreads openin openin_revised
BYTE_TARGETS=  $(patsubst %, test_%.byte, $(TESTS))
NATIVE_TARGETS=$(patsubst %, test_%.native, $(TESTS))
COMPILE= $(BYTE_TARGETS) $(NATIVE_TARGETS)

all: library clean tests

library:
	cd ../batteries/ && make byte opt uninstall install

tests: $(COMPILE)

%.byte:
	ocamlbuild $@

%.native:
	ocamlbuild $@

clean:
	ocamlbuild -clean
