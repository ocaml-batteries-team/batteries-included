#TESTS=ref random2 random sexp2 sexp where_revised where threads nothreads openin openin_revised
TESTS=marshal enum ref random2 random sexp2 where_revised where threads nothreads openin
BYTE_TARGETS=  $(patsubst %, test_%.byte, $(TESTS))
NATIVE_TARGETS=$(patsubst %, test_%.native, $(TESTS))
COMPILE= $(BYTE_TARGETS) $(NATIVE_TARGETS)

all: library clean tests

byte:
	cd ../batteries/ && make byte uninstall install

opt:
	cd ../batteries/ && make opt uninstall install

library:
	cd ../batteries/ && make byte opt uninstall install

tests: $(COMPILE)

%.byte:
	ocamlbuild $@

%.native:
	ocamlbuild $@

clean:
	ocamlbuild -clean
