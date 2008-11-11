#TESTS=ref random2 random sexp2 sexp where_revised where threads nothreads openin openin_revised
TESTS=version flush marshal enum ref random2 random sexp2 threads nothreads openin
BYTE_TARGETS=  $(patsubst %, test_%.byte, $(TESTS))
NATIVE_TARGETS=$(patsubst %, test_%.native, $(TESTS))
COMPILE= $(BYTE_TARGETS) $(NATIVE_TARGETS)


bytelib:
	cd ../ && make byte uninstall install

optlib:
	cd ../ && make opt uninstall install

library:
	cd ../ && make byte opt uninstall install

tests: $(COMPILE)

%.byte:
	ocamlbuild $@

%.native:
	ocamlbuild $@

clean:
	ocamlbuild -clean
