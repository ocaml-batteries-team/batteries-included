TESTS=threads nothreads list openin openin_revised
BYTE_TARGETS=  $(patsubst %, test_%.byte, $(TESTS))
NATIVE_TARGETS=$(patsubst %, test_%.native, $(TESTS))
COMPILE= $(BYTE_TARGETS) $(NATIVE_TARGETS)

all: $(COMPILE)

%.byte:
	ocamlbuild $@

%.native:
	ocamlbuild $@

clean:
	ocamlbuild -clean
