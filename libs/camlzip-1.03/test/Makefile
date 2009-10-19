all: minizip minigzip minibzip2 

CAMLFLAGS=-g -I .. -dllpath ..

minibzip2: ../zip.cma minibzip2.ml
	ocamlc $(CAMLFLAGS) -o minibzip2 ../zip.cma minibzip2.ml

testtar: ../zip.cma testtar.ml
	ocamlc $(CAMLFLAGS) -o minitar ../zip.cma testtar.ml

minigzip: ../zip.cma minigzip.ml
	ocamlc $(CAMLFLAGS) -o minigzip ../zip.cma minigzip.ml

minizip: ../zip.cma minizip.ml
	ocamlc $(CAMLFLAGS) -o minizip unix.cma ../zip.cma minizip.ml

testzlib: ../zip.cma testzlib.ml
	ocamlc $(CAMLFLAGS) -o testzlib ../zip.cma testzlib.ml

clean:
	rm -f *.cm*
	rm -f minigzip minizip testzlib minibzip2 testtar
