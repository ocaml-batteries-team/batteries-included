### Configuration section

CFLAGS=-g

# Comment out if you don't have bzip2
CFLAGS+=-DUSE_BZIP2

# The name of the Zlib library.  Usually -lz
ZLIB_LIB=-lz

# The name of the Bzip2 library. Usually -lbz2. Comment out if you don't have it.
BZIP2_LIB=-lbz2

# The directory containing the Zlib library (libz.a or libz.so)
ZLIB_LIBDIR=/usr/local/lib

# The directory containing the Zlib header file (zlib.h)
ZLIB_INCLUDE=/usr/local/include

# Where to install the library.  By default: sub-directory 'zip' of
# OCaml's standard library directory.
INSTALLDIR=`$(OCAMLC) -where`/zip

### End of configuration section

OCAMLC=ocamlc -g
OCAMLOPT=ocamlopt
OCAMLDEP=ocamldep
OCAMLMKLIB=ocamlmklib

OBJS=zlib.cmo zip.cmo gzip.cmo bzlib.cmo bzip2.cmo tar.cmo
C_OBJS=zlibstubs.o

all: libcamlzip.a zip.cma

allopt: libcamlzip.a zip.cmxa

zip.cma: $(OBJS)
	$(OCAMLMKLIB) -o zip -oc camlzip $(OBJS) \
            -L$(ZLIB_LIBDIR) $(ZLIB_LIB) $(BZIP2_LIB)

zip.cmxa: $(OBJS:.cmo=.cmx)
	$(OCAMLMKLIB) -o zip -oc camlzip $(OBJS:.cmo=.cmx) \
            -L$(ZLIB_LIBDIR) $(ZLIB_LIB) $(BZIP2_LIB)

libcamlzip.a: $(C_OBJS)
	$(OCAMLMKLIB) -oc camlzip $(C_OBJS) \
            -L$(ZLIB_LIBDIR) $(ZLIB_LIB) $(BZIP2_LIB)

.SUFFIXES: .mli .ml .cmo .cmi .cmx

.mli.cmi:
	$(OCAMLC) -c $<
.ml.cmo:
	$(OCAMLC) -c $<
.ml.cmx:
	$(OCAMLOPT) -c $<
.c.o:
	$(OCAMLC) -c -ccopt "$(CFLAGS)" -ccopt -I$(ZLIB_INCLUDE) $<

clean:
	rm -f *.cm*
	rm -f *.o *.a *.so

install:
	mkdir -p $(INSTALLDIR)
	cp zip.cma zip.cmi gzip.cmi zip.mli gzip.mli bzip2.mli bzip2.cmi \
		tar.mli tar.cmi libcamlzip.a $(INSTALLDIR)
	if test -f dllcamlzip.so; then \
	  cp dllcamlzip.so $(INSTALLDIR); \
          ldconf=`$(OCAMLC) -where`/ld.conf; \
          installdir=$(INSTALLDIR); \
          if test `grep -s -c $$installdir'$$' $$ldconf || :` = 0; \
          then echo $$installdir >> $$ldconf; fi \
        fi

installopt:
	cp zip.cmxa zip.a zip.cmx gzip.cmx bzip2.cmx tar.cmx $(INSTALLDIR)

depend:
	gcc -MM -I$(ZLIB_INCLUDE) *.c > .depend
	ocamldep *.mli *.ml >> .depend

include .depend
