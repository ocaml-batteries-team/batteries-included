# OASIS_START
# DO NOT EDIT (digest: 7b2408909643717852b95f994b273fee)

SETUP = ocaml setup.ml

build: setup.data
	$(SETUP) -build $(BUILDFLAGS)

doc: setup.data build
	$(SETUP) -doc $(DOCFLAGS)

test: setup.data build
	$(SETUP) -test $(TESTFLAGS)

all:
	$(SETUP) -all $(ALLFLAGS)

install: setup.data
	$(SETUP) -install $(INSTALLFLAGS)

uninstall: setup.data
	$(SETUP) -uninstall $(UNINSTALLFLAGS)

reinstall: setup.data
	$(SETUP) -reinstall $(REINSTALLFLAGS)

clean:
	$(SETUP) -clean $(CLEANFLAGS)

distclean:
	$(SETUP) -distclean $(DISTCLEANFLAGS)

setup.data:
	$(SETUP) -configure $(CONFIGUREFLAGS)

.PHONY: build doc test all install uninstall reinstall clean distclean configure

# OASIS_STOP

###############################################################################
#	Pre-Processing of Source Code
###############################################################################

PREPROCESSED_FILES = src/batMarshal.mli src/io/batUnix.mli src/all/batPervasives.mli \
		     src/batInnerPervasives.ml src/batHashtbl.ml \
		     src/batPrintexc.mli src/batSys.mli src/batBigarray.mli

prefilter: $(PREPROCESSED_FILES)

# Ocaml 4.00 can benefit strongly from some pre-processing to expose
# slightly different interfaces
.SUFFIXES: .mli .mliv .ml .mlv

# Look for lines starting with ##Vx##, and delete just the tag or the
# whole line depending whether the x matches the ocaml major version
.mliv.mli:
	ocaml str.cma build/prefilter.ml < $^ > $@

.mlv.ml:
	ocaml str.cma build/prefilter.ml < $^ > $@

.PHONY: prefilter
