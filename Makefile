.PHONY: all clean install uninstall reinstall test doc

all:
	dune build @all

clean:
	dune clean

install:
	dune build @install
	dune install

uninstall:
	dune uninstall

test:
	dune runtest

doc:
	dune build @all
	dune build @doc

test_install:
	./scripts/test_install.sh

reinstall:
	dune uninstall
	dune build @install
	dune install


###############################################################################
#	BENCHMARK SUITE
###############################################################################

bench:
	$(OCAMLBUILD) $(OCAMLBUILDFLAGS) $(TARGETS) $(BENCH_TARGETS)
	$(RM) bench.log
	$(foreach BENCH, $(BENCH_TARGETS), _build/$(BENCH) | tee -a bench.log; )
	@echo "Benchmarking results are written to bench.log"


###############################################################################
#	PREPARING RELEASE FILES
###############################################################################

release:
	$(MAKE) clean
	git stash save "stashing local modifications before release"
	$(MAKE) release-cleaned

# assumes irreproachably pristine working directory
release-cleaned: doc test-native
	git archive --format=tar --prefix=batteries-$(VERSION)/ HEAD \
	  | gzip -9 > batteries-$(VERSION).tar.gz

# uploads the current documentation to github hdoc2/ directory
upload-docs:
	make doc && \
	rm -rf /tmp/batteries.docdir && \
	cp -a _build/batteries.docdir /tmp/ && \
	git checkout gh-pages && \
	rm -f hdoc2/*.html && \
	cp /tmp/batteries.docdir/*.html hdoc2/ && \
	git add hdoc2/*.html && \
	git commit hdoc2 -m "Update ocamldoc to latest release" && \
	git push origin gh-pages && \
	git checkout master
