Make a release
--------------

From various emails and interviews with VIP:

- 'make test' on a 64 bits machine
- 'make test' on a 32 bits machine
- inspect commits to find @since tags to add/substitute (especially @since NEXT_RELEASE)
- check whether new functions should go in Incubator
- build changelog (from `git log`, see `howto/changelog`)
- Bump version in source (in `_oasis`)
- change ocamlfind dependencies in `META` if necessary
- Commit and add a tag (`git tag -a`; `git push --tags origin`)
- run `make release`
- upload tarball to ocamlforge
- write release notes (using changelog)
- upload documentation (`make upload-docs` ?)
- update opam package
