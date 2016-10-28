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
- run `make release`
- Commit and add a tag (`git tag -a <name>`; `git push --tags origin`)
  Tag names are usually of the form "vM.m.b", for example "v2.5.3",
  use `git tag --list` to see existing tags.
- upload tarball to ocamlforge
- write release notes (using changelog)
- upload documentation (`make upload-docs` ?)
- update opam package
