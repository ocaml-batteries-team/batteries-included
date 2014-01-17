Make a release
--------------

From various emails and interviews with VIP:

- inspect commits to find @since tags to add/substitute
- check whether new functions should go in Incubator
- build changelog (from `git log`, see `howto/changelog`)
- Bump version in source (in `_oasis`)
- Commit and add a tag (`git tag`)
- run `make release`
- upload tarball to ocamlforge
- write release notes (using changelog)
- upload documentation (`make upload-docs` ?)
- update opam package
