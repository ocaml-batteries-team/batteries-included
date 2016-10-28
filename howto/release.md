Make a release
--------------

# Quality checking

- `make test` on a 64 bits machine
- `make test` on a 32 bits machine

# Release marking

These steps can be redone as many times as necessary, and do not need
to be performed by someone with commit rights.

- inspect commits and sources to find @since tags to add/substitute
  (especially @since NEXT_RELEASE); `sh scripts/find_since.sh` can
  help

- check whether new functions should go in Incubator

- update the changelog with release notes

  The changelog should contain an entry for each notable change that
  went in the release, with proper crediting of contributors. You may
  want to use `git log` to check that nothing was forgotten.

  You should write a summary of the release in a few sentences, which
  will serve as release notes, and include it at the top of the
  Changelog for the new release.

- Bump version in source (in `_oasis`)

- change ocamlfind dependencies in `META` if necessary

- check that `make release` correctly produces a release tarball

# Performing the actual release

- Commit and add a tag (`git tag -a <name>`; `git push --tags origin`)
  Tag names are usually of the form "vM.m.b", for example "v2.5.3",
  use `git tag --list` to see existing tags.
- run `make release` to produce a tarball
- upload the tarball to ocamlforge
- upload the documentation (`make upload-docs` ?)
- send a pull-request against the public opam repository

# Post-release work

- create a Changelog section for NEXT_RELEASE
- once the new opam package is merged, announce on the mailing-list
