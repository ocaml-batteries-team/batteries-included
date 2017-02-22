Make a release
--------------

# Quality checking

- `make test` on a 64 bits machine
- `make test` on a 32 bits machine

- install the to-be-released version with `opam pin add -k git .`, and
  then run the post-install tests with `make test-build-from-install`

  (If you do not want to provoke a rebuild of batteries-depending
  software in your main development switch, feel free to move to
  a fresh new switch to test this.)

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

## opam preparation work

Performing the release will require sending a pull-request against the
public opam repository with an `opam` metadata file for the new
version. Here is how you should prepare this `opam` file.

There are two sources of inspiration for the new opam file:

- there is a local `opam` file at the root of the ocamlbuild
  repository, that is used for pinning the development version.

- there are the `opam` files for previous OCamlbuild releases in the
  public opam repository:
  https://github.com/ocaml/opam-repository/tree/master/packages/batteries

In theory the two should be in synch: the `opam` file we send to the
public opam repository is derived from the local `opam` file. However,
upstream opam repository curators may have made changes to the public
opam files, to reflect new packaging best practices and policies. You
should check for any change to the latest version's `opam` file; if
there is any, it should probably be reproduced into our local `opam`
file, and commited.

Note that the local file may have changed during the release lifetime
to reflect new dependencies or changes in packaging policies. These
changes should also be preserved in the opam file for the new version.

To summarize, you should first update the local `opam` file to contain
interesting changes from the in-repository versions. You can then
prepare an `opam` file for the new version, derived from the local
`opam` file.

When editing an opam file (locally or in the package repository), you
should use use `opam lint` to check that the opam file follows best
practices.

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

- once the new opam package is merged, announce on the mailing-list.
