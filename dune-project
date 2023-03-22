(lang dune 2.7)

(name batteries)

(generate_opam_files true)
(allow_approximate_merlin) ; dune requires this due to the use of prefilter.exe

(source (github ocaml-batteries-team/batteries-included))
(documentation "http://ocaml-batteries-team.github.io/batteries-included/hdoc2/")
(authors "OCaml batteries-included team")
(maintainers
 "Cedric Cellier <rixed@happyleptic.org>"
 "Francois Berenger <unixjunkie@sdf.org>"
 "Gabriel Scherer <gabriel.scherer@gmail.com>"
 "Thibault Suzanne <thi.suzanne@gmail.com>"
 "Simmo Saan <simmo.saan@gmail.com>")
(license "LGPL-2.1-or-later with OCaml-LGPL-linking-exception")

(package
 (name batteries)
 (synopsis "A community-maintained standard library extension")
 (depends
  (ocaml (>= 4.02.3))
  camlp-streams
  (ocamlfind (>= 1.5.3))
  (qtest (and :with-test (>= 2.5)))
  (qcheck (and :with-test (>= 0.9) (< 0.14)))
  (benchmark (and :with-test (>= 1.6)))
  num)
 (conflicts
  base-effects
  ocaml-option-no-flat-float-array))
