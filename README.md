Welcome to OCaml Batteries Included
===================================

***OCaml Batteries Included***, or just ***Batteries***, is a
community-maintained foundation library for your OCaml projects.
Batteries

* defines a standard set of libraries which may be expected on every
  compliant installation of OCaml;

* organizes these libraries into a hierarchy of modules, with a single
  source of documentation; and

* provides a consistent API for otherwise independent libraries.

[![Build Status](https://travis-ci.org/ocaml-batteries-team/batteries-included.png?branch=master)](https://travis-ci.org/ocaml-batteries-team/batteries-included)

Building Batteries
------------------

### Requirements

You will need the following libraries:

* [OCaml][] >= 3.12.1
* [Findlib][] >= 1.2.5
* GNU make
* [OUnit][] to build and run the tests (optional)
* [ocaml-benchmark][] to build and run the performance tests (optional)
* [bisect][] to compute the coverage of the test suite (optional)

[Findlib]: http://projects.camlcity.org/projects/findlib.html/
[OCaml]: http://caml.inria.fr/ocaml/release.en.html
[Camomile]: http://camomile.sourceforge.net/
[OUnit]: http://ounit.forge.ocamlcore.org/
[ocaml-benchmark]: http://ocaml-benchmark.forge.ocamlcore.org/
[bisect]: http://bisect.x9c.fr/

### Configuration and Installation

To install the full version of Batteries, execute

    $ make all
    $ make test test          [ optional ]
    $ sudo make install

    $ make doc                [ optional ]
    $ sudo make install-doc   [ optional ]

If you want the documentation installed elsewhere, set this *before*
starting the build process because this location is stored in the
`Batteries_config` module generated during compilation.

    $ export DOCROOT=/path/to/new/docroot/

To disable native compilation:

    $ export BATTERIES_NATIVE=false

To disable building of native shared libraries:

    $ export BATTERIES_NATIVE_SHLIB=false


Using Batteries
---------------

To get started using Batteries at the toplevel, copy the `ocamlinit`
file to `~/.ocamlinit`:

    $ cp ocamlinit ~/.ocamlinit

If you already have findlib in your `~/.ocamlinit`, you only need the
last line in our ocamlinit to load batteries.

More usage help available on the [batteries-included wiki][batwiki].

[batwiki]: https://github.com/ocaml-batteries-team/batteries-included/wiki/

ExtLib Compatibility
--------------------

If your project currently uses [ExtLib][], most likely you can just change
`-package extlib` to `-package batteries` and add `open Extlibcompat`
to the top of any extlib-using modules.  Batteries' modules are all
named BatFoo to differentiate them from extlib's modules, so one can
use Batteries and ExtLib in the same project.

  [ExtLib]: http://code.google.com/p/ocaml-extlib/

COMPATIBILITY NOTE: If you're using ExtLib's Unzip module, it does not
have a corresponding module in batteries at the moment.


Extending Batteries
-------------------

See doc/batteries/GUIDELINES and the [guidelines wiki page][batwiki-dev].

[batwiki-dev]: https://github.com/ocaml-batteries-team/batteries-included/wiki/Developers-guidelines

If you use emacs, the file `batteries_dev.el` has extra highlighting to support writing quicktests.

Changelog
---------

## v2.2.0

- cartesian product in batSet
- Enum.concat_map alias
- UChar.is_ascii
- equality and enumeration (from, to UChar enum) in batText
- String.find_all function
- Seq.iteri, mapi, iter2, map2 (see issue #417)
- cartesian product of enums (issue #442)
- List.subset
- Array.bsearch dichotomic search (issue #433)
- Enum.print_at_most (issue #425)
- BatOption.ord instance, (issue #393)
- Fix infinite loop in BitSet
- Levenshtein distance on strings
- Seq.{of_list, equal}
- basic .merlin file for merlin users
- BatDeque.eq function to compare Deques by content
- BatteriesExceptionless
- More explicit overridding of ocamlbuild rules, use batteries.mllib
- Add Kahan summation (numerically-accurate sum of floats) to List,Array,Enum
- Add BatOption.some
- (text) improve element indexing in BatList's mli documentation
- Add BatList.filteri_map
- Compatibility with ocaml 4.01
- Add BatList.filteri
- Add Set.split_lt and split_le
- Add split_opt wherever there is split
- Add List.range
- Add the new O_CLOEXEC flag to Unix.open_flag in version 4.01
- Fix BatMutex.DebugMutex.id is always 0.
- Simplify List.partition code
- Add List.ntake and List.takedrop
- Added List.Acc.create and use it
- Add a LazyList.eager_fold_right alias to LazyList.fold_right, with sane argument order
- and many tests and documentation
