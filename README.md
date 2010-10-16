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


Building Batteries
------------------

### Requirements

You will need the following libraries:

* [OCaml][] >= 3.11
* [Findlib][] >= 1.2.5
* [Camomile][] >= 0.7
* GNU make
* [OUnit][] to build and run the tests
* [ocaml-benchmark][] to build and run the performance tests (optional)

[Findlib]: http://projects.camlcity.org/projects/findlib.html/
[OCaml]: http://caml.inria.fr/ocaml/release.en.html
[Camomile]: http://camomile.sourceforge.net/
[OUnit]: http://ounit.forge.ocamlcore.org/
[ocaml-benchmark]: http://ocaml-benchmark.forge.ocamlcore.org/

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

If you want findlib to use a `-destdir` argument to `ocamlfind install`, set

    $ export DESTDIR=/path/to/findlib/dest

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
