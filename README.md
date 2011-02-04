Welcome to Batteries Included
================================
OCaml Batteries Included: the community-maintained foundation library
for your OCaml projects.


Batteries Included serves the following purposes:

* define a standard set of libraries which may be
  expected on every compliant installation of OCaml
* organize these libraries into a hierarchy of modules,
  with one source of documentation
* provide a consistent API for otherwise independent
  libraries.

Building Batteries Included
==============================

Requirements
------------------------------
You will need the following libraries:

* OCaml >= 3.11
* [Findlib](http://projects.camlcity.org/projects/findlib.html/) >= 1.2.5
* GNU make (optional; aids build and installation)
* [Camomile](http://camomile.sourceforge.net/) >= 0.7
* [OUnit](http://ounit.forge.ocamlcore.org/)    to build and run the tests

Configuration and installation
------------------------------

To install the full version of OCaml Batteries Included, execute

    $ make all
    $ make test qtest    [ optional ]
    $ sudo make install

    $ make doc      [ optional ]
    $ sudo make install-doc   [ optional ]

If you want the documentation installed elsewhere, set this *before*
starting the build process (it becomes part of `batteries_config.ml`).

    $ export DOCROOT=/path/to/new/docroot/

If you want findlib to use a `-destdir` argument to [ocamlfind install], set

    $ export DESTDIR=/path/to/findlib/dest

To disable native compilation:

    $ export BATTERIES_NATIVE=false

To disable building of native shared libraries:

    $ export BATTERIES_NATIVE_SHLIB=false

Using Batteries
==============================

To get started using Batteries at the toplevel, copy the `ocamlinit`
file to `~/.ocamlinit`:

    $ cp ocamlinit ~/.ocamlinit

If you already have findlib in your `~/.ocamlinit`, you only need the
last line in our ocamlinit to load batteries.

More usage help available on the [batteries-included wiki](http://wiki.github.com/ocaml-batteries-team/batteries-included/)

ExtLib Compatibility
==============================

If your project currently uses ExtLib, most likely you can just change
`-package extlib` to `-package batteries` and add `open Extlibcompat`
to the top of any extlib-using modules.  Batteries' modules are all
named BatFoo to differentiate them from extlib's modules, so one can
use Batteries and ExtLib in the same project.

COMPATIBILITY NOTE: If you're using ExtLib's Unzip module, it does not
have a corresponding module in batteries at the moment.

Extending Batteries Included
==============================

See doc/batteries/GUIDELINES and the [guidelines wiki page](https://github.com/ocaml-batteries-team/batteries-included/wiki/Developers-guidelines)
