AC_INIT

INSTALLATION=

AC_SUBST(INSTALLATION)

# version info
VERSION=0.20090623
AC_SUBST(VERSION)

# check for OS
AC_MSG_CHECKING(operating system)
uname=`uname -s`
case $uname in
  Darwin)
      AC_MSG_RESULT(MacOS X / Darwin)
      DEFAULT_BROWSER="open %s &> /dev/null";;
  CYGWIN*)
      AC_MSG_RESULT(Windows with Cygwin)
      DEFAULT_BROWSER="cygstart %s &> /dev/null";;
  Mingw*)
      AC_MSG_RESULT(Windows with Mingw)
      DEFAULT_BROWSER="start %s &> /dev/null";;
  *) 
      AC_MSG_RESULT($uname)
      DEFAULT_BROWSER="x-www-browser %s &> /dev/null";;
esac


# check for basic OCaml components
AC_CHECK_PROGS(OCAMLC,[ocamlc.opt ocamlc],)
if test -z "$OCAMLC" ; then
   AC_MSG_ERROR([Cannot find ocamlc.])
   exit 1
fi
AC_MSG_CHECKING(ocaml version)
OCAMLVERSION=`$OCAMLC -version`
AC_MSG_NOTICE($OCAMLVERSION)
AC_CHECK_PROGS(OCAMLOPT,[ocamlopt.opt ocamlopt],)
if test -z "$OCAMLOPT" ; then
   AC_MSG_WARN([Cannot find ocamlopt; native code compilation only.])
fi
AC_CHECK_PROGS(OCAMLBUILD,[ocamlbuild ocamlbuild.native ocamlbuild.byte],)
if test -z "$OCAMLBUILD" ; then
   AC_MSG_WARN([Cannot find ocamlbuild.])
fi
AC_CHECK_PROGS(OCAMLDOC,[ocamldoc.opt ocamldoc],)
if test -z "$OCAMLDOC" ; then
   AC_MSG_WARN([Cannot find ocamldoc; API doc cannot be generated.])
fi
AC_SUBST(OCAMLC)
AC_SUBST(OCAMLOPT)
AC_SUBST(OCAMLBUILD)
AC_SUBST(OCAMLDOC)
AC_SUBST(OCAMLVERION)

OCAMLVERSION=$($OCAMLC -version)
OCAMLLIBDIR=$($OCAMLC -where)
AC_SUBST(OCAMLVERSION)
AC_SUBST(OCAMLLIBDIR)

# check for Findlib
AC_CHECK_PROGS(OCAMLFIND,[ocamlfind],)
if test -z "$OCAMLFIND" ; then
   AC_MSG_ERROR([Cannot find Findlib (i.e. ocamlfind).])
   exit 1
fi

OCAMLFIND_DEPS=""

dnl check if a given Findlib package exists, set OCAMLPKG_<PKGNAME>
dnl accordingly
AC_DEFUN(AC_CHECK_OCAML_PKG,
[dnl
AC_MSG_CHECKING(findlib package $1)
if tmp=`$OCAMLFIND query -format "%p" $1` 2>/dev/null ; then
   AC_MSG_RESULT(found)
   [$2]=$tmp
   OCAMLFIND_DEPS="$OCAMLFIND_DEPS $tmp"
else
   AC_MSG_WARN(not found)
fi
])

dnl as above, but without warning
dnl accordingly
AC_DEFUN(AC_CHECK_OCAML_PKG_SILENTLY,
[dnl
AC_MSG_CHECKING(findlib package $1)
if tmp=`$OCAMLFIND query $1` 2>/dev/null ; then
   AC_MSG_RESULT(found)
   [$2]=$tmp
   OCAMLFIND_DEPS="$OCAMLFIND_DEPS $tmp"
fi
])
# check for needed Findlib packages
AC_CHECK_OCAML_PKG(camomile, OCAMLPKG_camomile)
if test -z "$OCAMLPKG_camomile" ; then
   AC_MSG_ERROR([Cannot find Camomile])
   exit 1
fi
AC_SUBST(OCAMLPKG_camomile)

AC_CHECK_OCAML_PKG(zip, OCAMLPKG_zip)
# Camlzip has different Findlib-level names on, e.g., Debian and GODI
if test -z "$OCAMLPKG_zip" ; then
  AC_CHECK_OCAML_PKG(camlzip, OCAMLPKG_zip)
  if test -z "$OCAMLPKG_zip" ; then
     AC_MSG_ERROR([Cannot find Camlzip])
     exit 1
  fi
fi
AC_SUBST(OCAMLPKG_zip)
AC_CHECK_OCAML_PKG(type-conv, OCAMLPKG_type_conv)
if test -z "$OCAMLPKG_type_conv" ; then
   AC_MSG_ERROR([Cannot find type-conv])
   exit 1
fi
AC_CHECK_OCAML_PKG(sexplib, OCAMLPKG_sexplib)
if test -z "$OCAMLPKG_sexplib" ; then
   AC_MSG_ERROR([Cannot find sexplib])
   exit 1
fi
AC_CHECK_OCAML_PKG(bin_prot, OCAMLPKG_bin_prot)
if test -z "$OCAMLPKG_bin_prot" ; then
   AC_MSG_ERROR([Cannot find bin_prot])
   exit 1
fi
AC_CHECK_OCAML_PKG(netstring, OCAMLPKG_netstring)
if test -z "$OCAMLPKG_netstring" ; then
   AC_MSG_ERROR([Cannot find netstring (part of ocamlnet)])
   exit 1
fi
AC_SUBST(OCAMLFIND_DEPS)

# set build-time options

# default browser command
AC_ARG_WITH(
	browser,
	[  --with-browser=BROWSER  set command to open HTML API reference
	                  default: "x-www-browser %s"],
	BROWSER_COMMAND=$withval,
	BROWSER_COMMAND=$DEFAULT_BROWSER)
if test "$BROWSER_COMMAND" = "yes" ; then
   AC_MSG_ERROR([--with-browser requires an argument])
   exit 1
fi
AC_SUBST(BROWSER_COMMAND)


AC_ARG_WITH(
	godi,
	[  --with-godi             require compilation for GODI],
	HAS_GODI=yes,
	HAS_GODI=no)

AC_SUBST(HAS_GODI)
if test "$HAS_GODI" = "yes" ; then
   AC_MSG_NOTICE([GODI-specific installation enabled])
   DEFAULT_DOCDIR=`$OCAMLFIND printconf destdir`/../../../doc/godi-batteries
else
   DEFAULT_DOCDIR="$docdir"
fi
DOCDIR="$DEFAULT_DOCDIR"

#Should documentation be built?
AC_ARG_WITH(
	doc,
	[  --with-doc                build documentation (yes by default)],
        BUILD_DOC=yes,
        BUILD_DOC=no)


if test "$INSTALLATION" = "GODI"; then
  AC_MSG_NOTICE([Installing to GODI default directory])
  DESTDIR="`$OCAMLFIND printconf destdir`"
  AC_CHECK_OCAML_PKG_SILENTLY(batteries, OCAMLPKG_batteries)
  if test -z "$OCAMLPKG_batteries"; then
   	DESTDIR=`$OCAMLFIND printconf destdir`
  else
	AC_MSG_ERROR([Batteries is already installed])
	exit 1
  fi
  BUILD_DOC="yes"
else
   if test "${prefix}" = "NONE"; then
        AC_MSG_NOTICE([Locating default installation directory])
	AC_CHECK_OCAML_PKG_SILENTLY(batteries, OCAMLPKG_batteries)
	if test -z "$OCAMLPKG_batteries"; then
   	   DESTDIR=`$OCAMLFIND printconf destdir`
	else
	   AC_MSG_WARN([Batteries is already installed])
   	   AC_MSG_WARN([Configuration will succeed but you will need to uninstall Batteries before reinstalling it])
   	   DESTDIR=`dirname $OCAMLPKG_batteries`
	fi
   else
	AC_MSG_NOTICE([Using user-provided prefix ${prefix}])
	DESTDIR="${prefix}/lib/batteries"
   fi
fi
if test "$BUILD_DOC" = "yes"; then
   AC_MSG_NOTICE([Building documentation])
else
   AC_MSG_NOTICE([Documentation will not be built])
fi
AC_SUBST(BUILD_DOC)

AC_MSG_NOTICE([Default destination is $DESTDIR])
AC_SUBST(DESTDIR)

AC_ARG_WITH(
	docroot,
	[  --with-docroot=PATH     set documentation root
	                           default: "$DESTDIR/doc/batteries"],
	DOCDIR=$withval,
	DOCDIR=NONE)
if test "$DOCDIR" = "yes" ; then
   AC_MSG_ERROR([--with-docroot requires an argument])
   exit 1
fi
if test "$DOCDIR" = "NONE"; then
   if test "$prefix" = "NONE"; then
      DOCDIR=$DESTDIR/doc/batteries
   else
      DOCDIR=$prefix/doc/batteries
   fi
fi
AC_MSG_NOTICE([documentation root directory: $DOCDIR])
AC_SUBST(DOCDIR)

AC_ARG_WITH(
	runner,
	[  --with-runner     build experimental dynamic linking runner
	   		     deactivated by default],
	BUILD_RUNNER=$withval,
	BUILD_RUNNER=no)
AC_SUBST(BUILD_RUNNER)


# fill output templates
AC_CONFIG_FILES([
 Makefile
 build/META
 build/threaded/META
 build/nothreads/META
 src/batteries_toolchain/ocaml
])
AC_OUTPUT

