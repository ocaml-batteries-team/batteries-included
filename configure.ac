AC_INIT

# version info
VERSION=0.20081023
AC_SUBST(VERSION)

# check for basic OCaml components
AC_CHECK_PROGS(OCAMLC,[ocamlc.opt ocamlc],)
if test -z "$OCAMLC" ; then
   AC_MSG_ERROR([Cannot find ocamlc.])
   exit 1
fi
AC_CHECK_PROGS(OCAMLOPT,[ocamlopt.opt ocamlopt],)
if test -z "$OCAMLOPT" ; then
   AC_MSG_WARN([Cannot find ocamlopt; native code compilation only.])
fi
AC_CHECK_PROGS(OCAMLDOC,[ocamldoc.opt ocamldoc],)
if test -z "$OCAMLDOC" ; then
   AC_MSG_WARN([Cannot find ocamldoc; API doc cannot be generated.])
fi
AC_SUBST(OCAMLC)
AC_SUBST(OCAMLOPT)
AC_SUBST(OCAMLDOC)

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
   AC_MSG_RESULT(found (version $tmp))
   [$2]=$tmp
   OCAMLFIND_DEPS="$OCAMLFIND_DEPS $tmp"
else
   AC_MSG_WARN(not found)
fi
])

# check for needed Findlib packages
AC_CHECK_OCAML_PKG(camomile, OCAMLPKG_camomile)
if test -z "$OCAMLPKG_camomile" ; then
   AC_MSG_ERROR([Cannot find Camomile])
   exit 1
fi
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
AC_SUBST(OCAMLFIND_DEPS)

# fill output templates
AC_CONFIG_FILES([
 config.ml
 Makefile
 build/META
 build/threaded/META
 build/nothreads/META
])
AC_OUTPUT

