(*
 * sample.ml
 * ---------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of optcomp.
 *)

(* This file show how to use optcomp *)

(* +-----------+
   | Variables |
   +-----------+ *)

(* Variables are defined with the directive let:

   #let <id> = <expr>

   where <id> is any lower or upper identifier and <expr> is any
   well-parenthesed expression, followed by a newline.

   For instanse here are some correct variable definition:
*)

#let X = 1
#let y = 1 + (1
              + 1)
#let z = X + y

(* Notes:

   - contrary to cpp, expressions are evalued at definition time, so
   they may only refer to previously defined variables

   - there is no #ifdef, #ifndef directives, but you can give a
   default value to a variable with:

   #let_default <id> = <expr>

   This means that if <id> is not yet defined then it will be defined
   to <expr>.

   For instance, in:
*)

#let_default toto = 2
#let truc = true
#let_default truc = false

(* [toto] will be bound to [2] but [truc] will be bound to [true]

   By default only the variable ocaml_version is set. It is set to the
   pair of integers (major_version, minor_version).
*)

(* +------------+
   | Conditions |
   +------------+ *)

(* To switch between different different pieces of code, one can use
   the following directives:

   #if <expr>
   #else
   #elif <expr>
   #endif

   where <expr> must evaluate to a boolean value.

   For example:
*)

#if ocaml_version >= (3, 11)
type t = private int
#else
type t
#endif

(* It is also possible to split the expression over multible lines by
   using parentheses: *)

#let ocaml_major_version = fst ocaml_version
#let ocaml_minor_version = snd ocaml_version

#if (
  (ocaml_major_version = 3
      && ocaml_minor_version >= 11)
  || ocaml_major_version > 3
)
let lazy x = lazy 1
#else
let x = 1
#endif

(* +-------------+
   | Expressions |
   +-------------+ *)

(* It is actually not possibles to use any kind of expressions. Here
   is what is allowed:

   - litterals booleans, integers, strings and characters:
   - basic interger operations: +, -, /, *, mod
   - value comparing: =, <>, <, >, <=, >=
   - maximum and minimum: max, min
   - basic boolean operations: or, ||, &&, not
   - pair operations: fst, snd
   - let-bindings

   Example:
*)

#let x = (1, 2, (3, 4))

#let y = (let (a, b, (c, d)) = x in
          a + b = c || (max b c = 2 && d = a - 1))

(* +-------------+
   | Indentation |
   +-------------+ *)

(* Spaces and comments are ignored between the "#" at the beginning of the line
   and the directive name, so directives can be indented like that: *)

#if true

let x = 1

#  if false

let y = 2

# (* plop *) elif 1 + 1 = 2

let i = 2

#  else

let o = 42

#  endif

#endif

(* +---------------------+
   | Errors and warnings |
   +---------------------+ *)

(* You may also use the #error #warning directives to make the parser
   to fail or print a warning: *)

#if ocaml_version < (3, 0)
#  error "too old ocaml version, minimum is 3.0"
#endif

#if ocaml_version < (2048, 0)
#  warning "plop!"
#endif

(* +-------------------------+
   | #include and #directory |
   +-------------------------+ *)

(* To include an another file one can use either #use or #include
   directives.

   The difference is that with the #include option, the file will be
   searched in all directories specified with the "-I" command line
   option or with #directory directives.

   #directory "dir"
   #include "file"

   Notes:

   - argument of #directory and #include can be any expression of type
   string

   - the environment can be accessed and modified by included files

   - #directory directives are interpreted by both optcomp and camlp4
*)

#include "sample_incl.ml"

(* +-----------------------------------+
   | Access to definitions in the code |
   +-----------------------------------+ *)

(* We may want to access to values of the optcomp environment. For
   that we can use the "optcomp" quotation, which will be expansed
   into an expression or pattern: *)

#let totolib_version = (1, 1)

let print_info _ =
  let (major, minor) = <:optcomp< totolib_version >> in
  Printf.printf "sample is compiled with totolib version %d.%d" major minor
