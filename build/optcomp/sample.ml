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

(* Variables are defined with the directive define:

   #define <id> <expr>

   where <id> is any lower or upper identifier and <expr> is any
   well-parenthesed expression, followed by a newline.

   For instanse here are some correct variable definition:
*)

#define X 1
#define y 1 + (1
               + 1)
#define z X + y

(* Notes:

   - contrary to cpp, expressions are evalued at definition time, so
   they may only refer to previously defined variables

   - there is no #ifdef, #ifndef directives, but you can give a
   default value to a variable with:

   #default <id> <expr>

   This means that if <id> is not yet defined then it will be defined
   to <expr>.

   For instance, in:
*)

#default toto 2
#define truc true
#default truc false

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

#define ocaml_major_version fst ocaml_version
#define ocaml_minor_version snd ocaml_version

#if (
  (ocaml_major_version = 3
      && ocaml_minor_version >= 11)
  || ocaml_major_version > 3
)
let lazy x = lazy 1
#else
let x = 1
#endif

(* +--------+
   | Errors |
   +--------+ *)

(* You may also use the #error directive to make the parser to raise
   an error: *)

#if ocaml_version < (3, 0)
#error "too old ocaml version, minimum is 3.0"
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

#define x (1, 2, (3, 4))

#define y (let (a, b, (c, d)) = x in
           a + b = c || (max b c = 2 && d = a - 1))

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
