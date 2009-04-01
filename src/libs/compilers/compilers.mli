(*
 * Compilers -- Calling the elements of the OCaml toolchain.
 * Copyright (C) 2009 David Rajchenbach-Teller, LIFO, Universite d'Orleans
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version,
 * with the special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

(**
   Invoke the OCamlc compilers, documentation generator...

   This module defines a simple layer for invoking the stand-alone elements of the toolchain
   (ocamlc, ocamlopt, ocamlfind, ocamldoc, ocamlrun, ocaml).

   {b Note} Before calling any of the functions of this module, you need to initialize
   Findlib by calling either {!Findlib.init} or {!Findlib.init_manually}.
*)

type command
  (** Abstraction of a command, i.e. the instructions necessary
      to run one of the tools of the toolchain. *)

val string_of_command: command -> string
  (** Format a command as a [string], fit for use with {!Sys.command} *)

val command_for_exec:  command -> string * string array
  (** Format a command as a [string] and an array of arguments, fit for use
      with {!Unix.exec}.*)

(**
   {6 Options}

   If this is the first time you attempt to invoke theh tools, you should probably disregard
   these options for now.
*)

(** Package options, understood by ocamlfind.

    These options serve essentially for automatically resolving dependencies between packages.
*)
type package_option =
    [ `package  of string         (**[`package name]: this compilation requires package [name].     *)
    | `linkpkg                    (**[`linkpkg]: link packages in.                                  *)
    | `predicates  of string list (**[`predicates p]: add predicate [p] for the resolution of package properties.*)
(*    | `dontlink                   (**[`dontlink]:Don't link after compilation.                      *)*)
    | `dontlink    of string      (**[`dontlink name]: don't link package [name] nor its ancestors. *)
    | `syntax      of string      (**[`syntax p]: use a preprocessor identified by predicate [p].   *)
    | `ppopt       of string list (**[`ppopt s]: pass options [s] to the preprocessor.              *)
    | `dllpath_pkg of string      (**[`dllpath_pkg name]: add dll path for package [name].          *)
    | `dllpath_all                (**[`dllpath_all]: add dll path for all packages.                 *)
    | `ignore_error               (**[`ignore_error]: ignore the error directive defined by the package.*)
    | `passopt     of string list (**[`passopt s]: pass options [s] directly to the compiler.       *)
    | `verbose                    (**[`verbose]: display the complete command line.                 *)
    ]

type warning =
    [`comments             of bool (**[true] to warn on suspicions comments, [false] otherwise.*)
    |`deprecated           of bool (**[true] to warn on deprecated features, [false] otherwise.*)
    |`fragile_match        of bool (**[true] to warn on fragile matches, [false] otherwise.*)
    |`partial_applications of bool (**[true] to warn on partially applied functions, [false] otherwise.    *)
    |`omitted_labels       of bool (**[true] to warn on labels omitted in application, [false] otherwise.  *)
    |`overridden_methods   of bool (**[true] to warn on methods overridden several times, [false] otherwise.*)
    |`partial_match        of bool (**[true] to warn on incomplete matches, [false] otherwise.*)
    |`non_unit             of bool (**[true] to warn on non-unit statements, [false] otherwise.*)
    |`unused_match         of bool (**[true] to warn on unused match cases, [false] otherwise.*)
    |`overridden_field     of bool (**[true] to warn on overridden instance variables, [false] otherwise.*)
    |`unused_var           of bool (**[true] to warn on suspicious unused variables, [false] otherwise.*)
    |`other_unused_var     of bool (**[true] to warn on all unused variables, [false] otherwise.*)
    |`misc                 of bool (**[true] for miscellaneous warnings, [false] otherwise.*)
]

(** Compiler options, understood directly by ocamlc or ocamlopt. *)
type compiler_option =
    [ `library                    (**[`library]: Compile as a library (.a).                         *)
    | `c                          (**[`c]: As [`dontlink].                                          *)
    | `cc of string               (**[`cc command]: Use [command] as the C compiler and linker.     *)
    | `cclib of string list       (**[`cclib opts]: Pass options [opts] to the C linker.            *)
    | `ccopt of string list       (**[`ccopt opts]: Pass options [opts] to the C compiler and linker.*)
    | `config                     (**[`confing]: Don't compile, just print configuration.           *)
    | `custom                     (**[`custom]: Link in custom mode.                                *)
    | `dllib of string            (**[`dllib lib]: Use dynamic library [lib].                       *)
    | `dllpath of string          (**[`dllpath path]: Add [path] to the run-time search path for shared libraries. *)
    | `dtypes                     (**[`dtypes]: Save type information in [filename^".annot"]        *)
    | `for_pack of string         (**[`for_pack ident]: Generate code that can later be packed into
				     a module [String.uppercase ident]. Ignored in ocamlc.          *)
    | `g                          (**[`g]: Save debugging information. *)
    | `debug                      (**[`debug]: as [`g].                *)
    | `i                          (**[`i]: Don't generate code, print inferred interface.           *)
    | `I of string list           (**[`I dir]: Add [dir] to the list of include directories.        *)
    | `include_dir of string list (**[`include_dir dir]: as [`I dir]                                *)
    | `impl of string             (**[`impl file]: Compile [file] as a .ml file, regardless of its actual extension. *)
    | `intf of string             (**[`intf file]: Compile [file] as a .mli file, regardless of its actual extension. *)
    | `intf_suffix of string      (**[`intf_suffix]: Change default suffix for interface files (default: .mli).    *)
    | `labels                     (**[`labels]: allow labels to commute (default)*)
    | `linkall                    (**[`linkall]: Link all modules, even unused ones.                    *)
    | `make_runtime               (**[`make_runtime]: Build a runtime system with given C objects and libraries.   *)
    | `noassert                   (**[`noassert]: Don't compile assertion checks.                       *)
    | `noautolink                 (**[`noautolink]:  Don't automatically link C libraries specified in .cma files. *)
    | `nolabels                   (**[`nolabels]: Ignore non-optional labels in types.                  *)
    | `nostdlib                   (**[`nostdlib]: Do not add default directory to the list of include directories.*)
    | `output of string           (**[`output file]:  Set output file name to [file].                   *)
    | `o of string                (**[`o file]: as [`output file].                                      *)
    | `output_obj                 (**[`output_obj]: Output a C object file instead of an executable.    *)
    | `pack                       (**[`pack]:  Package the given .cmo files into one .cmo.              *)
    | `pp of string               (**[`pp command]:  Pipe sources through preprocessor [command]. If you can, prefer
				     using {!`syntax}. *)
    | `principal                  (**[`principal]: Check principality of type inference.                *)
    | `rectypes                   (**[`rectypes]: Allow arbitrary recursive types.                      *)
    | `thread                     (**[`thread]: Generate code that supports the system threads library. *)
    | `unsafe                     (**[`unsafe]: No bounds checking on array and string access.          *)
    | `use_runtime of string      (**[`use_runtime file]: Generate bytecode for the given runtime system.*)
    | `v                          (**[`v]: Don't compile, print compiler version and location of standard library and exit.*)
    | `version                    (**[`version]: Don't compile, print compiler version and exit.        *)
    | `verbose                    (**[`verbose]: Print calls to external commands.                      *)
    | `vmthread                   (**[`vmthread]: Generate code that supports the threads library with VM-level
     scheduling.*)
    | `warnings of warning list   (**[`warning flags]: Activate/deactivate warnings [flags].            *)
    | `warn_error of warning list(**[`warn_errors flags]: Treat the warnings of [flags] as errors.     *)
    | `where                      (**[`where]: Print location of standard library and exit.             *)
    | `nopervasives
    | `dparsetree
    | `drawlambda
    | `dlambda
    | `dinstr
    | `use_prims of string
    | `file      of string        (**[`file file]: Compile or link [file] -- [file] must be a name ending in
				     .ml, .mli, .cmo or .cma. The order in which files are given is 
				     important, as it will determine linking. *) ]

type interpreter_option =
  [ `init of string             (**[`init file]: Load [file] instead of the default .ocamlinit file*)
  | `I of string list           (**[`I dir]: Add [dir] to the list of include directories.        *)
  | `labels                     (**[`labels]: allow labels to commute (default)*)
  | `noassert                   (**[`noassert]: Don't compile assertion checks.                       *)
  | `nolabels                   (**[`nolabels]: Ignore non-optional labels in types.                  *)
  | `noprompt                   (**[`noprompt]: Suppress all prompts.                                 *)
  | `nostdlib                   (**[`nostdlib]: Do not add default directory to the list of include directories.*)
  | `principal                  (**[`principal]: Check principality of type inference.                *)
  | `rectypes                   (**[`rectypes]: Allow arbitrary recursive types.                      *)
  | `unsafe                     (**[`unsafe]: No bounds checking on array and string access.          *)
  | `version                    (**[`version]: Don't compile, print compiler version and exit.        *)
  | `warnings of warning list   (**[`warning flags]: Activate/deactivate warnings [flags].            *)
  | `warn_error of warning list(**[`warn_errors flags]: Treat the warnings of [flags] as errors.     *)
  | `dparsetree
  | `drawlambda
  | `dlambda
  | `dinstr
  | `file      of string        (**[`file file]: Compile or link [file] -- [file] must be a name ending in
				   .ml, .mli, .cmo or .cma. The order in which files are given is 
				   important, as it will determine linking.
				   Only one [.ml] file may be used.*) ]

val all_warnings : warning list 



(**
   {6 Compilers}
*)

val ocamlc: 
  ?batteries: bool ->
   ?package:package_option list -> 
  ?options:compiler_option list ->
  string list -> command
(**
   Invoke the ocamlc compiler, to produce bytecode modules and executables.

   [ocamlc l] returns a command which may be used
   to run the ocamlc compiler to compile files [l].

   @param batteries Use the standard library ([true] by default).
   @param package An optional list of package options.
   @param options An optional list of compiler options.

   Note that it is possible to specify that a file [some_file] must be
   compiled either by making sure that [some_file] appears in [l] or
   by making sure that [`file some_file] appears in [options]. Either
   manners of adding a file are equivalent, with files mentioned in [options]
   being compiled before files mentioned in [l].

   For more informations about the ocamlc compiler, see
   {{:http://caml.inria.fr/pub/docs/manual-ocaml/manual022.html} the manual of OCaml}.
*)


val ocamlopt: 
  ?batteries:bool               ->
  ?package:package_option list  -> 
  ?options:compiler_option list ->
  string list -> command
(**
   Invoke the ocamlopt compiler, to produce native optimized modules and executables.
   
   [ocamlopt l] returns a command which may be used
   to run the ocamlopt compiler to compile files [l].

   @param batteries Use the standard library ([true] by default).
   @param package An optional list of package options.
   @param options An optional list of compiler options.
   
   Note that it is possible to specify that a file [some_file] must be
   compiled either by making sure that [some_file] appears in [l] or
   by making sure that [`file some_file] appears in [options]. Either
   manners of adding a file are equivalent, with files mentioned in [options]
   being compiled before files mentioned in [l].
   
   For more informations about the ocamlopt compiler, 
   {{:http://caml.inria.fr/pub/docs/manual-ocaml/manual025.html} the manual of OCaml}.
*)

val ocaml: 
  ?batteries:bool ->
  ?options:interpreter_option list ->
  string list -> command
(**
   Invoke the ocaml interpreter, to execute files without a compilation phase.
   
   [ocaml l] returns a command which may be used
   to run the ocaml interpreter to execute files [l].

   @param batteries Use the standard library ([true] by default).
   @param package An optional list of package options.
   @param options An optional list of compiler options.
   
   For more informations about the ocaml interpreter,
   {{:http://caml.inria.fr/pub/docs/manual-ocaml/manual023.html} the manual of OCaml}.
*)


(*
  val ocamlcp:
  val ocamlmktop:
  val ocamldep: ?package:package_option list -> ?options:dep_option list -> string list -> command
  val ocamldoc:
*)
