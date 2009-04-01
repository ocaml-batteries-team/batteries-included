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

open Extlib
open Extlib.ExtArray
open Extlib.ExtString
open Extlib.ExtList
open Extlib.Enum
open Extlib.IO


let all_warnings = [`comments true; `deprecated true; `fragile_match true; `partial_applications true;
		    `omitted_labels true; `overridden_methods true; `partial_match true; `non_unit true;
		    `unused_match true; `overridden_field true; `unused_var true; `other_unused_var true;
		    `misc true]

let is_initialized = ref false

let make_list opt l = Extlib.ExtList.List.interleave ~first:opt opt l 

type package_option =
    [ `package  of string         (**[`package name]: this compilation requires package [name].     *)
    | `linkpkg                    (**[`linkpkg]: link packages in.                                  *)
    | `predicates  of string list (**[`predicates p]: add predicate [p] for the resolution of package properties.*)
    | `dontlink    of string      (**[`dontlink name]: don't link package [name] nor its ancestors. *)
    | `syntax      of string      (**[`syntax p]: use a preprocessor identified by predicate [p].   *)
    | `ppopt       of string list (**[`ppopt s]: pass options [s] to the preprocessor.              *)
    | `dllpath_pkg of string      (**[`dllpath_pkg name]: add dll path for package [name].          *)
    | `dllpath_all                (**[`dllpath_all]: add dll path for all packages.                 *)
    | `ignore_error               (**[`ignore_error]: ignore the error directive defined by the package.*)
    | `passopt     of string list (**[`passopt s]: pass options [s] directly to the compiler.       *)
    | `verbose
    ]

let build_package_option : package_option -> string list = function
  | `package s    -> ["-package"; s]
  | `linkpkg      -> ["-linkpkg"]
  | `predicates l -> make_list "-predicates" l
  | `dontlink s   -> ["-dontlink"; s]
  | `syntax s     -> ["-syntax"; s]
  | `ppopt l      -> make_list "-ppopt" l
  | `dllpath_pkg s-> ["-dllpath-pkg"; s]
  | `dllpath_all  -> ["-dllpath-all"]
  | `ignore_error -> ["-ignore-error"]
  | `passopt l    -> make_list "-passopt" l
  | `verbose      -> ["-verbose"]
  

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
    |`misc                 of bool]

let string_of_warnings (warnings : warning list) : string =
  let buf = Buffer.create 16 in
    List.iter (fun x -> Buffer.add_char buf (match x with
       |`comments             true -> 'C'
       |`comments             false-> 'c'
       |`deprecated           true -> 'D'
       |`deprecated           false-> 'd'
       |`fragile_match        true -> 'E'
       |`fragile_match        false-> 'e'
       |`partial_applications true -> 'F'
       |`partial_applications false-> 'f'
       |`omitted_labels       true -> 'L'
       |`omitted_labels       false-> 'l'
       |`overridden_methods   true -> 'M'
       |`overridden_methods   false-> 'M'
       |`partial_match        true -> 'P'
       |`partial_match        false-> 'p'
       |`non_unit             true -> 'S'
       |`non_unit             false-> 's'
       |`unused_match         true -> 'U'
       |`unused_match         false-> 'u'
       |`overridden_field     true -> 'V'
       |`overridden_field     false-> 'v'
       |`unused_var           true -> 'Y'
       |`unused_var           false-> 'y'
       |`other_unused_var     true -> 'Z'
       |`other_unused_var     false-> 'z'
       |`misc                 true -> 'X'
       |`misc                 false-> 'x')) warnings;
    Buffer.contents buf

      
(** Compiler options, understood directly by ocamlc or ocamlopt. *)
type compiler_option =
    [ `library                    (**[`library]: Compile as a library (.a).                         *)
(*    | `dontlink                   (**[`dontlink]:Don't link after compilation.                      *)*)
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
				   important, as it will determine linking. *) ]

let build_compiler_option compiler = function
    | `library       -> ["-library"]
    | `dontlink | `c -> ["-c"]
    | `cc s          -> ["-cc"; s]
    | `cclib l       -> make_list "-cclib" l
    | `ccopt l       -> make_list "-ccopt" l
    | `compact when compiler = `ocamlc -> ["-compact"]
    | `compact       -> []
    | `config        -> ["-config"]
    | `custom        -> ["-custom"]
    | `dllib s       -> ["-dllib"; s]
    | `dllpath s     -> ["-dllpath"; s]
    | `dtypes        -> ["-dtypes"]
    | `ffastmath when compiler = `ocamlc   -> []
    | `ffastmath     -> ["-ffast-math"]
    | `for_pack s    -> ["-for-pack"; s]
    | `g | `debug    -> ["-g"]
    | `i             -> ["-i"]
    | `inline _ when compiler = `ocamlc    -> []
    | `inline x      -> ["-inline"; string_of_int x]
    | `I l | `include_dir l -> make_list "-I" l
    | `impl l        -> ["-impl"; l]
    | `intf l        -> ["-intf"; l]
    | `intf_suffix s -> ["-intf-suffix"; s]
    | `labels        -> ["-labels"]
    | `linkall       -> ["-linkall"]
    | `make_runtime  -> ["-make_runtime"]
    | `noassert      -> ["-noassert"]
    | `noautolink    -> ["-noautolink"]
    | `nolabels      -> ["-nolabels"]
    | `nostdlib      -> ["-nostdlib"]
    | `output s |`o s-> ["-o"; s]
    | `output_obj    -> ["-output-obj"]
    | `pack          -> ["-pack"]
    | `p | `profile when compiler = `ocamlc -> []
    | `p | `profile  -> ["-p"]
    | `pp s          -> ["-s"]
    | `principal     -> ["-principal"]
    | `rectypes      -> ["-rectypes"]
    | `S | `keep_asm when compiler = `ocamlc -> []
    | `S | `keep_asm -> ["-S"]
    | `thread        -> ["-thread"]
    | `unsafe        -> ["-unsafe"]
    | `use_runtime s -> ["-use-runtime"; s]
    | `v             -> ["-v"]
    | `version       -> ["-version"]
    | `verbose       -> ["-verbose"]
    | `vmthread      -> ["-vmthread"]
    | `warnings l    -> ["-w"; string_of_warnings l]
    | `warn_error l  -> ["-warn-error"; string_of_warnings l]
    | `where         -> ["-where"]
    | `nopervasives  -> ["-nopervasives"]
    | `dparsetree    -> ["-dparsetree"]
    | `drawlambda    -> ["-drawlambda"]
    | `dlambda       -> ["-dlambda"] 
    | `dinstr        -> ["-dinstr"]
    | `dcmm  when compiler = `ocamlc -> []
    | `dsel  when compiler = `ocamlc -> []
    | `dlive when compiler = `ocamlc -> []
    | `dspill when compiler = `ocamlc-> []
    | `dsplit when compiler = `ocamlc-> []
    | `dinterf when compiler = `ocamlc->[]
    | `dprefer when compiler = `ocamlc->[]
    | `dalloc  when compiler = `ocamlc->[]
    | `dreload when compiler = `ocamlc->[]
    | `dscheduling when compiler=`ocamlc->[]
    | `dlinear when compiler = `ocamlc->[]
    | `dstartup when compiler= `ocamlc->[]
    | `dcmm                           -> ["-dcmm"]
    | `dsel                           -> ["-dsel"]
    | `dlive                          -> ["-dlive"]
    | `dspill                         -> ["-dspill"]
    | `dsplit                         -> ["-dsplit"]
    | `dinterf                        -> ["-dinterf"]
    | `dprefer                        -> ["-dprefer"]
    | `dalloc                         -> ["-dalloc"]
    | `dreload                        -> ["-dreload"]
    | `dscheduling                    -> ["-dscheduling"]
    | `dlinear                        -> ["-dlinear"]
    | `dstartup                       -> ["-dstartup"]
    | `use_prims file                 -> ["-use-prims"; file]
    | `file file -> 
	begin
	  match compiler with `ocaml -> [file]
	    |                  _     -> ["- "; file]
	end
    | `init s                         -> ["-init"; s]
    | `noprompt                       -> ["-noprompt"]



type command = string * string list
type task    = int

let prepare_compiler_args compiler package options files =
  List.of_enum (Enum.concat (Enum.concat (List.enum 
				 [Enum.map (fun x -> List.enum (build_package_option x)) (List.enum package);
				  Enum.map (fun x -> List.enum (build_compiler_option compiler x)) (List.enum options);
				  match compiler with
				    | `ocaml -> Enum.map List.enum (List.enum [files])
				    | _ ->  Enum.map (fun file ->  List.enum ["- "; file]) (List.enum files)])))

    


let command_for_exec  (cmd, args) = (cmd, Array.of_list args)
let string_of_command (cmd, args) = 
  let buf = Buffer.create 16 in
    Printf.bprintf buf "%S %a" cmd (fun buf -> List.print IO.nwrite ~first:"" ~last:"" ~sep:" " (IO.output_buffer buf)) args;
      (*This could be made much more efficient.*)
    Buffer.contents buf


let ocamlc ?(batteries=true) ?(package=[]) ?(options=[]) files =
  let package = if batteries then [`package "batteries"; `syntax "camlp4o"]@package 
                 else              package
  in
    (Findlib.command `ocamlc, (prepare_compiler_args `ocamlc package (options :> compiler_option list) files))

let ocamlopt ?(batteries=true) ?(package=[]) ?(options=[]) files =
  let package = if batteries then [`package "batteries"; `syntax "camlp4o"]@package 
                 else              package
  in
  (Findlib.command `ocamlopt, (prepare_compiler_args `ocamlopt package (options :> compiler_option list) files))

let ocaml ?(batteries=true) ?(options=[]) files =
  let executable = 
    if batteries then Findlib.resolve_path "@batteries/ocaml"
    else              Findlib.resolve_path "ocaml"
  in
  (executable, prepare_compiler_args `ocaml [] options files) 


(*let background (executable, args) =
  let (out_read, out_write) = Unix.pipe ()                 in
  let (err_read, err_write) = Unix.pipe ()                 in
    Unix.set_close_on_exec out_write;
    Unix.set_close_on_exec err_write;
    match Unix.fork() with
	0  ->
	  Unix.dup2 out_write Unix.stdout;
	  Unix.dup2 err_write Unix.stderr;
	  Unix.execv executable args
      | id ->
	  (id, Extlib.IO.input_channel (Unix.in_channel_of_descr out_read),  Extlib.IO.input_channel (Unix.in_channel_of_descr err_read))

let run (executable, args) =
  match Unix.fork() with
      0  -> Unix.execv executable args
    | id -> snd (Unix.waitpid [] id)*)
    

(*let ocamlc =
  run (Findlib.command `ocamlc)*)
