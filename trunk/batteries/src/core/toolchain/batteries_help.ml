(* 
 * Batteries_help - Calling the help system from the toplevel
 * Copyright (C) 2008 David Teller, LIFO, Universite d'Orleans
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
open ExtPrintf
open ExtString
open ExtList

(*
(** Catch errors *)
let protect f arg =
  try
    let _ = f arg in ()
  with
      Failure s ->
	print_endline s
    | Fl_package_base.No_such_package(pkg, reason) ->
	print_endline ("No such package: " ^ pkg ^ 
		       (if reason <> "" then " - " ^ reason else ""))
    | Fl_package_base.Package_loop pkg ->
	print_endline ("Package requires itself: " ^ pkg)
;;
*)


type url = string

type table =
    {
      url :     (string, string * url)                Hashtbl.t (**A map from fully qualified name to 
								 - the name of the help package containing the item
								 - the full URL for the help on that item.*);
      complete: (string, (string * string) RefList.t) Hashtbl.t (**A map from unqualified name to a list of
								 - name of the help packages containing the item
								 - fully qualified names for this name.*)
    }
let table () =
  { url      = Hashtbl.create 16;
    complete = Hashtbl.create 16 }

let language = table ()
let values   = table ()
let types    = table ()
let modules  = table ()
let exns     = table ()
let modtypes = table ()
let classes  = table ()
let methods  = table ()
let attributes=table ()
let objtypes = table ()


let browse name url =
  Printf.printf "Opening %s\n%!" name;
  ignore (Batteries_config.browse url)

let go kind item source url = browse (Printf.sprintf "help on %s %S (%s)" kind item source) url

let tutorial () =
  browse "on-line OCaml Tutorial" "http://www.ocaml-tutorial.org/"

let find_help command table item =
  try `Direct (Hashtbl.find table.url item)
  with Not_found -> 
    try
    let completions = Hashtbl.find table.complete item in
      match RefList.length completions with
	| 0 -> `None
	| 1 -> (try `Direct (Hashtbl.find table.url (snd (RefList.hd completions)))
                with Not_found -> `Inconsistency)
	| _ -> `Suggestions (List.map (fun (_, item) -> Printf.sprintf "%s %S;;" command item) (RefList.to_list completions))
    with Not_found -> `None

(**
   Do all the work of attempting to display the help.

   @param command The human-readable name of the command currently launched.
   @param table   The table in which to look for help.
   @param kind    The human-readable kind of help being looked for.
   @param kinds   The human-readable kind of help being looked for (plural form)
   @param item    The item requested by the user.
*)
let help_aux command table kind kinds item =
  match find_help command table item with
    | `Direct (source, url)   -> go kind item source url
    | `None | `Inconsistency  -> Printf.printf "Sorry, I don't know any %s named %S.\n%!" kind item
    | `Suggestions l          ->
	Printf.printf "Several %s exist with name %S. To obtain the help on one of them, please use one of\n%a%!"
	  kinds item 
	(List.print
	  ~first:""
	  ~sep:"\n "
	  ~last:"\n"
	  String.print)
	  l

	  

let help_value    = help_aux "#help_value"      values     "value"                 "values"
let help_type     = help_aux "#help_type"       types      "type"                  "types"
let help_language = help_aux "#help_language"   language   "language construction" "language constructions"
let help_module   = help_aux "#help_module"     modules    "module"                "modules"
let help_exception= help_aux "#help_exception"  exns       "exception"             "exceptions"
let help_exn      = help_exception
let help_signature= help_aux "#help_signature"  modtypes   "signature"             "signatures"
let help_modtype  = help_signature
let help_class    = help_aux "#help_class"      classes    "class"                 "classes"
let help_method   = help_aux "#help_method"     methods    "method"                "methods"
let help_attribute= help_aux "#help_attributes" attributes "attribute"             "attributes"
let help_field    = help_attribute
let help_objtype  = help_aux "#help_objtype"    objtypes   "object type"           "object types"

(*command name,    table,     singular name, plural name, indefinite name*)
let helpers = [("#help_value",     values   , "value",     "values",   "a value");
	       ("#help_type",      types    , "type",      "types",    "a type" );
	       ("#help_language",  language , "language construction",  "language constructions","a language construction");
	       ("#help_module",    modules  , "module",    "modules",  " a module"     );
	       ("#help_exception", exns     , "exception", "exceptions", "an exception");
	       ("#help_signature", modtypes , "signature", "signatures", "a signature" );
	       ("#help_class",     classes  , "class",     "classes",    "a class"     );
	       ("#help_method",    methods,   "method",    "methods",    "a method"    );
	       ("#help_attribute", attributes,"attribute", "attributes", "an attribute"    );
	       ("#help_objtype",   objtypes , "object type", "object types", "an object type")]
let help item =
  let results = List.map (fun (command, table, kind, kinds, a_kind) -> 
			    (command, find_help command table item, kind, kinds, a_kind))
    helpers in
(*    if List.for_all (fun `None -> true | _ -> false) results then (*No solution*)
      Printf.printf "Sorry, I can't help you with %S.\n%!" item
    else*)
      match List.fold_left 
	(fun acc (command, result, kind, kinds, a_kind) -> match result with
	   | `None | `Inconsistency -> acc
	   | `Direct destination    -> 
	       let line = Printf.sprintf "%s. For more information on %S as %s, you may use\n  %s %S;;\n" 
		 a_kind item a_kind command item in
		 begin
		   match acc with
		     | `None_so_far                      -> `One_possibility (destination, kind, line)
		     | `One_possibility (_, _, previous) -> `Several_possibilities [previous;line]
		     | `Several_possibilities l          -> `Several_possibilities (line::l)
		 end
	   | `Suggestions l         ->
	       let line = 
		 Printf.sprintf2 "%s, with several possibilities. For more information on %S as %s, you may use\n%a" a_kind item a_kind
		    (List.print ~first:"  " ~sep:"\n  " ~last:"" String.print) l
	       in
	       match acc with
		 | `None_so_far                      -> `Several_possibilities [line]
		 | `One_possibility (_, _, previous) -> `Several_possibilities [previous; line]
		 | `Several_possibilities previous   -> `Several_possibilities (line::previous)
	)
	`None_so_far results with
	  | `None_so_far                              -> Printf.printf "Sorry, I can't help you with %S.\n%!" item
	  | `One_possibility ((source, url), kind, _) -> go kind item source url
	  | `Several_possibilities lines              ->
	      let first = Printf.sprintf "Several definitions exist for %S.\nThis item exists as " item
	      and sep   = Printf.sprintf "\nItem %S also exists as " item in
		Printf.printf "%a\n%!" (List.print ~first ~sep ~last:"\n" String.print) lines;;

(** {6 Add directives}*)

(*Toploop.toplevel_startup_hook :=
  fun () ->
    !Toploop.toplevel_startup_hook ();*)



(*
    if !Sys.interactive then
      Printf.printf "OCaml Batteries Included succesfully loaded.\n If you need help on some topic, invoke:\n  #help \"the_topic\"\n%!" 
  ;;
*)


module Extend =
struct
  type kind =
    | Language
    | Values  
    | Types
    | Modules
    | Exceptions
    | Module_types
    | Classes
    | Methods
    | Attributes
    | Class_types


  let basename name =
    try let index = String.rindex name '.' in
      String.sub name ( index + 1 ) (String.length name - index - 1) 
    with Not_found -> name

  let append table k (v:(string * string)) =
    try let found = Hashtbl.find table k in
      RefList.push found v
    with Not_found -> Hashtbl.add table k (RefList.empty ())

  let register ~name ~kind ~index ~prefix =
    let prefix = if String.length prefix = 0 then "/"
                 else if String.get prefix (String.length prefix - 1) = '/' then prefix
                 else prefix^"/"
    in
    let table = match kind with
      | Language       -> language
      | Values         -> values
      | Types          -> types
      | Modules        -> modules
      | Exceptions     -> exns
      | Module_types   -> modtypes
      | Classes        -> classes
      | Methods        -> methods
      | Attributes     -> attributes
      | Class_types    -> objtypes
    in
      try
      Enum.iter 
	(fun line -> 
	   Scanf.sscanf line " %S : %S " (fun item url ->
(*	     Printf.eprintf "Adding manual %S => %S (%S)\n" item url name;
	     Printf.eprintf "Adding completion %S => %S (%S)\n" (basename item) item name;*)
	     Hashtbl.add table.url item (name, prefix^url); (*Add fully qualified name -> url*)
	     append table.complete (basename item) (name, item)
	))
	(File.lines_of index)
      with e -> 
	Printf.eprintf "While initializing the on-line help, error reading file %S\n%!" index;
	Printexc.print e (*At this point, just ignore errors*)

  let auto_register () =
    let root_dir   = Batteries_config.documentation_root           in
    let root_file  = Filename.concat root_dir "documentation.idex" in
    (*let prefix = "file://"^root_dir                                in*)
      try
    Enum.iter
      (fun line -> 
	 Scanf.sscanf line "%s %s " 
	   (fun category index ->
  	      let maybe_kind = 
		match category with
		| "language" -> Some Language
		| "values"   -> Some Values
		| "types"    -> Some Types
		| "modules"  -> Some Modules
		| "exceptions"| "exns"         -> Some Exceptions
		| "modtypes"  | "module_types" -> Some Module_types
		| "classes"                    -> Some Classes
		| "methods"                    -> Some Methods
		| "attributes"                 -> Some Attributes
		| "class_types"                -> Some Class_types
		| ""                           -> None
		| _                            -> Printf.eprintf 
		    "Warning: During the initialization of the help system from index %S, I don't know what to do with category %S\n%!" 
		      root_file category;
		    None
	      in
		match maybe_kind with 
		    Some kind ->
		      let index          = Filename.concat  root_dir index in
		      let html_directory = Filename.dirname index       in
		      register ~name:"OCaml Batteries Included" ~kind
			~index
			~prefix:("file://"^html_directory)
		  | _ -> ()
	   )
      )
      (File.lines_of root_file)
      with e ->
	Printf.eprintf "While initializing the on-line help, error reading file %S\n%!" root_file;
	Printexc.print e
end;;

let init () =
  Extend.auto_register ();
  List.iter
    (fun (command, table, singular, plural, _) ->
       let name = (String.sub command 1 (String.length command - 1)) (*remove leading "#"*) in
	 Hashtbl.add
	   Toploop.directive_table
	   name
	   (Toploop.Directive_string (help_aux command table singular plural)))
    helpers;
  Hashtbl.add
    Toploop.directive_table
    "help"
    (Toploop.Directive_string help)

