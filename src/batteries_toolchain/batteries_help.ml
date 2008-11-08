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
open IO


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
  if Batteries_config.browse url <> 0 then
    Printf.eprintf "Sorry, I had a problem communicating with your browser and I couldn't open the manual.\n%!"

let go kind item source url = browse (Printf.sprintf "help on %s %S (%s)" kind item source) url

(*let tutorial () =
  browse "on-line OCaml Tutorial" "http://www.ocaml-tutorial.org/"*)

(*let debug fmt =
  Printf.eprintf fmt*)
let debug fmt =
  Printf.fprintf IO.stdnull fmt

let find_help command table kind item =
  try `Direct (Hashtbl.find table.url item)
  with 
    Not_found -> debug "[find_help] Nothing about %s %S, assuming it's a fully qualified name.\n%!" kind item;
    try
    let completions = Hashtbl.find table.complete item in
      match RefList.length completions with
	| 0 -> debug "[find_help] No completion about %s %S\n%!" kind item;
	    `None
	| 1 -> debug "[find_help] There's one completion about %s %S\n%!" kind item;
	       (try `Direct (Hashtbl.find table.url (snd (RefList.hd completions)))
                with Not_found -> `Inconsistency)
	| n -> debug "[find_help] Total of %d completions for %s %S\n%!" n kind item;
	    `Suggestions (List.map (fun (_, item) -> Printf.sprintf "%s %S;;" command item) (RefList.to_list completions))
    with Not_found -> `None

(**
   Do all the work of attempting to display the help.

   @param command The human-readable name of the command currently launched.
   @param table   The table in which to look for help.
   @param kind    The human-readable kind of help being looked for.
   @param kinds   The human-readable kind of help being looked for (plural form)
   @param item    The item requested by the user.
*)
let man_aux command table kind kinds item =
  match find_help command table kind item with
    | `Direct (source, url)   -> go kind item source url
    | `None | `Inconsistency  -> Printf.printf "Sorry, I don't know any %s named %S.\n%!" kind item
    | `Suggestions l          ->
	Printf.printf "Several %s exist with name %S. To obtain the help on one of them, please use one of\n %a%!"
	  kinds item 
	(List.print
	  ~first:""
	  ~sep:"\n "
	  ~last:"\n"
	  String.print)
	  l
	  
let man_value    = man_aux "#man_value"      values     "value"                 "values"
let man_type     = man_aux "#man_type"       types      "type"                  "types"
let man_language = man_aux "#man_language"   language   "language topic"        "language topics"
let man_module   = man_aux "#man_module"     modules    "module"                "modules"
let man_exception= man_aux "#man_exception"  exns       "exception"             "exceptions"
let man_exn      = man_exception
let man_signature= man_aux "#man_signature"  modtypes   "signature"             "signatures"
let man_modtype  = man_signature
let man_class    = man_aux "#man_class"      classes    "class"                 "classes"
let man_method   = man_aux "#man_method"     methods    "method"                "methods"
let man_attribute= man_aux "#man_attributes" attributes "attribute"             "attributes"
let man_field    = man_attribute
let man_objtype  = man_aux "#man_objtype"    objtypes   "object type"           "object types"

(*command name,    table,     singular name, plural name, indefinite name*)
let helpers = [("#man_value",     values   , "value",     "values",     "a value");
	       ("#man_type",      types    , "type",      "types",      "a type" );
	       ("#man_language",  language , "language construction",   "language topics","a language topic");
	       ("#man_module",    modules  , "module",    "modules",    "a module"     );
	       ("#man_exception", exns     , "exception", "exceptions", "an exception");
	       ("#man_signature", modtypes , "signature", "signatures", "a signature" );
	       ("#man_class",     classes  , "class",     "classes",    "a class"     );
	       ("#man_method",    methods,   "method",    "methods",    "a method"    );
	       ("#man_attribute", attributes,"attribute", "attributes", "an attribute"    );
	       ("#man_objtype",   objtypes , "object type", "object types", "an object type")]
let man item =
  let results = List.map (fun (command, table, kind, kinds, a_kind) -> 
			    (command, find_help command table kind item, kind, kinds, a_kind))
    helpers in
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

  let append_to_table table k (v:(string * string)) =
    let found = 
      try Hashtbl.find table k
      with Not_found -> 
	let l = RefList.empty ()
	in Hashtbl.add table k l;
	  l
    in
      RefList.push found v

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
	debug "Now registering file %s (%s)\n" index name;
      Enum.iter 
	(fun line -> 
	   Scanf.sscanf line " %S : %S " (fun item url ->
	     let full_url = try ignore (String.find url "://");
	                        url
	                    with Invalid_string -> prefix^url
	     in
	     Hashtbl.add table.url item (name, full_url); (*Add fully qualified name -> url*)
	     append_to_table table.complete (basename item) (name, item);
	     debug "Adding manual %S => %S (%S)\n" item full_url name;
	     debug "Adding completion %S => %S (%S)\n" (basename item) item name
	))
	(File.lines_of index)
      with e -> 
	Printf.eprintf "While initializing the on-line help, error reading file %S\n%s%!" index (Printexc.to_string e)

  let auto_register () =
    let root_dir   = Batteries_config.documentation_root           in
    let root_file  = Filename.concat root_dir "documentation.idex" in
      begin
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
	Printf.eprintf "While initializing the on-line help, error reading file %S\n%s%!" root_file (Printexc.to_string e)
      end
(*;
      List.iter 
	( fun(_, table, singular, _, _) ->
	    let file = "/tmp/"^singular in
	      Printf.eprintf "Dumping table %s to file %S\n %!" singular file;
	      File.with_file_out file (
		fun cout ->
		  Printf.fprintf cout "URL\n";
		  Hashtbl.iter (fun key (name, url) ->
				  Printf.fprintf cout "%s -> %s (%s)\n" key url name
			       ) table.url;
		  Printf.fprintf cout "\nCompletions\n";
		  Hashtbl.iter (fun key list ->
				  Printf.fprintf cout "%s -> %a\n" key 
				    (List.print 
				       (fun out (source, name) -> Printf.fprintf out "%s (%s)" name source
				       )) (RefList.to_list list)
			       ) table.complete
	      )
	) helpers*)
end;;

let help () =
  File.with_file_in (Batteries_config.documentation_root ^ "/toplevel.help")
    (fun file -> copy file stdout);
  flush stdout;;

let init () =
  Extend.auto_register ();
  List.iter
    (fun (command, table, singular, plural, _) ->
       let name = (String.sub command 1 (String.length command - 1)) (*remove leading "#"*) in
	 Hashtbl.add
	   Toploop.directive_table
	   name
	   (Toploop.Directive_string (man_aux command table singular plural)))
    helpers;
  Hashtbl.add
    Toploop.directive_table
    "man"
    (Toploop.Directive_string man);
  Hashtbl.add
    Toploop.directive_table
    "help"
    (Toploop.Directive_none help)

