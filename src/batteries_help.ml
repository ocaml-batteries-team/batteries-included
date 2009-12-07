(* 
 * Batteries_help - Calling the help system from the toplevel
 * Copyright (C) 2009 David Teller, LIFO, Universite d'Orleans
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

open BatIO

(*let debug fmt =
  Printf.eprintf fmt*)
let debug fmt =
  BatPrintf.fprintf BatIO.stdnull fmt



(**
   {6 Kinds}
*)


type kinds = 
  | Values
  | Types
  | Topics
  | Modules
  | Exns
  | Modtypes
  | Classes
  | Methods
  | Attributes
  | Objtypes

(** Parse a category name into a topic.*)
let kind_of_name = function
  | "topic" | "language"   -> Some Topics
  | "values"   -> Some Values
  | "types"    -> Some Types
  | "modules"  -> Some Modules
  | "exceptions"| "exns"         -> Some Exns
  | "modtypes"  | "module_types" -> Some Modtypes
  | "classes"                    -> Some Classes
  | "methods"                    -> Some Methods
  | "attributes"                 -> Some Attributes
  | "class_types"                -> Some Objtypes
  | _                            -> None

(**
   {6 Tables}
*)

type url       = string(**A kind of string used to represent URLs. Distinguished for documentation purposes.*)
type qualified = string(**A kind of string used to represent fully-qualified names.*)
type unqualif  = string(**A kind of string used to represent unqualified names, i.e. names without their module.*)
type package   = string(**A lomd pf stromg used to represent help providers.*)

type suggestion =
    {
      url       : url(**The url to open in the browser to visit help on this suggestion.*);
      spackage  : package(**The package which provides the url.*);
    }

type completion =
    {
      qualified: qualified (**A possible qualified name matching the request*);
      cpackage : package (**The package which provides the completion.*)
    }

type table =
    {
      suggestions: (qualified, suggestion)     Hashtbl.t(**A map from fully qualified name to suggestions.*);
      completions: (unqualif, completion list) Hashtbl.t(**A map from unqualified name to a list of completions.*)
    }


(**
   Convert a table of reflists to a table of lists.
*)
let table_of_tableref t = 
  let result = Hashtbl.create (Hashtbl.length t) in
    Hashtbl.iter (fun k d -> Hashtbl.add result k (BatRefList.to_list d)) t;
    result

let append_to_table table k v =
    let found = 
      try Hashtbl.find table k
      with Not_found -> 
	let l = BatRefList.empty ()
	in Hashtbl.add table k l;
	  l
    in
      BatRefList.push found v


(**
   {6 Browsing}
*)

let browse pages =
  try
    List.iter (fun page -> 
		 debug "Showing %s\n" page.url;
		 if Batteries_config.browse page.url <> 0 then failwith "Browser") pages
  with Failure "Browser" -> 
    Printf.eprintf "Sorry, I had a problem communicating with your browser and I couldn't open the manual.\n%!"



(**
   {6 Loading}
*)

(**Extract the unqualified name of a possibly qualified name.

   [local_name "a.b.c.d"] produces ["d"]*)
let local_name s =
  try snd (BatString.rsplit s ".")
  with Not_found -> s

(**
   Load the contents of an index file into hash tables.
*)
let load_index ~name ~index ~prefix ~suggestions ~completions =
  try
    BatEnum.iter
      (fun line -> 
	 Scanf.sscanf line " %S : %S " 
	   (fun item url ->
	      let full_url = try ignore (BatString.find url "://"); url
	      with Not_found -> prefix^url
	      in
		Hashtbl.add suggestions item {spackage = name; url = full_url}; (*Add fully qualified name -> url*)
		let basename = Filename.basename item in
		let leafname = local_name basename    in
		let completion={cpackage = name; qualified = item} in
		append_to_table completions basename completion;
		  if leafname <> basename then append_to_table completions leafname completion;
		debug "Adding manual %S => %S (%S)\n" item full_url name;
		debug "Adding completion %S => %S (%S)\n" basename item name;
		debug "Adding completion %S => %S (%S)\n" leafname item name
	   ))
      (BatFile.lines_of index)
  with e -> 
    Printf.eprintf
      "While initializing the on-line help, error reading index file %S\n%s\n%!"
      index (Printexc.to_string e)



(** Acquire a table, loading it if it hasn't been loaded yet.

    {b Note} This function is thread-unsafe. Don't call it from any thread other than the main thread.
*)
let get_table =
  let tables : (kinds, table) Hashtbl.t = Hashtbl.create 16
  in fun kind ->
    try Hashtbl.find tables kind
    with Not_found ->
      let root_dir   = Batteries_config.documentation_root           in
      let root_file  = Filename.concat root_dir "documentation.idex" in
	try
	  let suggestions = Hashtbl.create 256
	  and completions = Hashtbl.create 256 in
	  BatEnum.iter
	    (fun line -> 
	       try
	       Scanf.sscanf line "%s %s " 
		 (fun category index ->
		    match kind_of_name category with
		      | Some k when k = kind ->
			  let index          = Filename.concat  root_dir index in
			  let html_directory = Filename.dirname index          in
			    if Sys.file_exists index then
		              load_index
				~name:"OCaml Batteries Included" 
				~index
				~prefix:("file://"^html_directory^"/")
				~suggestions
				~completions
		      | _ -> ()
		 )
	       with _ -> () (*At this point, ignore syntax errors, they're probably comments.*)
	    ) 
	    (BatFile.lines_of root_file);
	    let result = {suggestions = suggestions; completions = table_of_tableref completions} in
	      Hashtbl.add tables kind result;
	      result
	      
	with e ->
	  Printf.eprintf
	    "While initializing the on-line help, error in root doc file %S\n%s\n%!" root_file
	    (Printexc.to_string e);
	    let result = {suggestions = Hashtbl.create 0; completions = Hashtbl.create 0} in
	      Hashtbl.add tables kind result;
	      result

	      


(**
   {6 Searching}
*)

(**Print a warning regarding inconsistencies.*)
let inconsistency topic subject = 
  Printf.eprintf "Configuration issue: the help system promises something about a %s called %S but does not contain anything such. There may be an error with your installation of the documentation.\n" topic subject

(**
   Find all the URL of each qualified name from a list of completions.
   
   Qualified names which can't be found in the table are dropped and a warning is printed.
*)
let result_of_completions table singular subject (l:completion list) =
  BatList.filter_map (fun {qualified = q} -> try Some (Hashtbl.find table.suggestions q) with Not_found -> 
		inconsistency singular subject; (*Report internal inconsistency*)
		None) l

(**A deconstructor for [completion].*)
let get_qualified {qualified = q} = q

(**
   Look for a given subject inside one of the manuals

   @param cmd The command used to invoke this manual. This string is used to suggest further searches.
   @param singular The singular noun corresponding to this manual. This string is used to display
   information regarding where the information may be found.
   @param plural The plural noun corresponding to this manual. This string is used to display
   information regarding where the information may be found.
   @param undefined The undefined noun corresponding to this manual. This string is used to display
   information regarding where the information may be found.
   @param kind The key corresponding to the manual.
   @param subject The subject to search inside a manual.

*)
let man_aux ~cmd ~kind ~singular ~plural ~undefined  subject =
  try
    let table = get_table kind in
      try match Hashtbl.find table.completions subject with
	| []                -> `No_result (*No completion on the subject, report subject not found*)
	| [{qualified = q}] as l -> (*Check for inconsistency*)
	    (try ignore (Hashtbl.find table.suggestions q); `Suggestions (l, table)
	    with Not_found -> inconsistency singular subject; `No_result)
	| l                -> `Suggestions (l, table)
      with Not_found -> `No_result
  with Sys_error e -> 
    Printf.printf "Sorry, I had a problem loading the help on %s. Deactivating help on that subject.\n Detailed error message is %s\n" plural e;
    `No_result

(**
   Look for a given subject inside one of the manuals and display the results.

   @param cmd The command used to invoke this manual. This string is used to suggest further searches.
   @param singular The singular noun corresponding to this manual. This string is used to display
   information regarding where the information may be found.
   @param plural The plural noun corresponding to this manual. This string is used to display
   information regarding where the information may be found.
   @param undefined The undefined noun corresponding to this manual. This string is used to display
   information regarding where the information may be found.
   @param kind The key corresponding to the manual.
   @param tabs If [true], all matching subjects will be opened, each one in its tab. Otherwise,
   a message will allow selecting one subject.
   @param subject The subject to search inside a manual.

*)
let man ~cmd ~kind ~singular ~plural ~undefined ~tabs subject =
  match man_aux ~cmd ~kind ~singular ~plural ~undefined subject
  with  `No_result       -> Printf.printf "Sorry, I don't know any %s named %S.\n%!" singular subject
    |   `Suggestions (l,table) when tabs -> browse (result_of_completions table singular subject l)
    |   `Suggestions ([h],table)         -> browse (result_of_completions table singular subject [h])
    |   `Suggestions (l,_) -> 
	  BatPrintf.printf "Several %s exist with name %S. To obtain help on one of them, please use one of\n %a%!"
	    plural subject
	    (BatList.print ~first:"" ~sep:"\n " ~last:"\n" (fun out {qualified = q} -> BatPrintf.fprintf out " %s %S\n" cmd q))
	    l

(**
   Look for a given subject across all manuals and display the results.
*)
let man_all sources ~tabs subject =
  let found_something = 
    if tabs then
      List.fold_left (fun was_found     (*Browse help directly*)
			(cmd, kind, singular, plural, undefined) ->
			  match man_aux ~cmd ~kind ~singular ~plural ~undefined subject with
			    | `No_result     -> was_found
			    | `Suggestions (l, table) -> 
				match result_of_completions table singular subject l with
				  | [] -> false (*Inconsistency*)
				  | l' -> let _ = browse l' in true)
	false sources
    else
      match
    List.fold_left 
      (fun (((result_as_strings : string list)(**The text to display, as a list of strings, one string per kind.*),
	    one_suggestion    (**The latest suggestion -- used only in case there's only one suggestion.*)) as acc)
	 (cmd, kind, singular, plural, undefined) ->
	   match man_aux ~cmd ~kind ~singular ~plural ~undefined subject with
	     | `No_result                -> acc
	     | `Suggestions ([h], table) -> 
		 let display : string =
		   Printf.sprintf "There's information on %S in %s. To read this information, please use\n %s %S%!"
		   subject plural cmd h.qualified in
		 (display :: result_as_strings, `Browse (h, table, singular))
	     | `Suggestions (l,_)  ->
		 let display : string = 
		   BatPrintf.sprintf2 "There's information on %S in %s. To read this information, please use one of\n%a%!"
		     subject plural
		     (BatList.print ~first:"" ~sep:"" ~last:"" 
			(fun out {qualified = q} -> BatPrintf.fprintf out " %s %S\n" cmd q))
		     l
		 in (display::result_as_strings, `No_browsing))
      ([], `No_result) sources
      with 
	| ([], _)                 -> false (*No result*)
	| ([h],`Browse (l,table, singular) ) -> (match result_of_completions table singular subject [l] with
	    | [] -> false (*Inconsistency*)
	    | l' -> let _ = browse l' in true)
	| (texts, _) ->
	    BatPrintf.printf "Several definitions exist for %S.\n%a%!" subject
	      (BatList.print ~first:"" ~sep:"\n" ~last:"\n" BatString.print)
	      texts;
	    true
  in if not found_something then
      Printf.printf "Sorry, I don't know anything about %S.\n%!" subject

(**
   {6 Registration}
*)

(** The various functions which may be used to access the manual.*)
let helpers = 
  let sources = 
    [("#man_value",     Values   , "value",       "values",     "a value");
     ("#man_type",      Types    , "type",        "types",      "a type" );
     ("#man_topic",     Topics   , "topic",       "topics",     "a topic");
     ("#man_module",    Modules  , "module",      "modules",    "a module"     );
     ("#man_exception", Exns     , "exception",   "exceptions", "an exception");
     ("#man_signature", Modtypes , "signature",   "signatures", "a signature" );
     ("#man_class",     Classes  , "class",       "classes",    "a class"     );
     ("#man_method",    Methods,   "method",      "methods",    "a method"    );
     ("#man_attribute", Attributes,"attribute",   "attributes", "an attribute"    );
     ("#man_objtype",   Objtypes , "object type", "object types", "an object type")]
  in
    ("man", man_all sources ~tabs:false)::
    (List.map (fun (cmd, kind, singular, plural, undefined) -> (String.sub cmd 1 (String.length cmd - 1),
								man ~cmd ~kind ~singular ~plural ~undefined ~tabs:false)) sources)
     

(**Launch the introductory help text.*)
let help () =
  BatFile.with_file_in (Batteries_config.documentation_root ^ "/toplevel.help")
    (fun file -> copy file stdout);
  flush stdout;;

(**Print the signature of a module.*)
let print_module name = 
  try
    let flattened = Str.global_replace (Str.regexp "[^_0-9a-zA-Z]") "__" name in
    let phrase = !Toploop.parse_toplevel_phrase (Lexing.from_string (Printf.sprintf "module %s = %s;;" flattened name)) in
      ignore (Toploop.execute_phrase true Format.std_formatter phrase)
  with _ -> ();;

let man = List.assoc "man" helpers

(** Initialize the help system (lazily)*)
let init () =
  try 
    (*The manual*)
    List.iter (fun (key, search) -> Hashtbl.add Toploop.directive_table key (Toploop.Directive_string search))
      helpers;
    (*Directive #help*)
    Hashtbl.add
      Toploop.directive_table
      "help"
      (Toploop.Directive_none help);
    (*Directive #browse*)
    Hashtbl.add
      Toploop.directive_table
      "browse"
      (Toploop.Directive_string print_module)
  with e -> Printf.printf "Error while initializing help system:\n%s\n%!" (Printexc.to_string e)
