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

type url = string

type table =
    {
      url :     (string, string * url)              Hashtbl.t (**A map from fully qualified name to 
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


let browser = match Sys.os_type with
  | "Win32" -> "firefox.exe"
  | _       -> "firefox"

let browse name url =
  Printf.printf "Opening %s\n" name;
  ignore (Sys.command (Printf.sprintf "%s %s" browser url))

let tutorial () =
  browse "on-line OCaml Tutorial" "http://www.ocaml-tutorial.org/"

(**
   Do all the work of attempting to display the help.

   @param command The human-readable name of the command currently launched.
   @param table   The table in which to look for help.
   @param kind    The human-readable kind of help being looked for.
   @param kinds   The human-readable kind of help being looked for (plural form)
   @param item    The item requested by the user.
*)
let help_aux command table kind kinds item =
  let go source url = browse (Printf.sprintf "help on %s %S (%s)" kind item source) url in
  try let (source, url) = Hashtbl.find table.url item in
    go source url
  with Not_found ->
    try
    let completions = Hashtbl.find table.complete item in
      if RefList.length completions = 1 then (*Only one possibility*)
	let (_, item)     = RefList.hd completions      in
	let (source, url) = Hashtbl.find table.url item in
	  go source url
      else (*Display all possibilities*)
	Enum.print
	  ~first:(Printf.sprintf "Several %s exist with name %S. To open one of them, please write one of\n" kinds item)
	  (fun out (source, item) -> Printf.fprintf out " %s %S;; (this help comes from %s)\n" command item source)
	  IO.stdout
	  (RefList.enum completions)
    with Not_found ->
      Printf.printf "Sorry, I don't know any %s named %S.\n" kind item

let help_value = help_aux "help_value" values "value" "values"
let help_type  = help_aux "help_type"  types  "type"  "types"

let help = help_value (*That will change to encompass all tables.*)

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
      String.sub name index (String.length name - index) 
    with Not_found -> name

  let append table k (v:(string * string)) =
    try let found = Hashtbl.find table k in
      RefList.push found v
    with Not_found -> Hashtbl.add table k (RefList.empty ())

  let register ~name ~kind ~index ~directory =
    let directory = directory^"/" in
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
      Enum.iter 
	(fun line -> 
	   let (item, url) = String.split line ":" in
	   let (item, url) = (String.trim item, String.trim url) in
	     Hashtbl.add table.url item (name, directory^url); (*Add fully qualified name -> url*)
	     append table.complete (basename item) (name, item)
	)
	(File.lines_of index)

end
