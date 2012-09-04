(*
 * Odoc_generator_batlib - custom documentation generator for Batteries
 * Copyright (C) 2008 Maxence Guesdon
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

(*From OCamlDoc*)
open Odoc_info;;
module Naming = Odoc_html.Naming
module Name   = Odoc_name
open Odoc_info.Value
open Odoc_info.Module
open Odoc_info.Type
open Odoc_info.Class
open Odoc_info.Exception

(*From the base library*)
open List

(** {1 Tools}*)

(** Concatenate two names into a module path.
    [concat a b] is [a^"."^b] if neither [a]
    nor [b] is empty, [a] if [b] is empty and
    [b] if [a] is empty.*)
let concat a b =
  if String.length b = 0      then a
  else if String.length a = 0 then b
  else Name.concat a b

(** Return the basename in a path.

    [end_of_name "A.B.C.D.E.t"] produces ["t"] *)
let end_of_name name =
  Name.get_relative (Name.father (Name.father name)) name

(** Print an [info option]*)
let string_of_info_opt = function
  | None   -> "No information"
  | Some i -> info_string_of_info i


let bs = Buffer.add_string
let bp = Printf.bprintf
let opt = Odoc_info.apply_opt
let new_buf () = Buffer.create 1024


(** {1 Configuration}*)

(** A list of primitive type names for which we should rather link
    to the corresponding module *)
let primitive_types_names =
  [   "char",      "Data.Text.Char.t";
      "string",    "Data.Text.String.t";
      "array",     "Data.Mutable.Array.t" ;
      "lazy_t",    "Data.Persistent.Lazy.t";
      "list",      "Data.Persistent.List.t";
      "option",    "Data.Persistent.Option.t";
      "int32",     "Data.Numeric.Int32.t";
      "int64",     "Data.Numeric.Int64.t";
      "nativeint", "Data.Numeric.Nativeint.t";
      "big_int",   "Data.Numeric.Big_int.t";
      "int",       "Data.Numeric.Int.t";
      "bool",      "Data.Logical.Bool.t";
      "unit",      "Data.Numeric.Unit.t";
      "float",     "Data.Numeric.Float.t";
      "ref",       "Data.Mutable.Ref";
      (*"exn",     "Batteries.Control.Exceptions.Exn.t";*)(*Module not implemented yet*)
      "format4",   "Languages.Printf.format4"
]

let has_parent a ~parent:b =
  a = b ||
  let len_a  = String.length a
  and len_b  = String.length b    in
  let result =
    len_a > len_b &&
      let prefix = String.sub a 0 len_b in
	prefix = b &&
	  a.[len_b] = '.'
  in verbose (Printf.sprintf "Checking whether %s has parent %s: %b" a b result);
    result


(** The list of modules which should appear as roots in the hierarchy. *)
let roots = ["Batteries"]

let merge_info_opt a b =
  verbose ("Merging informations");
  if a <> b then
    begin
      verbose ("1: "^(string_of_info_opt a));
      verbose ("2: "^(string_of_info_opt b));
      let result = Odoc_merge.merge_info_opt Odoc_types.all_merge_options a b in
	verbose (">: "^(string_of_info_opt result));
	result
    end
  else
    a

(** {1 Actual rewriting}*)

(**[get_documents i] determines if information [i] specifies that this module
   "documents" another module, not included in the source tree. Specifying that
   module [Foo] documents module [Bar] means that every hyperlink to some element
   [e] in module [Bar] should actually point to an with the same name in module
   [Foo].

   To add such a specification, add [@documents Bar] to the module comments of
   module [Foo].

   Typical use of this feature:
   - the documentation of module [Sna] makes use of elements (types, values, etc.)
     of module [Bar]
   - module [Bar] is not included in the project, as it belongs to another project
   - for some reason, documenting module [Bar] is important, possibly because this
     module has not been documented by its original author or because it is expected
     that developers will need to read through the documentation of module [Bar] so
     often that this documentation should be added to the project
   - create a module [Foo] in the project by importing or re-creating [bar.mli]
   - document [foo.mli]
   - add [@documents Bar] in the module comments of module [Foo]
*)
let get_documents = function
  | None   -> []
  | Some i ->
      List.fold_left (fun acc x -> match x with
			| ("documents", [Raw s]) ->
			    verbose ("This module documents "^s);
			    s::acc
			| ("documents", x      ) ->
			    warning ("Weird documents "^(string_of_text x));
			    (string_of_text x)::acc
			| _ -> acc) [] i.i_custom

(* undocumented for now, possibly useless *)
let get_documented = function
  | None   -> []
  | Some i ->
      List.fold_left (fun acc x -> match x with
			| ("documented", [Raw s]) ->
			    verbose ("This module should take its place as "^s);
			    s::acc
			| ("documented", x      ) ->
			    warning ("Weird documented "^(string_of_text x));
			    (string_of_text x)::acc
			| _ -> acc) [] i.i_custom


(** [module_dependencies m] lists the dependencies of a module [m] in
    terms of other modules*)
let module_dependencies m =
  let rec handle_kind acc = function
    | Module_struct  e      -> List.fold_left (fun acc x -> handle_element acc x) acc e
    | Module_alias   a      -> a.ma_name :: acc
    | Module_functor (_, a) -> handle_kind acc a
    | Module_apply (a, b)   -> handle_kind (handle_kind acc a) b
    | Module_with (_, _)    -> acc
    | Module_constraint (a, _) -> handle_kind acc a
  and handle_element acc = function
    | Element_module m          -> handle_kind acc m.m_kind
    | Element_included_module a -> a.im_name :: acc
    | _                         -> acc
  in handle_kind m.m_top_deps m.m_kind


(**
   [rebuild_structure m] walks through list [m] and rebuilds it as a forest
   of modules and sub-modules.

   - Resolving aliases: if we have a module [A] containing [module B = C], module [C]
   is renamed [A.B] and we keep that renaming for reference generation.
   - Resolving inclusions: if we have a module [A] containing [include C], the contents
   of module [C] are copied into [A] and we fabricate a renaming from [C] to [A] for
   reference generation.

   @return [(m, r)], where [m] is the new list of modules and [r] is a mapping from
   old module names to new module names.
*)
let rebuild_structure modules =
  let all_renamed_modules      = Hashtbl.create 256  (**Mapping [old name] -> [new name, latest info]*)
  and all_renamed_module_types = Hashtbl.create 256  (**Mapping [old name] -> [new name, latest info]*)
  and all_modules              = Hashtbl.create 256  (**Mapping [name]     -> [t_module] -- unused*)
  in
  let add_renamed_module ~old:(old_name,old_info) ~current:(new_name,new_info) =
    verbose ("Setting module renaming from "^old_name^" to "^new_name);
    try
      let (better, better_info) = Hashtbl.find all_renamed_modules new_name in
	verbose ("... actually setting renaming from "^old_name^" to "^better);
	let complete_info = merge_info_opt (merge_info_opt old_info new_info) better_info in
	  Hashtbl.replace all_renamed_modules old_name (better, complete_info);
	  complete_info
    with Not_found ->
      let complete_info = merge_info_opt old_info new_info in
	Hashtbl.add all_renamed_modules old_name (new_name, complete_info);
	complete_info
  and add_renamed_module_type old current =
    verbose ("Setting module type renaming from "^old^" to "^current);
    try
      let further_references = Hashtbl.find all_renamed_module_types current in
	verbose ("... actually setting renaming from "^old^" to "^further_references);
	Hashtbl.add all_renamed_module_types old further_references
    with Not_found -> Hashtbl.add all_renamed_module_types old current
  in
(*First pass: build hierarchy*)
  let rec handle_kind path (m:t_module) = function
    | Module_struct     x      -> Module_struct (List.flatten (List.map (handle_module_element path m) x))
    | Module_alias      x      -> Module_alias (handle_alias path m x)
    | Module_functor    (p, k) -> Module_functor (p, handle_kind path m k)
    | Module_apply      (x, y) -> Module_apply (handle_kind path m x, handle_kind path m y)
    | Module_with       (k, s) -> Module_with (handle_type_kind path m k,  s)
    | Module_constraint (x, y) -> Module_constraint (handle_kind path m x, handle_type_kind path m y)
  and handle_module_element path m = function
    | Element_module    x      -> [Element_module      (handle_module path m x)]
    | Element_module_type x    -> [Element_module_type (handle_module_type path m x)]
    | Element_module_comment _  as y  -> [y]
    | Element_class x                ->
	[Element_class     {(x) with cl_name  = concat path (Name.simple x.cl_name)}]
    | Element_class_type x           ->
	[Element_class_type{(x) with clt_name = concat path (Name.simple x.clt_name)}]
    | Element_value x                ->
	[Element_value     {(x) with val_name = concat path (Name.simple x.val_name)}]
    | Element_exception x            ->
	[Element_exception {(x) with ex_name  = concat path (Name.simple x.ex_name)}]
    | Element_type x                 ->
	[Element_type {(x) with ty_name = concat path (Name.simple x.ty_name)}]
    | Element_included_module x as y ->
(*	verbose ("Meeting inclusion "^x.im_name);*)
	match x.im_module with
	  | Some (Mod a) ->
	      verbose ("This is an included module, we'll treat it as "^path);
	      let a' = handle_module path m {(a) with m_name = ""} in
		(
		  match a'.m_kind with
		    | Module_struct l ->
			(*Copy the contents of [a] into [m]*)
			(*Copy the information on [a] into [m]*)
			verbose ("Merging "^m.m_name^" and included "^a'.m_name);
			m.m_info <- merge_info_opt
			  (add_renamed_module ~old:(a.m_name,a.m_info) ~current:(m.m_name, m.m_info))
			  (add_renamed_module ~old:(Name.get_relative m.m_name a.m_name, None)
			     ~current:(m.m_name, m.m_info));
			l
		    | _               ->
			verbose ("Structure of the module is complex");
			[Element_included_module {(x) with im_module = Some (Mod a')}]
			  (*Otherwise, it's too complicated*)
		)
	  | Some (Modtype a) ->
(*	      verbose ("This is an included module type");*)
	      let a' = handle_module_type path m a in
		[Element_included_module {(x) with im_module = Some (Modtype a')}]
	  | None ->
	      verbose ("Module couldn't be found");
	      m.m_info <- add_renamed_module ~old:(x.im_name,None) ~current:(m.m_name,m.m_info);
	      [y]
  and handle_module path m t      =
    let path' = concat path (Name.simple t.m_name) in
      verbose ("Visiting module "^t.m_name^" from "^m.m_name^", at path "^path');
    let result =
      {(t) with
	 m_kind = handle_kind path' t t.m_kind;
	 m_name = path'} in
      result.m_info <- add_renamed_module ~old:(t.m_name,t.m_info) ~current:(path',None);
      (match get_documents t.m_info with
	 | [] -> verbose ("No @documents for module "^t.m_name)
	 | l  ->
	     List.iter (fun r ->
			  verbose ("Manual @documents of module "^r^" with "^path');
			  result.m_info <- add_renamed_module ~old:(r,None) ~current:(path',result.m_info)) l);
      (match get_documented t.m_info with
	 | [] -> verbose ("No @documented for module "^t.m_name)
	 | l  ->
	     List.iter (fun r ->
			  verbose ("Manual @documented of module "^r^" with "^path');
			  result.m_info <- add_renamed_module ~current:(r,None) ~old:(path',result.m_info)) l);
      result
  and handle_module_type path m (t:Odoc_module.t_module_type) =
    let path' = concat path (Name.simple t.mt_name) in
      let result =
	{(t) with mt_kind = (match t.mt_kind with
	   | None -> None
	   | Some kind -> Some (handle_type_kind path' m kind)); mt_name = path'} in
	add_renamed_module_type t.mt_name path';
	result
  and handle_alias path m (t:module_alias) : module_alias     = (*Module [m] is an alias to [t.ma_module]*)
    match t.ma_module with
      | None         ->
	  verbose ("I'd like to merge information from "^m.m_name^" and "^t.ma_name^" but I can't find that module");
	  t
	  (*let rec aux = function
	    | []   -> verbose ("Can't do better"); t
	    | x::xs -> if Name.prefix x t.ma_name then
		let suffix = Name.get_relative x t.ma_name in
		let info = add_renamed_module ~old:(suffix, m.m_info) ~current:(path, None) in
		  {(t) with ma_name = suffix}
	      else aux xs
	  in aux packs*)
      | Some (Mod a) ->
(*	  add_renamed_module a.m_name path;*)
	  verbose ("Merging information from "^m.m_name^" and aliased "^a.m_name);
	  let info = add_renamed_module ~old:(a.m_name,a.m_info) ~current:(path,m.m_info) in
	    m.m_info <- info;
	    a.m_info <- info;
	    let a' = {(a) with m_kind = handle_kind path m a.m_kind} in
	      {(t) with ma_module = Some (Mod a')}
      | Some (Modtype a) ->
	  verbose ("Merging information from "^m.m_name^" and aliased type "^a.mt_name);
	  m.m_info <- merge_info_opt m.m_info a.mt_info;
	  a.mt_info <- m.m_info;
	  add_renamed_module_type a.mt_name path;
	  let info = Odoc_merge.merge_info_opt Odoc_types.all_merge_options m.m_info a.mt_info in
	  let a' = match a.mt_kind with
	    | None      -> a
	    | Some kind -> {(a) with mt_kind = Some (handle_type_kind path m kind); mt_info = info} in
	    {(t) with ma_module = Some (Modtype a')}
  and handle_type_kind path m :module_type_kind -> module_type_kind   = function
    | Module_type_struct x      -> Module_type_struct (List.flatten (List.map (handle_module_element path m) x))
    | Module_type_functor (p, x)-> Module_type_functor (p, handle_type_kind path m x)
    | Module_type_alias x       -> Module_type_alias (handle_type_alias path m x)
    | Module_type_with (k, s)   -> Module_type_with (handle_type_kind path m k, s)
  and handle_type_alias path m t =  match t.mta_module with
      | None        -> (*verbose ("module type "^t.mta_name^" not resolved in cross-reference stage");*) t
      | Some a      ->
	  (*verbose ("module type "^a.mt_name^" renamed "^(concat m.m_name a.mt_name));*)
	  (*if a.mt_info <> None then m.m_info <- a.mt_info;*)
	  add_renamed_module_type a.mt_name path;
	  let info = Odoc_merge.merge_info_opt Odoc_types.all_merge_options m.m_info a.mt_info in
	  {(t) with mta_module = Some ({(a) with mt_name = concat m.m_name a.mt_name; mt_info = info})}
  in
  (*1. Find root modules, i.e. modules which are neither included nor aliased*)
(*  let all_roots = Hashtbl.create 100 in
    List.iter (fun x -> if Name.father x.m_name = "" then
		 (
(*		   verbose ("Adding "^x.m_name^" to the list of roots");*)
		   Hashtbl.add all_roots x.m_name x
		 ) (*else
		   verbose ("Not adding "^x.m_name^" to the list of roots")*)
	      ) modules;


    List.iter (fun x ->
		    begin
		      List.iter (fun y -> (*verbose(" removing "^y^" which is brought out by "^x.m_name);*)
				   Hashtbl.remove all_roots y
				) (*x.m_top_deps*) (module_dependencies x)
		    end)  modules;
    Hashtbl.iter (fun name _ -> verbose ("Root: "^name)) all_roots;
    (*let for_rewriting = Hashtbl.fold (fun k m acc -> if List.mem k roots then
					begin
					  verbose ("Rewriting: " ^k);
					  (k,m)::acc
					end
				      else
					begin
					  verbose ("Not rewriting: "^k);
					  acc
					end) all_roots [] in*)
      (*Actually, we're only interested in modules which appear in [roots]*)
      (*Note: we could probably do something much more simple, without resorting
	to this dependency analysis stuff*)*)
  let for_rewriting = List.fold_left
    (fun acc x ->
       Hashtbl.add all_modules x.m_name x;
       if List.mem x.m_name roots then begin
	 verbose ("We need to visit module "^x.m_name);
	 (x.m_name, x)::acc
       end else begin
	 verbose ("Discarding module "^x.m_name^" for now");
	 acc
       end) [] modules in
    verbose ("[Starting to rearrange module structure]");
(*    let for_rewriting = Hashtbl.fold (fun k m acc -> (k,m)::acc) all_roots [] in*)
      (*2. Dive into these*)
    (*let rewritten = Hashtbl.fold (fun name contents acc ->
		    {(contents) with m_kind = handle_kind name contents contents.m_kind}::acc
		 ) all_roots [] in*)
      let rewritten = List.fold_left (fun acc (name, contents) ->
					{(contents) with m_kind = handle_kind "" contents contents.m_kind}::acc)
	[] for_rewriting in
      let result =  Search.modules rewritten in
	(*TODO: Second pass: walk through references -- handled during html generation for the moment*)
	(result, all_renamed_modules)


let find_renaming renamings original =
  let rec aux s suffix =
    if String.length s = 0 then
      (
(*	verbose ("Name '"^original^"' remains unchanged");*)
	suffix
      )
    else
      let renaming =
	try  Some (fst(Hashtbl.find renamings s))
	with Not_found -> None
      in match renaming with
	| None ->
	    let father = Name.father s              in
	    let son    = Name.get_relative father s in
	      aux father (concat son suffix)
	| Some r -> (*We have found a substitution, it should be over*)
	    let result = concat r suffix in
(*	      verbose ("We have a renaming of "^s^" to "^r);*)
	      verbose ("Name "^original^" replaced with "^result);
	      result
  in aux original ""


(** {1 Batteries generation}*)

let name_substitutions : (string, string) Hashtbl.t = Hashtbl.create 100

class batlib_generator =
  object(self)
    inherit Odoc_html.html as super
    (*inherit framed_html as super*)

    val mutable renamings : (string, (string*info option)) Hashtbl.t = Hashtbl.create 0

      (** Determine the category of a name*)

    val mutable known_values_names      = Odoc_html.StringSet.empty
    val mutable known_exceptions_names  = Odoc_html.StringSet.empty
    val mutable known_methods_names     = Odoc_html.StringSet.empty
    val mutable known_attributes_names  = Odoc_html.StringSet.empty
    val mutable known_class_types_names = Odoc_html.StringSet.empty
    val mutable known_module_types_names= Odoc_html.StringSet.empty

    method is_value n =
      Odoc_html.StringSet.mem n known_values_names

    method is_exception n =
      Odoc_html.StringSet.mem n known_exceptions_names

    method is_method n =
      Odoc_html.StringSet.mem n known_methods_names

    method is_attribute n =
      Odoc_html.StringSet.mem n known_attributes_names

    method is_class n =
      Odoc_html.StringSet.mem n known_classes_names

    method is_class_type n =
      Odoc_html.StringSet.mem n known_class_types_names

    method is_module n =
      Odoc_html.StringSet.mem n known_modules_names

    method is_module_type n =
      Odoc_html.StringSet.mem n known_modules_names

    method is_type n =
      Odoc_html.StringSet.mem n known_types_names

    method what_is n =
      if self#is_module n           then Some RK_module
      else if self#is_class n       then Some RK_class
      else if self#is_class_type  n then Some RK_class_type
      else if self#is_value       n then Some RK_value
      else if self#is_type        n then Some RK_type
      else if self#is_exception   n then Some RK_exception
      else if self#is_attribute   n then Some RK_attribute
      else if self#is_method      n then Some RK_method
      else if self#is_module_type n then Some RK_module_type
      else None

(**Making links*)
    method make_link ?(target="detailsFrame") ~text ~url () =
      Printf.sprintf "<a href=%S target=%S>%s</a>" url target text


(**Customizing index generation

   Only document modules which may be reached from the root.
*)
	
    method generate_types_index module_list =
      self#generate_elements_index
	((map (fun t -> `Primitive t) primitive_types_names) @
	 (map (fun t -> `Derived   t) self#list_types))
        (function `Derived t        -> t.ty_name
	   |   `Primitive (name, _) -> name)
        (function `Derived t        -> t.ty_info
	   |   `Primitive (_, _)    -> None)
        (function `Derived t        -> Naming.complete_type_target t
	   |   `Primitive (_, alias)-> Naming.complete_target Naming.mark_type alias)
        Odoc_messages.index_of_types
        self#index_types
	

    (** A method to create index files. *)
    method generate_elements_index :
        'a.
        'a list ->
        ('a -> Odoc_info.Name.t) ->
        ('a -> Odoc_info.info option) ->
        ('a -> string) -> string -> string -> unit =
    fun elements name info target title simple_file ->
      try
        let chanout = open_out (Filename.concat !Args.target_dir simple_file) in
        let b = new_buf () in
        bs b "<html>\n";
        self#print_header b (self#inner_title title);
        bs b "<body>\n<center><h1>";
        bs b title;
        bs b "</h1></center>\n" ;
	self#html_of_Index_list b;
        let sorted_elements = List.stable_sort
            (fun e1 e2 -> compare (Name.simple (name e1)) (Name.simple (name e2)))
            elements
        in
        let groups = Odoc_info.create_index_lists sorted_elements (fun e -> Name.simple (name e)) in
        let f_ele e = (*Print one entry*)
          (*let simple_name = Name.simple (name e) in
          let father_name = Name.father (name e) in
          bp b "<tr><td>%s" (self#make_link ~url:(target e) ~text:(self#escape simple_name) ());
          if simple_name <> father_name && father_name <> "" then
            bs b (self#make_link
		    ~url:(fst (Naming.html_files father_name))
		    ~text:father_name ());
          bs b "</td>\n<td>";
          self#html_of_info_first_sentence b (info e);
          bs b "</td></tr>\n";*)
	  let simple_name = Name.simple (name e)
	  and father_name = Name.father (name e) in
	    bp b "<li class='index_entry_entry'>%s%s" (self#make_link ~url:(target e) ~text:(self#escape simple_name) ())
	    (if simple_name <> father_name && father_name <> "" then (*Print container module*)
	      Printf.sprintf " [%s]" (self#make_link ~url:(fst (Naming.html_files father_name)) ~text:father_name ())
	     else "");
	    (self#html_of_info_first_sentence b (info e));
	    bs b  "</li>\n"
        in
        let f_group l = (*Print all entries for a letter*)
          match l with
            [] -> ()
          | e :: _ ->
	      let e' = Name.simple (name e) in
              let s =
		if String.length e' = 0 then
		  begin
		    warning ("I'm not going to find an uppercase letter for "^(name e));
		    ""
		  end
		else
                match (Char.uppercase e'.[0]) with
                    'A'..'Z' as c -> String.make 1 c
                  | _ -> ""
              in
              bs b "<tr><td align=\"left\"><br>";
              bs b s ;
              bs b "</td></tr>\n<tr class='index_entry'><td>\n" ;
	      bs b "<ul class='index_entry'>\n";
              List.iter f_ele l;
	      bs b "</ul>\n</td></tr>"
        in
        bs b "<table class='index_of_elements'>\n";
        List.iter f_group groups ;
        bs b "</table><br>\n" ;
        bs b "</body>\n</html>";
        Buffer.output_buffer chanout b;
        close_out chanout
      with
        Sys_error s ->
          raise (Failure s)
	| _ -> assert false

    method is_reachable_from_root m = true (*List.exists (fun p -> has_parent m ~parent:p) roots*)
    method generate_modules_index _ =
      try
      let list_modules = List.filter (fun m -> self#is_reachable_from_root m.m_name) self#list_modules in
      self#generate_elements_index
        list_modules
        (fun m -> m.m_name)
        (fun m -> m.m_info)
        (fun m -> fst (Naming.html_files m.m_name))
        Odoc_messages.index_of_modules
        self#index_modules
      with _ -> assert false

    method html_of_Module_list b _ =
      try
      let list_modules = List.map (fun m -> m.m_name)
	((List.filter (fun m -> not (List.mem  m.m_name roots) && self#is_reachable_from_root m.m_name) self#list_modules)) in
	super#html_of_Module_list b list_modules
      with _ -> assert false

(**Customizing appearance of modules*)

    method html_of_module b ?(info=true) ?(complete=true) ?(with_link=true) m =
      try
      let name = m.m_name in
      let (html_file, _) = Naming.html_files name in
      let father = Name.father name in
      bs b "<pre>";
      bs b ((self#keyword "module")^" ");
      (
       if with_link then
         bs b (self#make_link ~text:(*(Name.simple name)*)name ~url:html_file ())
       else
         bs b (*(Name.simple name)*)name
      );
      (
       match m.m_kind with
         Module_functor _ when !Odoc_info.Args.html_short_functors  ->
           ()
       | _ -> bs b ": "
      );
      self#html_of_module_kind b father ~modu: m m.m_kind;
      bs b "</pre>";
      if info then
        begin
	verbose ("Printing information of module "^m.m_name^":\n"^(string_of_info_opt m.m_info));
          if complete then
            self#html_of_info ~indent: false
          else
            self#html_of_info_first_sentence
        end b m.m_info
      else
        begin
	  warning ("Module "^m.m_name^" has no associated information")
	end
      with _ -> assert false
	

    method html_of_Ref b name ref_opt =
      let renamed = find_renaming renamings name in
      let type_of_ref =
	match ref_opt with
	  | Some _ -> ref_opt (*We already have all the details*)
	  | _      -> match self#what_is name with
	      | Some _ as r ->
		  warning ("Found the type of "^name);
		  r
	      | None        -> match self#what_is renamed with
		  | Some _ as r ->
		      verbose ("Could not find the type of "^name^", but found that of "^renamed);
		      r
		  | None        ->
		      warning ("Could not find the type of "^name^", even as "^renamed);
		      None
      in
      super#html_of_Ref b renamed type_of_ref


      (**Replace references to [string] with [String.t],
	 [list] with [List.t] etc.

	 Override of [super#create_fully_qualified_idents_links]*)
    method create_fully_qualified_idents_links m_name s =
      try
	(** Replace a complete path with a URL to that path*)
      let handle_qualified_name original_type_name =
	let renamed_type_name  = find_renaming renamings original_type_name in
	  let rel     = Name.get_relative m_name renamed_type_name in
	  let s_final = Odoc_info.apply_if_equal
	    Odoc_info.use_hidden_modules
	    renamed_type_name
	    rel
	  in
	    if self#is_type original_type_name || self#is_type renamed_type_name
	    then self#make_link ~url:(Naming.complete_target Naming.mark_type renamed_type_name)
	      ~text:s_final ()
	    else(
	      if self#is_class original_type_name || self#is_class renamed_type_name then
		let (html_file, _) = Naming.html_files renamed_type_name in
		  self#make_link ~url:html_file ~text:s_final ()
              else s_final)
		(**Replace primitive type names with links to their representation module*)
      in let handle_word str_t =
	let result =
	  let (before,match_s) = (Str.matched_group 1 str_t, Str.matched_group 2 str_t) in
	    try
	      let link = List.assoc match_s primitive_types_names in
		(*let text = before^(end_of_name link) in*)
		before^(self#make_link
			  ~url:(Naming.complete_target Naming.mark_type link)
			  ~text:match_s ())
		  (*(handle_qualified_name link)*)
	    with Not_found -> Str.matched_string str_t
	in result
      in
      let s2 = Str.global_substitute (*Substitute fully qualified names*)
	(Str.regexp "\\([A-Z]\\([a-zA-Z_'0-9]\\)*\\.\\)+\\([a-z][a-zA-Z_'0-9]*\\)")
	(fun str_t -> handle_qualified_name (Str.matched_string str_t))
	s
      in
      let s3 = Str.global_substitute (*Substitute fully qualified names*)
	(Str.regexp "\\([^.a-zA-Z_0-9]\\|^\\)\\([a-zA-Z_0-9]+\\)")
	handle_word
	s2
      in s3
      with _ -> assert false




(*    method html_of_module b ?info ?complete ?with_link m =
      try
	verbose ("Generating html for module "^m.m_name);
	flush_all ();
	super#html_of_module b ?info ?complete ?with_link m
      with _ -> assert false *)

    method index_prefix = "root"
    (** Generate [index.html], as well as [indices.html] for the given module list*)
    method generate_index module_list =
      try
        let title = match !Args.title with None -> "" | Some t -> self#escape t in

	(*[index.html]*)
        let chanout = open_out (Filename.concat !Args.target_dir "index.html") in
        let b = new_buf () in
        (*let title = match !Args.title with None -> "" | Some t -> self#escape t in*)
        bs b doctype ;
        bs b "<html>\n";
        self#print_header b self#title;
	bs b "<frameset cols=\"20%,80%\">\n";
	bs b "<frame src=\"root_modules.html\" name =\"indicesFrame\"/>\n";
	bs b "<frame src=\"root.html\"    name =\"detailsFrame\"/>\n";
	bs b "</frameset>";
	bs b "Frame Alert</h2>\n";
	bs b "<p>\n";
	bs b "This document is designed to be viewed using the frames feature. If you see this message, you are using a non-frame-capable web client.\n";
	bs b "<br>\n";
	bs b "Link to <a href=\"root.html\" target=\"detailsFrame\">Non-frame version.</a></noframes>\n";
	bs b "</html>\n";
        Buffer.output_buffer chanout b;
        close_out chanout;

(*	(*[indices.html]*)
        let chanout = open_out (Filename.concat !Args.target_dir "indices.html") in
        let b = new_buf () in
        bs b doctype ;
        bs b "<html>\n";
        self#print_header b self#title;
        bs b "<body>\n";
        bs b "<center><h1>";
        bs b title;
        bs b "</h1></center>\n" ;
        self#html_of_Index_list b;
        bs b "<br/>";
        self#html_of_Module_list b
          (List.map (fun m -> m.m_name) module_list);
        bs b "</body>\n</html>";
        Buffer.output_buffer chanout b;
        close_out chanout;*)

	(*[root.html]*)
        let chanout = open_out (Filename.concat !Args.target_dir "root.html") in
        let b = new_buf () in
        bs b doctype ;
        bs b "<html>\n";
        self#print_header b self#title;
        bs b "<body>\n";
        bs b "<center><h1>";
        bs b title;
        bs b "</h1></center>\n" ;
        let info = Odoc_info.apply_opt
            (Odoc_info.info_of_comment_file module_list)
            !Odoc_info.Args.intro_file
        in
        (
         match info with
           None -> ()
             (*self#html_of_Index_list b;
             bs b "<br/>";*)
         | Some i -> self#html_of_info ~indent: false b info
        );
          self#html_of_Module_list b
            (List.map (fun m -> m.m_name) module_list);
          bs b "</body>\n</html>";

        Buffer.output_buffer chanout b;
        close_out chanout
      with (*Sys_error s -> raise (Failure s)*)
	| _            -> assert false

    method html_of_Index_list b =
      let item s = bp b "<li class=\"index_of\">%s</li>\n" s in
      let index_if_not_empty l url m =
        match l with
          [] -> ()
        | _ -> item (self#make_link ~text:m ~url ~target:"indicesFrame"())
      in
	bs b "<div class=\"indices\"><ul class=\"indices\">\n";
	item (self#make_link ~url:"index.html"~text:"Home" ~target:"_parent" ());
	index_if_not_empty self#list_types        self#index_types        "Types"(*Odoc_messages.index_of_types*);
	index_if_not_empty self#list_values       self#index_values       "Values" (*Odoc_messages.index_of_values*);
	index_if_not_empty self#list_exceptions   self#index_exceptions   "Exceptions" (*Odoc_messages.index_of_exceptions*);
	index_if_not_empty self#list_classes      self#index_classes      "Classes" (*Odoc_messages.index_of_classes*);
	index_if_not_empty self#list_attributes   self#index_attributes   "Attributes" (*Odoc_messages.index_of_attributes*);
	index_if_not_empty self#list_methods      self#index_methods      "Methods" (*Odoc_messages.index_of_methods*);
	index_if_not_empty self#list_class_types  self#index_class_types  "Class types" (*Odoc_messages.index_of_class_types*);
	index_if_not_empty self#list_modules      self#index_modules      "Modules" (*Odoc_messages.index_of_modules*);
	index_if_not_empty self#list_module_types self#index_module_types "Module types"; (*Odoc_messages.index_of_module_types*)
	bs b "</ul></div><hr />"


    method generate_external_index name mark set =
      let cout = open_out (Filename.concat !Args.target_dir (name ^ ".idex")) in
	Odoc_html.StringSet.iter (fun elt -> Printf.fprintf cout "%S: %S\n" elt (Naming.complete_target mark elt)) set;
	if name = "types" then (*Special case for primitive types*)
	  List.iter (fun (type_name, type_alias) -> Printf.fprintf cout "%S: %S\n"
		     type_name (Naming.complete_target type_alias type_alias)) primitive_types_names;
	close_out cout

    method generate modules =
      try
      match !Odoc_args.dump with
	| Some l ->
	    Odoc_info.verbose "[Internal representation stage, no readable output generated yet]";
	    ()
	| None   ->
	    Odoc_info.verbose "[Final stage, generating html pages]";
	    flush_all ();
	    (*Pre-process every module*)
	    List.iter (fun m -> verbose ("My bag contains "^m.m_name)) modules;
	    let everything        = Search.modules modules in
	    let (rewritten_modules, renamed_modules) = rebuild_structure everything in
	      list_values       <- Odoc_info.Search.values            rewritten_modules ;
	      list_exceptions   <- Odoc_info.Search.exceptions        rewritten_modules ;
	      list_types        <- Odoc_info.Search.types             rewritten_modules ;
	      list_attributes   <- Odoc_info.Search.attributes        rewritten_modules ;
	      list_methods      <- Odoc_info.Search.methods           rewritten_modules ;
	      list_classes      <- Odoc_info.Search.classes           rewritten_modules ;
	      list_class_types  <- Odoc_info.Search.class_types       rewritten_modules ;
	      list_modules      <- Odoc_info.Search.modules           rewritten_modules ;
	      list_module_types <- Odoc_info.Search.module_types      rewritten_modules ;
	      (*Cache set of values*)
	      known_values_names <-
		List.fold_left
		(fun acc t -> Odoc_html.StringSet.add t.val_name acc)
		known_values_names
		list_values ;
	      (*Cache set of exceptions*)
	      known_exceptions_names <-
		List.fold_left
		(fun acc t -> Odoc_html.StringSet.add t.ex_name acc)
		known_exceptions_names
		list_exceptions ;
	      (*Cache set of methods*)
	      known_methods_names <-
		List.fold_left
		(fun acc t -> Odoc_html.StringSet.add t.met_value.val_name acc)
		known_methods_names
		list_methods ;
	      (*Cache set of attributes*)
	      known_attributes_names <-
		List.fold_left
		(fun acc t -> Odoc_html.StringSet.add t.att_value.val_name acc)
		known_attributes_names
		list_attributes ;
	      (*Cache set of class types*)
	      known_class_types_names <-
		List.fold_left
		(fun acc t -> Odoc_html.StringSet.add t.clt_name acc)
		known_class_types_names
		list_class_types ;
	      (*Cache set of module_types *)
	      known_module_types_names <-
		List.fold_left
		(fun acc t -> Odoc_html.StringSet.add t.mt_name acc)
		known_module_types_names
		list_module_types ;
	      (*Proceed to generation*)
	      renamings <- renamed_modules;
	      verbose "Beautification of modules complete, proceeding to generation";
	      flush_all ();
	      super#generate rewritten_modules;
	      (*Generate indices*)
	      self#generate_external_index "types"       Naming.mark_type  known_types_names;
	      self#generate_external_index "values"      Naming.mark_value known_values_names;
	      self#generate_external_index "modules"     "" known_modules_names;
	      self#generate_external_index "classes"     "" known_classes_names;
	      self#generate_external_index "exceptions"  Naming.mark_exception   known_exceptions_names;
	      self#generate_external_index "methods"     Naming.mark_method      known_methods_names;
	      self#generate_external_index "attributes"  Naming.mark_attribute   known_attributes_names;
	      self#generate_external_index "class_types" "" known_class_types_names;
	      self#generate_external_index "module_types""" known_module_types_names

      with e -> Printf.eprintf "%s\n%!" (Printexc.to_string e);
	  assert false

(*    method html_of_custom_tag_developer text =
      verbose ("Generating developer name "^(string_of_text text));
      "<div><span style ='developer'>developer:</span> "^(string_of_text text)^"</span></div>"*)

    initializer
(*      tag_functions         <- ("developer", self#html_of_custom_tag_developer) :: tag_functions;*)
      default_style_options <- default_style_options@
	["li.index_of {display:inline}";
	 "ul.indices  {display:inline;font-variant:small-caps;list-style-position: inside;list-style-type:none;padding:0px}";
         "div.indices {text-align:center}";
	 ".index_entry{font-size:x-small}";
	 "ul.index_entry {list-style-type:none;padding:0px; margin-left:none; text-ident:-1em}";
	 "li.index_entry_entry div.info {margin-left:1em}";
	 "pre {background-color:rgb(250,250,250);margin-top:2em}";
	 "pre.example {margin-top:2px; margin-bottom:2em}";
	 "p {text-align:justify}";
	 ".superscript { font-size : 8pt }"
	];

  end;;

let set_batlib_doc_generator () =
  let doc_generator = ((new batlib_generator) :> Args.doc_generator) in
    Args.set_doc_generator (Some doc_generator)

let _ =
  Odoc_args.verbose := true;
  set_batlib_doc_generator ();
  Args.add_option ("-html", Arg.Unit
		     (fun _ -> Odoc_info.verbose "Deactivating built-in html generator";
			set_batlib_doc_generator())
		     , "<workaround for ocamlbuild adding -html even when we don't want to>") ;
  match !Odoc_args.dump with
    | None -> Odoc_info.verbose "This is the final stage of documentation generation. You should go take a coffee, it's bound to last 15+ minutes."
    | _ -> ()
