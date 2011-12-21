(*From OCamlDoc*)
open Odoc_info;;
open Odoc_info.Value
open Odoc_info.Module
open Odoc_info.Type
open Odoc_info.Class
open Odoc_info.Exception
(*module StringSet = Odoc_html.StringSet*)
module StringSet = Set.Make(String);;

warning "Loading factored";;

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
  [   "char",      "Char.t";
      "string",    "String.t";
      "array",     "Array.t" ;
      "lazy_t",    "Lazy.t";
      "list",      "List.t";
      "option",    "Option.t";
      "int32",     "Int32.t";
      "int64",     "Int64.t";
      "nativeint", "Nativeint.t";
      "big_int",   "Big_int.t";
      "int",       "Int.t";
      "bool",      "Bool.t";
      "unit",      "Unit.t";
      "float",     "Float.t";
      "ref",       "Ref.t";
      "exn",       "Exception.t";
      "format4",   "Printf.format4"
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

(** The list of modules which should appear as roots in the hierarchy. *)
let roots = ["Batteries"]

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
      verbose ("Visiting module "^t.mt_name^" from "^m.m_name^", at path "^path');
      let result =
	{(t) with mt_kind = (match t.mt_kind with
	   | None      -> None
	   | Some kind -> Some (handle_type_kind path' m kind));
	   mt_name = path'} in
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

(**
   Determine into which topics each module/type/value/... goes
*)
let sort_by_topics modules =
  let write s = verbose ( "[SORT] "^s ) in
  let rec string_of_path = function
    | []              -> ""
    | (`Level l)::t   -> Printf.sprintf "[%d] > %s" l (string_of_path t)
    | (`Topic top)::t -> Printf.sprintf "%s > %s" top (string_of_path t)
  in
  (*let write s = Printf.eprintf "[SORT] %s\n%!" s in*)
  let topics           : StringSet.t ref                       = ref StringSet.empty (**The set of topics*)
  and modules_by_topic : (string, t_module list ref) Hashtbl.t = Hashtbl.create 16 (**topic -> set of modules*)
  in
  let add_module top m =
    write ("Adding module "^m.m_name);
    List.iter (function `Topic t ->
		 write ("Adding module "^m.m_name^" to topic "^t);
		 (
		   try
		     let l = Hashtbl.find modules_by_topic t in
		       l := m :: !l
		   with
		       Not_found -> Hashtbl.add modules_by_topic t (ref [m])
		 )
		 | _ -> ()) top
  in
  let push_top_topic l t = (*Push the latest topic on the stack of topics/levels*)
    write ("Adding topic "^t);
    topics := StringSet.add t !topics;
    let result = (`Topic t)::l in
    write ("Added topics from "^(string_of_path l)^" to "^(string_of_path result));
      result
  and push_top_level l t = (*Push the latest level on the stack of topics/levels*)
    write ("Entering level "^(string_of_int t));
    let result = (`Level t)::l in
      write ("Entered level from "^(string_of_path l)^" to "^(string_of_path result));
      result
  and pop_top_to_level l level =
    write ("Removing levels higher than "^(string_of_int level));
    let rec aux prefix = function
      | (`Level l')::t when l' >= level -> aux [] t
      | ((`Topic _ ) as p)::t           -> aux (p::prefix) t
      | _ as t                          -> List.rev_append prefix t
    in
    let result = aux [] l in
      write("From "^(string_of_path l)^" to "^(string_of_path result));
      result
  in
  let adjust_to_level top level =
    write ("Moving to level "^(string_of_int level));
    let result = push_top_level (pop_top_to_level top level) level in
      write("Moved levels from "^(string_of_path top)^" to "^(string_of_path result));
      result
  in
  let adjust_top_from_comment top c =
    fold_left (fun acc text -> match text with
		 | Title  (level, title, text) -> adjust_to_level acc level
		 | Custom (("topic" | "{topic"), text)      ->
		     write ("Custom topic "^(string_of_text text));
		     push_top_topic acc (string_of_text text)
		 | Custom (other, _) ->
		     write ("Custom other "^other);
		     acc
		 | _ -> acc ) top c
  in
  let adjust_top_from_info top = function
    | None                -> top
    | Some ({i_custom = l} as i) ->
	write ("Meeting custom in info "^(string_of_info i));
	List.fold_left (fun acc -> function (("topic"|"{topic"), t) ->
			  write ("Custom topic in info "^(string_of_text t));
			  push_top_topic acc (string_of_text t)
			  |   (other, content)  ->
				write ("Custom other in info "^other^": "^(string_of_text content));
				acc
(*			  |   _ -> acc*)) top l
  in
  let rec handle_kind top = function
    | Module_struct       x  -> List.fold_left handle_module_element top x
    | _                      -> top
  and handle_module_element top = function
    | Element_module x         ->
	let top' = adjust_top_from_info top x.m_info in
	  add_module top' x;
	  ignore (handle_kind top' x.m_kind);
	  top
    | Element_module_comment c ->
	adjust_top_from_comment top c
	(*Extract level (if any) and topics (if any)
	  If level exists
          - pop from [top] until we're at a level strictly higher than the level specified
	  - push the level of [c] and, if available, information
	  If no level exists but information exists
	  - push the information at the current level
	  Otherwise
	  - do nothing
	*)
(*	let (level, topic) = extract_info_from_comment c in
	  (match (level, topic) with
	    | (None, Some x) -> List.fold push_top_topic top x
	    | (Some l, None) -> push_top_level (pop_top_to_level top l) l
	    | (Some l, Some x)->
		List.fold push_top_topic (push_top_level (pop_top_to_level top l) l) x
	    | _ -> top)*)
    | _ -> top (*!TODO: other tables*)
  and handle_module top m = handle_kind (adjust_top_from_info top m.m_info) m.m_kind
  in
  let _ = List.fold_left handle_module [] modules in
    (StringSet.elements !topics, modules_by_topic)

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
