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

(********
TODO: modules by keyword
TODO: values  by keyword
TODO: types   by keyword
etc.
*********)


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

(*open Odoc_batteries_factored*)
INCLUDE "build/odoc_batteries_factored.ml"

 

(** {1 Batteries generation}*)

let name_substitutions : (string, string) Hashtbl.t = Hashtbl.create 100

class batlib_generator =
  object(self)
    inherit Odoc_html.html as super

    val mutable renamings         : (string, (string*info option)) Hashtbl.t = Hashtbl.create 0
    val mutable modules_by_topic  : string -> t_module list                  = fun _ -> assert false
    val mutable list_topics       : string list = []

      (** {2 Determine the category of a name}*)
      
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

(**
   {2 Generation of indices}
*)

(**Generate a list by topic.*)
    method generate_elements_index_by_topic:
      'a.
      topics:(string list)         ->
      elements:(string -> 'a list) ->
      name:('a -> Name.t)          ->
      info:('a -> info option)     ->
      target:('a -> string)        ->
      title:string                 ->
      simple_file:string           -> unit =
      fun ~topics ~elements ~name ~info ~target ~title ~simple_file ->
	let topics = List.sort String.compare topics in(*Actually, let's not sort topics*)
        let chanout = open_out (Filename.concat !Args.target_dir simple_file) in
        let b = new_buf () in
	let each_element e   = 
	  let simple_name = Name.simple (name e)
	  and father_name = Name.father (name e) in
	    bp b "<li class='index_entry_entry'>%s%s" (self#make_link ~url:(target e) ~text:(self#escape simple_name) ())
	      (if simple_name <> father_name && father_name <> "" then (*Print container module*)
		 Printf.sprintf " [%s]" (self#make_link ~url:(fst (Naming.html_files father_name)) ~text:father_name ())
	       else "");
	    (self#html_of_info_first_sentence b (info e));
	    bs b  "</li>\n"  in
	let each_topic topic =
	  match elements topic with [] -> ()
	    | elems ->
		bs b "<tr><td align=\"left\"><br>";
		bs b topic ;
		bs b "</td></tr>\n<tr class='index_entry'><td>\n" ;
		bs b "<ul class='index_entry'>\n";
		List.iter each_element elems;
		bs b "</ul>\n</td></tr>" 
	in
	  try
            bs b "<html>\n";
            self#print_header b (self#inner_title title);
            bs b "<body>\n<center><h1>";
            bs b title;
            bs b "</h1></center>\n" ;
	    self#html_of_Index_list b;
	    List.iter each_topic topics;
	    bs b "</table><br>\n" ;
            bs b "</body>\n</html>";
            Buffer.output_buffer chanout b;
            close_out chanout
	  with
	      Sys_error s -> raise (Failure s)
	    | e -> Printf.eprintf "%s\n%!" (Printexc.to_string e);
		assert false

    (**Generate the list of types.
       In addition to the list of types defined inside modules, we generate
       the list of primitive types.*)
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

(*    method generate_modules_index _ =
      try
      let list_modules = List.filter (fun m -> self#is_reachable_from_root m.m_name) self#list_modules in
      self#generate_elements_index
        list_modules
        (fun m -> m.m_name)
        (fun m -> m.m_info)
        (fun m -> fst (Naming.html_files m.m_name))
        Odoc_messages.index_of_modules
        self#index_modules
      with _ -> assert false*)

    method generate_modules_index _ =
      verbose ("[Index] Here's the list of modules");
      List.iter (fun m -> print_endline m.m_name) list_modules;
      verbose ("[Index] Here's the list of rewritten modules");
      List.iter (fun t -> List.iter (fun m -> print_endline m.m_name) (modules_by_topic t)) list_topics;
      self#generate_elements_index_by_topic
	~topics:list_topics 
	~elements:modules_by_topic
	~name:(fun m -> m.m_name)
	~info:(fun m -> m.m_info)
        ~target:(fun m -> fst (Naming.html_files m.m_name))
        ~title:Odoc_messages.index_of_modules
        ~simple_file:self#index_modules


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
	    Odoc_info.verbose "(you still have time for coffee)";
	    ()
	| None   -> 
	    Odoc_info.verbose "[Final stage, we will generate html pages]";
	    Odoc_info.verbose "(if you don't want coffee, you could also prepare some tea)";
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
		list_module_types;
	      (*Proceed to generation*)
	      renamings <- renamed_modules;
	      let topics = sort_by_topics (*modules*)rewritten_modules in
		modules_by_topic <- (let hash = snd topics in fun x -> try !(Hashtbl.find hash x) with Not_found -> []);
		list_topics      <- fst topics;
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


    initializer
(*      tag_functions <- ("topic", fun _ -> "topic") :: tag_functions;*)
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
		     , "<workaround for ocamlbuild adding -html even when we don't want to>") 
