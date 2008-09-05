(*********************************************************************************)
(*                Odoc_fhtml                                                     *)
(*                                                                               *)
(*    Copyright (C) 2004 Institut National de Recherche en Informatique et       *)
(*    en Automatique. All rights reserved.                                       *)
(*                                                                               *)
(*    This program is free software; you can redistribute it and/or modify       *)
(*    it under the terms of the GNU Lesser General Public License as published   *)
(*    by the Free Software Foundation; either version 2.1 of the License, or     *)
(*    any later version.                                                         *)
(*                                                                               *)
(*    This program is distributed in the hope that it will be useful,            *)
(*    but WITHOUT ANY WARRANTY; without even the implied warranty of             *)
(*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *)
(*    GNU Lesser General Public License for more details.                        *)
(*                                                                               *)
(*    You should have received a copy of the GNU Lesser General Public License   *)
(*    along with this program; if not, write to the Free Software                *)
(*    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA                   *)
(*    02111-1307  USA                                                            *)
(*                                                                               *)
(*    Contact: Maxence.Guesdon@inria.fr                                          *)
(*                                                                               *)
(*********************************************************************************)

(* $Id: odoc_fhtml.ml,v 1.1 2004/03/27 19:14:51 guesdon Exp $ *)

(** @ocamldoc_generator Generation of html documentation with frames. 
   @ocamldoc_url odoc_fhtml.ml
   @ocamldoc_compilation [ocamlc -I +ocamldoc -c odoc_fhtml.ml]
   @author Maxence Guesdon

*)

open Odoc_info 
open Parameter
open Value
open Type
open Exception
open Class 
open Module

module Naming = Odoc_html.Naming


(** The optional directory name which contains the classic html doc, to
   add links to this doc. *)
let link_classic = ref None

let new_buf () = Buffer.create 1024
let bp = Printf.bprintf
let bs = Buffer.add_string

(** class for generating a framed html documentation.*)
class framed_html =
  object (self)
    inherit Odoc_html.html as html

    (** the color for type expressions. *)
    val mutable type_color = "#664411"
	
    (** We redefine the init_style method to add some default options to the style,
       before calling html#init_style. *)
    method init_style =
      (* we add a default style option *)
      default_style_options <- 
	default_style_options @
	[
	  ".type { font-weight : bold ; color : "^type_color^" }" ;
	  ".function { font-weight : bold ; font-size : larger ; background-color : #CCDDFF }" ;
	  ".module { font-weight : bold ; font-size : larger ; background-color : #CCDDFF }" ;
	  ".class { font-weight : bold ; font-size : larger ; background-color : #CCDDFF }" ;
	  ".elementname { font-size : larger }" ;
	  ".mutable { font-size : smaller }" ;
	  ".filename { font-size : smaller }" ;
	  ".header { font-size : 22pt ; background-color : #CCCCFF  ; font-weight : bold}" ;
	  ".info { margin-left : 0 ; margin-right : 0 }" ;
	  "div.summarytitle { font-size : 20pt ; text-align: left; padding: 2px; }" ;
	  "div.summarysectiontitle { font-size: 16pt; font-weight: bold; margin-top: 5px; background-color: #CCDDFF; }";
	] ;
      html#init_style


    (** A method to build the header of pages, different from the one of the simple html, 
       because we can't display indexes in the detailsFrame. *)
    method prepare_header module_list =
      let s = "<head>"^style^"<title>" in
      let f b ?(nav=None) ?(comments=[]) t =
	bs b s;
	bs b t;
	bs b "</title>\n</head>\n"
      in
      header <- f

    (** Generate the code for a table of the given list of values,
       in the given buffer.*)
    method generate_values_table b value_list =
      match value_list with
	[] ->
	  ()
      | _ ->
	  bs b "<table border=\"1\" cellpadding=\"3\" cellspacing=\"0\" width=\"100%\">\n";
	  bs b "<TR>\n";
	  bs b "<TD class=\"header\" colspan=2>\n";
	  bs b Odoc_messages.values;
	  bs b "</TD>\n</TR>\n";
	  List.iter
	    (fun v ->
	      Odoc_info.reset_type_names () ;
	      bs b "<TR>\n";
	      bs b "<TD class=\"elementname\" ALIGN=\"right\" VALIGN=\"top\" width=\"1%\">\n" ;
              (* html target *)
	      bp b "<a name=\"%s\"></a>\n" (Naming.value_target v);
	      bs b "<code>";
	      (
	       match v.val_code with 
		 None -> bs b (Name.simple v.val_name)
	       | Some c -> 
		   let file = Naming.file_code_value_complete_target v in
		   self#output_code v.val_name (Filename.concat !Args.target_dir file) c;
		   bp b "<a href=\"%s\">%s</a>" file (Name.simple v.val_name)
	      );
	      bs b "</code></TD>\n";
	      bs b "<TD>";
	      self#html_of_type_expr b (Name.father v.val_name) v.val_type;
	      bs b "<br>\n";
	      self#html_of_info b v.val_info;
	      bs b "</TR>\n"
	    )
	    value_list;
	  bs b "</table><br>\n"

    (** Generate the code for a table of the given list of exceptions,
       in the given buffer.*)
    method generate_exceptions_table b excep_list =
      match excep_list with
	[] ->
	  ()
      | _ ->
	  bs b "<table border=\"1\" cellpadding=\"3\" cellspacing=\"0\" width=\"100%\">\n";
	  bs b "<TR>\n<TD class=\"header\" colspan=2>\n";
	  bs b Odoc_messages.exceptions;
	  bs b "</TD>\n</TR>\n";
	  List.iter
	    (fun e ->
	      Odoc_info.reset_type_names () ;
	      bs b "<TR>\n";
	      bs b "<TD class=\"elementname\" ALIGN=\"right\" VALIGN=\"top\" width=\"1%\">\n";
              (* html target *)
	      bp b "<a name=\"%s\"></a>\n" (Naming.exception_target e);
	      bs b "<code>";
	      bs b (Name.simple e.ex_name);
	      bs b "</code></TD>\n";
	      bs b "<TD>";
	      (
	       match e.ex_args with
		 [] -> ()
	       | _ -> 
		   bs b (self#keyword "of&nbsp;&nbsp;");
		   self#html_of_type_expr_list b (Name.father e.ex_name) " * " e.ex_args;
		   bs b "<br>\n"
	      );
	      self#html_of_info b e.ex_info;
	      bs b " </TD>\n</TR>\n";
	    )
	    excep_list;
	  bs b "</table><br>\n"

    (** Generate the code for a table of the given list of types,
       in the given buffer.*)
    method generate_types_table b type_list =
      match type_list with
	[] -> ()
      | _ ->
	  bs b "<table border=\"1\" cellpadding=\"3\" cellspacing=\"0\" width=\"100%\">\n";
	  bs b "<TR>\n";
	  bs b "<TD class=\"header\" colspan=2>\n";
	  bs b Odoc_messages.types;
	  bs b "</TD>\n</TR>\n";
	  List.iter
	    (fun t ->
	      Odoc_info.reset_type_names () ;
	      let father = Name.father t.ty_name in
	      bs b "<TR>\n<TD class=\"elementname\" ALIGN=\"right\" VALIGN=\"top\" width=\"10%\">\n";
	      (* html mark *)
	      bp b "<a name=\"%s\"></a>\n" (Naming.type_target t);
	      bs b "<code>";
	      self#html_of_type_expr_param_list b father t;
	      (match t.ty_parameters with [] -> () | _ -> bs b " ");
	      bs b (Name.simple t.ty_name);
	      bs b "</code></TD>\n";
	      bs b "<TD>\n";
	      self#html_of_info b t.ty_info;
	      (
	       match t.ty_manifest with
	       | None -> ()
	       | Some type_exp -> 
		   bs b "= ";
		   self#html_of_type_expr b father type_exp;
		   bs b "<br>\n"
	      );
	      (
	       match t.ty_kind with
		 Type_abstract ->
		   if t.ty_manifest = None then bs b Odoc_messages.abstract else ()
	       | Type_variant (l, priv) ->
		   bs b "<table cellpadding=\"3\" cellspacing=\"0\" width=\"100%\">\n";
		   bs b "<TR><TD ALIGN=\"left\" VALIGN=\"top\" width=\"1%\"><code>= ";
		   (if priv then bs b "private" else ());
		   bs b "</code></TD>\n";
		   Odoc_html.print_concat b 
		     "</TR>\n<TR><TD ALIGN=\"left\" VALIGN=\"top\" width=\"1%\"><code> | </code></TD>\n"
		     (fun constr ->
		       bs b "\n<TD ALIGN=\"left\" VALIGN=\"top\" width=\"1%\">\n";
		       bs b "<code>";
		       bs b (self#constructor constr.vc_name);
		       bs b "</code></TD>\n";
		       bs b "<TD ALIGN=\"left\" VALIGN=\"top\" width=\"100%\">\n";
		       (
			match constr.vc_args with
			  [] -> ()
			| l -> 
			    bs b (self#keyword "of&nbsp;&nbsp;");
			    self#html_of_type_expr_list b father " * " constr.vc_args;
			    bs b "<br>\n"
		       );
		       (
			match constr.vc_text with 
			  None -> () 
			| Some d -> 
			    self#html_of_text b d;
			    bs b "<br>\n"
		       )
		     )
		     l;
		   bs b "</TR>\n</table>\n"
			    
	       | Type_record (l, priv) ->
		   bs b "= ";
		   (if priv then bs b "private" else ());
		   bs b "{<br>\n";
		   bs b "<table cellpadding=\"3\" cellspacing=\"0\" width=\"100%\">\n";
		   List.iter
		     (fun r ->
		       bs b "<TR>\n<TD ALIGN=\"right\" VALIGN=\"top\" width=\"1%\">\n";
		       bs b "<code>";
		       bs b r.rf_name;
		       if r.rf_mutable then 
			 (
			  bs b "<br><span class=\"mutable\">(";
			  bs b Odoc_messages.mutab;
			  bs b ")</span>"
			 );
		       bs b "</code></TD>\n<TD>\n:&nbsp;&nbsp;";
		       self#html_of_type_expr b father r.rf_type;
		       bs b " ;";
		       bs b "<br>\n";
		       (
			match r.rf_text with 
			  None -> () 
			| Some d -> 
			    self#html_of_text b d;
			    bs b "<br>\n"
		       )
		       ;
		     )
		     l;
		   bs b "</table>\n";
		   bs b "}";
	      );
	      bs b "</TD>\n</TR>\n";
	    )
	    type_list;
	  bs b "</table><br>\n"
	    
    (** Generate the code for the given function,
       in the given buffer.*)
    method generate_function_code b f =
      Odoc_info.reset_type_names () ;
      bs b "<table border=\"1\" cellpadding=\"3\" cellspacing=\"0\" width=\"100%\">\n";
      bs b "<TR class=\"function\">";
      bs b "<TD><code>\n";
      (* html target *)
      bp b "<a name=\"%s\"></a>\n" (Naming.value_target f);
      (
       match f.val_code with 
	 None -> bs b (Name.simple f.val_name)
       | Some c -> 
	   let file = Naming.file_code_value_complete_target f in
	   self#output_code f.val_name (Filename.concat !Args.target_dir file) c;
	   bp b "<a href=\"%s\">%s</a>" file (Name.simple f.val_name)
      );
      bs b "</code>\n";
      bs b ": ";
      self#html_of_type_expr b (Name.father f.val_name) f.val_type;
      bs b "<br>\n";
      bs b "</TD>\n</TR></table>\n";
      (* description *)
      self#html_of_info b f.val_info;
      (* parameters *)
      (
       if !Args.with_parameter_list then
	 self#html_of_parameter_list b (Name.father f.val_name) f.val_parameters
       else
	 self#html_of_described_parameter_list b (Name.father f.val_name) f.val_parameters
      );
      bs b "<br>\n"

    (** Generate the code for the given list of functions,
       in the given buffer.*)
    method generate_function_list_code b fun_list =
      match fun_list with
	[] -> ()
      | _ ->
	  bs b "<table border=\"1\" cellpadding=\"3\" cellspacing=\"0\" width=\"100%\">\n";
	  bs b "<TR>\n";
	  bs b "<TD class=\"header\" colspan=2>\n";
	  bs b Odoc_messages.functions;
	  bs b "</TD>\n";
	  bs b "</TR></table>\n";
	  bs b "<br>\n";
	  List.iter (self#generate_function_code b) fun_list ;
	  bs b "<br>\n"

    (** Generate the code for the given module,
       in the given buffer.*)
    method generate_module_code b m =
      let (html_file, _) = Naming.html_files m.m_name in
      bs b "<table border=\"1\" cellpadding=\"3\" cellspacing=\"0\" width=\"100%\">\n";
      bs b "<tr class=\"module\">";
      bs b "<td><code>\n";
      bp b "<a href=\"%s\">%s</a> : " html_file (Name.simple m.m_name);
      self#html_of_module_type b (Name.father m.m_name) m.m_type;
      bs b "</code></td></tr></table>\n";
      self#html_of_info_first_sentence b m.m_info;
      bs b "<br>\n"

    (** Generate the code for the given list of modules,
       in the given buffer.*)
    method generate_module_list_code b mod_list =
      match mod_list with
	[] -> ()
      | _ ->
	  bs b "<table border=\"1\" cellpadding=\"3\" cellspacing=\"0\" width=\"100%\">\n";
	  bs b "<tr>\n<td class=\"header\" colspan=2>\n";
	  bs b Odoc_messages.modules;
	  bs b "/";
	  bs b Odoc_messages.functors;
	  bs b "</td></tr>\n</table>\n<br>\n";
	  List.iter (self#generate_module_code b) mod_list ;
	  bs b "<br>\n"

    (** Generate the code for the given module type,
       in the given buffer.*)
    method generate_module_type_code b mt =
      let (html_file, _) = Naming.html_files mt.mt_name in
      bs b "<table border=\"1\" cellpadding=\"3\" cellspacing=\"0\" width=\"100%\">\n";
      bs b "<tr class=\"module\">";
      bs b "<td><code>\n";
      bp b "<a href=\"%s\">%s</a>" html_file (Name.simple mt.mt_name);
      (
       match mt.mt_type with
       | Some t -> 
	   bs b " = ";
	   self#html_of_module_type b (Name.father mt.mt_name) t
       | None  ->  ()
      );
      bs b "</code></td></tr></table>\n";
      self#html_of_info_first_sentence b mt.mt_info;
      bs b "<br>\n"
	
    (** Generate the code for the given list of module types,
       in the given buffer.*)
    method generate_module_type_list_code b modtype_list =
      match modtype_list with
	[] -> ()
      | _ ->
	  bs b "<table border=\"1\" cellpadding=\"3\" cellspacing=\"0\" width=\"100%\">\n";
	  bs b "<TR>\n<TD class=\"header\" colspan=2>\n";
	  bs b Odoc_messages.module_types;
	  bs b "</TD></TR>\n</table>\n";
	  bs b "<br>\n";
	  List.iter (self#generate_module_type_code b) modtype_list ;
	  bs b "<br>\n"

    (** Generate the code for the given class,
       in the given buffer.*)
    method generate_class_code b m_name c =
      let (html_file, _) = Naming.html_files c.cl_name in
      bs b "<table border=\"1\" cellpadding=\"3\" cellspacing=\"0\" width=\"100%\">\n";
      bs b "<tr class=\"class\">";
      bs b "<td>\n";
      self#html_of_class b ~complete: false { c with cl_info = None};
      bs b "</td></tr></table>";
      self#html_of_info_first_sentence b c.cl_info ;
      bs b "<br>\n"

    (** Generate the code for the given list of classes,
       in the given buffer.*)
    method generate_class_list_code b m_name cl_list =
      match cl_list with
	[] -> ()
      | _ ->
	  bs b "<table border=\"1\" cellpadding=\"3\" cellspacing=\"0\" width=\"100%\">\n";
	  bs b "<tr>\n<td class=\"header\" colspan=2>\n";
	  bs b Odoc_messages.classes;
	  bs b "</td></tr>\n</table>\n";
	  bs b "<br>\n";
	  List.iter (self#generate_class_code b m_name) cl_list ;
	  bs b "<br>\n"

    (** Generate the code for the given class type,
       in the given buffer.*)
    method generate_class_type_code b m_name ct =
      let (html_file, _) = Naming.html_files ct.clt_name in
      bs b "<table border=\"1\" cellpadding=\"3\" cellspacing=\"0\" width=\"100%\">\n";
      bs b "<tr class=\"class\">";
      bs b "<td>\n";
      self#html_of_class_type b ~complete: false { ct with clt_info = None};
      bs b "</td></tr></table>";
      self#html_of_info_first_sentence b ct.clt_info ;
      bs b "<br>\n"

    (** Generate the code for the given list of class types,
       in the given buffer.*)
    method generate_class_type_list_code b m_name clt_list =
      match clt_list with
	[] -> ()
      | _ ->
	  bs b "<table border=\"1\" cellpadding=\"3\" cellspacing=\"0\" width=\"100%\">\n";
	  bs b "<tr>\n<td class=\"header\" colspan=2>\n";
	  bs b Odoc_messages.class_types;
	  bs b "</td></tr>\n</table>\n";
	  bs b "<br>\n";
	  List.iter (self#generate_class_type_code b m_name) clt_list ;
	  bs b "<br>\n"

    (** Generate the code for class inheritance of the given class,
       in the given buffer.*)
    method generate_class_inheritance_info b cl = 
      let rec iter_kind k = 
	match k with
	  Class_structure ([], _) ->
	    ()
	| Class_structure (_, _) ->
	    bs b "<table border=\"1\" cellpadding=\"3\" cellspacing=\"0\" width=\"100%\">\n";
	    bs b "<TR>\n";
	    bs b "<TD class=\"header\" colspan=2>\n";
	    bs b Odoc_messages.inheritance;
	    bs b "</TD>\n</TR></table>\n";
	    bs b "<br>\n";
	    (
	     let dag = Odoc_dag2html.create_class_dag [cl] [] in
	     bs b (self#html_of_dag dag)
            );
	    bs b "<br>\n"
	| Class_constraint (k,_) ->
	    iter_kind k
	| Class_apply _ 
	| Class_constr _ ->
	    ()
      in
      iter_kind cl.cl_kind

    (** Generate the code for class inheritance of the given class type,
       in the given buffer.*)
    method generate_class_type_inheritance_info b clt = 
      match clt.clt_kind with
	Class_signature ([], _) ->
	  ()
      | Class_signature (_, _) ->
	  bs b "<table border=\"1\" cellpadding=\"3\" cellspacing=\"0\" width=\"100%\">\n";
	  bs b "<TR>\n";
	  bs b "<TD class=\"header\" colspan=2>\n";
	  bs b Odoc_messages.inheritance;
	  bs b "</TD>\n</TR></table>\n";
	  bs b "<br>\n";
	  (
	   let dag = Odoc_dag2html.create_class_dag [] [clt] in
	   bs b (self#html_of_dag dag)
          );
	  bs b "<br>\n"
      |	Class_type _ ->
	  ()

    (** Generate the code for a table of the given list of attributes,
       in the given buffer.*)
    method generate_attributes_table b att_list =
      match att_list with
	[] -> ()
      | _ ->
	  bs b "<table border=\"1\" cellpadding=\"3\" cellspacing=\"0\" width=\"100%\">\n";
	  bs b "<TR>\n<TD class=\"header\" colspan=2>\n";
	  bs b Odoc_messages.attributes;
	  bs b "</TD>\n</TR>\n";
	  List.iter
	    (fun att ->
	      let module_name = Name.father (Name.father att.att_value.val_name) in
	      bs b "<TR>\n";
	      bs b "<TD class=\"elementname\" ALIGN=\"right\" VALIGN=\"top\" width=\"1%\">\n";
	      bs b "<code>";
	      (
	       match att.att_value.val_code with 
		 None -> bs b (Name.simple att.att_value.val_name)
	       | Some c -> 
		   let file = Naming.file_code_attribute_complete_target att in
		   self#output_code att.att_value.val_name (Filename.concat !Args.target_dir file) c;
		   bp b "<a href=\"%s\">%s</a>" file (Name.simple att.att_value.val_name);
	      );
	      bs b "</code></TD>\n";
	      bs b "<TD>";
	      self#html_of_type_expr b module_name att.att_value.val_type;
	      bs b "<br>\n";
	      self#html_of_info b att.att_value.val_info;
	      bs b "</TR>\n"
	    )
	    att_list;
	  bs b "</table><br>\n"
	    
    (** Generate the code for the given method,
       in the given buffer.*)
    method generate_method_code b m =
      let module_name = Name.father (Name.father m.met_value.val_name) in
      bs b "<table border=\"1\" cellpadding=\"3\" cellspacing=\"0\" width=\"100%\">\n";
      bs b "<TR class=\"function\">\n";
      bs b "<TD><code>\n";
      (* html target *)
      bp b "<a name=\"%s\"></a>\n" (Naming.method_target m);
      (if m.met_private then bs b ((self#keyword "private")^" ") else () );
      (if m.met_virtual then bs b ((self#keyword "virtual")^" ") else () );
      (
       match m.met_value.val_code with 
	 None -> bs b (Name.simple m.met_value.val_name)
       | Some c -> 
	   let file = Naming.file_code_method_complete_target m in
	   self#output_code m.met_value.val_name (Filename.concat !Args.target_dir file) c;
	   bp b "<a href=\"%s\">%s</a>" file (Name.simple m.met_value.val_name)
      );
      bs b "</code>\n";
      bs b ": ";
      self#html_of_type_expr b module_name m.met_value.val_type;
      bs b "<br>\n</TD>\n</TR></table>\n";
      (* description *)
      self#html_of_info b m.met_value.val_info;
      (* parameters *)
      (
       if !Args.with_parameter_list then
	 self#html_of_parameter_list b module_name m.met_value.val_parameters
       else
	 self#html_of_described_parameter_list b module_name m.met_value.val_parameters
      );
      bs b "<br>\n"

    (** Generate the code for the given list of methods,
       in the given buffer.*)
    method generate_method_list_code b met_list =
      match met_list with
	[] ->
	  ()
      | _ ->
	  bs b "<table border=\"1\" cellpadding=\"3\" cellspacing=\"0\" width=\"100%\">\n";
	  bs b "<TR>\n";
	  bs b "<TD class=\"header\" colspan=2>\n";
	  bs b Odoc_messages.methods;
	  bs b "</TD>\n</TR></table>\n<br>\n";
	  List.iter (self#generate_method_code b) met_list

    (** Generate the code of the html page for the given class.*)
    method generate_for_class _ _ cl =
      Odoc_info.reset_type_names () ;
      let (html_file, _) = Naming.html_files cl.cl_name in
      let type_file = Naming.file_type_class_complete_target cl.cl_name in
      try
	let chanout = open_out (Filename.concat !Args.target_dir html_file) in
	let b = new_buf () in
	bs b "<html>\n";
	self#print_header b (self#inner_title cl.cl_name);
	bs b "<body>\n<center><h1>";
	let s = Odoc_messages.clas in
	(
	 match !link_classic with
	   None -> bs b s
	 | Some d ->
	     let classic_file = Filename.concat d (fst (Naming.html_files cl.cl_name)) in
	     bp b "<a href=\"%s\">%s</a>" classic_file s
	);
	bp b " <a href=\"%s\">%s</a>" type_file cl.cl_name;
	bs b "</h1></center>\n";
	bs b "<br>\n";
	self#html_of_class b ~with_link: false cl;
	bs b "<br>\n";

	(* parameters *)
	self#html_of_parameter_list b "" cl.cl_parameters ;
        (* class inheritance *)
	self#generate_class_inheritance_info b cl;    
        (* class attributes *)
	self#generate_attributes_table b (Class.class_attributes ~trans: false cl);
        (* class methods *)
	self#generate_method_list_code b (Class.class_methods ~trans: false cl);

	bs b "</html>";
	Buffer.output_buffer chanout b;
	close_out chanout;

        (* output the file containing the whole class type *)
	self#output_class_type 
	  cl.cl_name
	  (Filename.concat !Args.target_dir type_file)
	  cl.cl_type
      with
	Sys_error s ->
	  raise (Failure s)

    (** Generate the code of the html page for the given class type.*)
    method generate_for_class_type _ _ clt =
      Odoc_info.reset_type_names () ;
      let (html_file, _) = Naming.html_files clt.clt_name in
      let type_file = Naming.file_type_class_complete_target clt.clt_name in
      try
	let chanout = open_out (Filename.concat !Args.target_dir html_file) in
	let b = new_buf () in
	bs b "<html>\n";
	self#print_header b (self#inner_title clt.clt_name);
	bs b "<body>\n<center><h1>";
	let s = Odoc_messages.class_type in
	(
	 match !link_classic with
	   None -> bs b s
	 | Some d ->
	     let classic_file = Filename.concat d (fst (Naming.html_files clt.clt_name)) in
	     bp b "<a href=\"%s\">%s</a>" classic_file s
	);
	bp b " <a href=\"%s\">%s</a>" type_file clt.clt_name;
	bs b "</h1></center>\n";
	bs b "<br>\n";
	self#html_of_class_type b ~with_link: false clt;
	bs b "<br>\n";

        (* class inheritance *)
	self#generate_class_type_inheritance_info b clt;    
        (* class attributes *)
	self#generate_attributes_table b (Class.class_type_attributes ~trans: false clt);
        (* class methods *)
	self#generate_method_list_code b (Class.class_type_methods ~trans: false clt);

	bs b "</html>";
	Buffer.output_buffer chanout b;
	close_out chanout;

	(* output the file containing the whole class type *)
	self#output_class_type 
	  clt.clt_name
	  (Filename.concat !Args.target_dir type_file)
	  clt.clt_type
      with
	Sys_error s ->
	  raise (Failure s)

    (** Generate the code for the main html file of the given module type,
       in the given buffer, and for its submodules and classes. *)
    method generate_module_type_main b mt =
      let type_file = Naming.file_type_module_complete_target mt.mt_name in
      bs b "<html>\n";
      self#print_header b (self#inner_title mt.mt_name);
      bs b "<body>\n<center><h1>";
      (
       match !link_classic with
	 None -> bs b Odoc_messages.module_type
       | Some d ->
	   let classic_file = Filename.concat d (fst (Naming.html_files mt.mt_name)) in
	   bp b "<a href=\"%s\">%s</a>" classic_file Odoc_messages.module_type
      );
      bs b " ";
      (
       match mt.mt_type with
	 Some _ -> bp b "<a href=\"%s\">%s</a>" type_file mt.mt_name
       | None-> bs b mt.mt_name
      );
      bs b "</h1></center>\n<br>\n";
      self#html_of_modtype b ~with_link: false mt;
   
      (* parameters *)
      (
       match Module.module_type_parameters mt with
	 [] -> bs b  "<br>\n"
       | l -> self#html_of_module_parameter_list b "" l
      );
      (* modules/functors *)
      self#generate_module_list_code b (Module.module_type_modules mt);
      (* module types *)
      self#generate_module_type_list_code b (Module.module_type_module_types mt);
      (* classes *)
      self#generate_class_list_code b mt.mt_name (Module.module_type_classes mt);
      (* class types *)
      self#generate_class_type_list_code b mt.mt_name (Module.module_type_class_types mt);
      (* types *)
      self#generate_types_table b (Module.module_type_types mt);
      (* exceptions *)
      self#generate_exceptions_table b (Module.module_type_exceptions mt);
      (* values *)
      self#generate_values_table b (Module.module_type_simple_values mt);
      (* functions *)
      self#generate_function_list_code b (Module.module_type_functions mt);


      bs b "</html>";
      
      (* generate html files for submodules *)
      self#generate_elements self#generate_for_module (Module.module_type_modules mt);
      (* generate html files for module types *)
      self#generate_elements self#generate_for_module_type (Module.module_type_module_types mt);
      (* generate html files for class types *)
      self#generate_elements self#generate_for_class_type (Module.module_type_class_types mt);
      (* generate html files for classes *)
      self#generate_elements self#generate_for_class (Module.module_type_classes mt);

      (* generate the file with the complete module type *)
      (
       match mt.mt_type with
	 None -> ()
       | Some mty -> self#output_module_type 
	     mt.mt_name
	     (Filename.concat !Args.target_dir type_file)
	     mty
      )

    (** Generate the code for the main html file of the given module,
       in the given buffer, and for its submodules and classes. *)
    method generate_module_main b modu =
      let type_file = Naming.file_type_module_complete_target modu.m_name in
      bs b "<html>\n";
      self#print_header b (self#inner_title modu.m_name);
      bs b "<body>\n<center><h1>";
      let s = if Module.module_is_functor modu then Odoc_messages.functo else Odoc_messages.modul in
      (
       match !link_classic with
	 None -> bs b s
       | Some d ->
	   let classic_file = Filename.concat d (fst (Naming.html_files modu.m_name)) in
	   bp b "<a href=\"%s\">%s</a>" classic_file s
      );
      bp b " <a href=\"%s\">%s</a>" type_file modu.m_name;
      bs b "</h1></center>\n";
      bs b "<br>\n";
      self#html_of_module b ~with_link: false modu;

      (* parameters *)
      (
       match Module.module_parameters modu with
	 [] -> bs b  "<br>\n"
       | l -> self#html_of_module_parameter_list b  "" l
      );
      (* modules/functors *)
      self#generate_module_list_code b (Module.module_modules modu);
      (* module types *)
      self#generate_module_type_list_code b (Module.module_module_types modu);
      (* classes *)
      self#generate_class_list_code b modu.m_name (Module.module_classes modu);
      (* class types *)
      self#generate_class_type_list_code b modu.m_name (Module.module_class_types modu);
      (* types *)
      self#generate_types_table b (Module.module_types modu);
      (* exceptions *)
      self#generate_exceptions_table b (Module.module_exceptions modu);
      (* values *)
      self#generate_values_table b (Module.module_simple_values modu);
      (* functions *)
      self#generate_function_list_code b (Module.module_functions modu);

      bs b "</html>";
      
      (* generate html files for submodules *)
      self#generate_elements self#generate_for_module (Module.module_modules modu);
      (* generate html files for modules types *)
      self#generate_elements self#generate_for_module_type (Module.module_module_types modu);
      (* generate html files for class types *)
      self#generate_elements self#generate_for_class_type (Module.module_class_types modu);
      (* generate html files for classes *)
      self#generate_elements self#generate_for_class (Module.module_classes modu);

      (* generate the file with the complete module type *)
      self#output_module_type 
	modu.m_name
	(Filename.concat !Args.target_dir type_file)
	modu.m_type

    (** Generate the code for the html frame file of the given module type,
       in the given buffer. *)
    method generate_module_type_frame b mt =
      let parent_module_name_opt =
	try Some (Filename.chop_extension mt.mt_name)
	with Invalid_argument _ -> None
      in
      let (html_file, _) = Naming.html_files mt.mt_name in
      bs b "<html>\n";
      self#print_header b (self#inner_title mt.mt_name);
      bs b "<body>\n";
      bs b "<code class=\"filename\">";
      bs b mt.mt_file;
      bs b "</code><br>\n<div class=\"summarytitle\">";
      (
       match parent_module_name_opt with
	 None -> ()
       | Some m_name -> 
	   let (_, html_parent) = Naming.html_files m_name in
	   bp b "<a href=\"%s\" target=\"summaryFrame\">%s.</a>\n" html_parent m_name;
      );
      bp b "<a href=\"%s\" target=\"detailsFrame\">%s</a></div>\n"
	html_file
	(Name.simple mt.mt_name);

      (* submodules *)
      (
       match Module.module_type_modules mt with
	 [] -> ()
       | l ->
	   bs b "<div class=\"summarysectiontitle\">";
	   bs b Odoc_messages.modules;
	   bs b "/";
	   bs b Odoc_messages.functors;
	   bs b "</div>\n";
	   List.iter
	     (fun m ->
	       let (html, html_frame) = Naming.html_files m.m_name in
	       bp b "<a href=\"%s\" target=\"summaryFrame\">%s</a><br>\n"
		 html_frame
		 (Name.simple m.m_name);
	     )
	     (List.sort (fun m1 -> fun m2 -> compare (Name.simple m1.m_name) (Name.simple m2.m_name)) l);
      );
        
      (* module types *)
      (
       match Module.module_type_module_types mt with
	 [] -> ()
       | l ->
	   bs b "<div class=\"summarysectiontitle\">";
	   bs b Odoc_messages.module_types;
	   bs b "</div>\n";
	   List.iter
	     (fun mt ->
	       let (html, html_frame) = Naming.html_files mt.mt_name in
	       bp b "<a href=\"%s\" target=\"summaryFrame\">%s</a><br>\n"
		 html_frame
		 (Name.simple mt.mt_name);
	     )
	     (List.sort (fun mt1 -> fun mt2 -> compare (Name.simple mt1.mt_name) (Name.simple mt2.mt_name)) l)
      );

      let f_toc title cpl_list =
	match cpl_list with
	  [] -> ()
	| _ ->
	    bs b "<div class=\"summarysectiontitle\">";
	    bs b title;
	    bs b "</div>\n";
	    List.iter
	      (fun (target, simple_name) ->
		bp b "<a href=\"%s\" target=\"detailsFrame\">%s</a><br>\n"
		  target
		  simple_name;
	      )
	      (List.sort (fun (_,n1) -> fun (_,n2) -> compare n1 n2) cpl_list);
      in
           (* classes *)
      f_toc Odoc_messages.classes 
	 (List.map (fun c -> (fst (Naming.html_files c.cl_name), Name.simple c.cl_name)) (Module.module_type_classes mt));
           (* class types *)
      f_toc Odoc_messages.class_types
	 (List.map (fun ct -> (fst (Naming.html_files ct.clt_name), Name.simple ct.clt_name)) (Module.module_type_class_types mt));
           (* types *)
      f_toc Odoc_messages.types 
	 (List.map (fun t -> (Naming.complete_type_target t, Name.simple t.ty_name)) (Module.module_type_types mt));
           (* exceptions *)
      f_toc Odoc_messages.exceptions 
	 (List.map (fun e -> (Naming.complete_exception_target e, Name.simple e.ex_name)) (Module.module_type_exceptions mt));
           (* values *)
      f_toc Odoc_messages.values 
	 (List.map (fun v -> (Naming.complete_value_target v, Name.simple v.val_name)) (Module.module_type_simple_values mt));
           (* functions *)
      f_toc Odoc_messages.functions 
	 (List.map (fun f -> (Naming.complete_value_target f, Name.simple f.val_name)) (Module.module_type_functions mt));
      
      bs b "</html>"

    (** Generate the code for the html frame file of the given module,
       in the given buffer. *)
    method generate_module_frame b modu =
      let parent_module_name_opt =
	try Some (Filename.chop_extension modu.m_name)
	with Invalid_argument _ -> None
      in
      let (html_file, _) = Naming.html_files modu.m_name in
      bs b "<html>\n";
      self#print_header b (self#inner_title modu.m_name);
      bs b "<body>\n";
      bs b "<code class=\"filename\">";
      bs b modu.m_file;
      bs b "</code><br>\n";
      bs b "<div class=\"summarytitle\">";
      (
       match parent_module_name_opt with
	 None -> ()
       | Some m_name -> 
	   let (_, html_parent) = Naming.html_files m_name in
	   bp b "<a href=\"%s\" target=\"summaryFrame\">%s.</a>\n" html_parent m_name
      );
      bp b "<a href=\"%s\" target=\"detailsFrame\">%s</a></div>\n"
	html_file (Name.simple modu.m_name);
         (* submodules *)
      (
       match Module.module_modules modu with
	 [] -> ()
       | l ->
	   bs b "<div class=\"summarysectiontitle\">";
	   bs b Odoc_messages.modules;
	   bs b "/";
	   bs b Odoc_messages.functors;
	   bs b "</div>\n";
	   List.iter
	     (fun m ->
	       let (html, html_frame) = Naming.html_files m.m_name in
	       bp b "<a href=\"%s\" target=\"summaryFrame\">%s</a><br>\n"
		 html_frame
		 (Name.simple m.m_name);
	     )
	     (List.sort
		(fun m1 -> fun m2 -> compare (Name.simple m1.m_name) (Name.simple m2.m_name)) l)
      );

         (* module types *)
      (
       match Module.module_module_types modu with
	 [] -> ()
       | l ->
	   bs b "<div class=\"summarysectiontitle\">";
	   bs b Odoc_messages.module_types;
	   bs b "</div>\n";
	   List.iter
	     (fun mt ->
	       let (html, html_frame) = Naming.html_files mt.mt_name in
	       bp b "<a href=\"%s\" target=\"summaryFrame\">%s</a><br>\n"
		 html_frame
		 (Name.simple mt.mt_name)
	     )
	     (List.sort 
		(fun mt1 -> fun mt2 -> compare (Name.simple mt1.mt_name) (Name.simple mt2.mt_name)) l)
      );

      let f_toc title cpl_list =
	match cpl_list with
	  [] -> ()
	| _ ->
	    bs b "<div class=\"summarysectiontitle\">";
	    bs b title;
	    bs b "</div>\n";
	    List.iter
	      (fun (target, simple_name) ->
		bp b "<a href=\"%s\" target=\"detailsFrame\">%s</a><br>\n"
		  target simple_name
	      )
	      (List.sort (fun (_,n1) -> fun (_,n2) -> compare n1 n2) cpl_list)
      in
          (* classes *)
      f_toc Odoc_messages.classes 
	(List.map (fun c -> (fst (Naming.html_files c.cl_name), Name.simple c.cl_name)) (Module.module_classes modu));
          (* class types *)
      f_toc Odoc_messages.class_types
	(List.map (fun ct -> (fst (Naming.html_files ct.clt_name), Name.simple ct.clt_name)) (Module.module_class_types modu));
          (* types *)
      f_toc Odoc_messages.types 
	(List.map (fun t -> (Naming.complete_type_target t, Name.simple t.ty_name)) (Module.module_types modu));
          (* exceptions *)
      f_toc Odoc_messages.exceptions 
	(List.map (fun e -> (Naming.complete_exception_target e, Name.simple e.ex_name)) (Module.module_exceptions modu));
          (* values *)
      f_toc Odoc_messages.values 
	(List.map (fun v -> (Naming.complete_value_target v, Name.simple v.val_name)) (Module.module_simple_values modu));
          (* functions *)
      f_toc Odoc_messages.functions 
	(List.map (fun f -> (Naming.complete_value_target f, Name.simple f.val_name)) (Module.module_functions modu));

      bs b "</html>"

    (** Generate the html file for the given module type. 
       @raise Failure if an error occurs.*)
    method generate_for_module_type _ _  mt =
      try
	let (html_file, html_frame_file) = Naming.html_files mt.mt_name in
	let chanout = open_out (Filename.concat !Args.target_dir html_file) in
	let chanout_frame = open_out (Filename.concat !Args.target_dir html_frame_file) in
	let b = new_buf () in
	let b_frame = new_buf () in
	self#generate_module_type_main b mt;
	self#generate_module_type_frame b_frame mt;
	Buffer.output_buffer chanout b;
	Buffer.output_buffer chanout_frame b_frame;
	close_out chanout;
	close_out chanout_frame
      with
	Sys_error s ->
	  raise (Failure s)

    (** Generate the html file for the given module. 
       @raise Failure if an error occurs.*)
    method generate_for_module _ _ modu =
      try
	let (html_file, html_frame_file) = Naming.html_files modu.m_name in
	let chanout = open_out (Filename.concat !Args.target_dir html_file) in
	let chanout_frame = open_out (Filename.concat !Args.target_dir html_frame_file) in
	let b = new_buf () in
	let b_frame = new_buf () in
	self#generate_module_main b modu;
	self#generate_module_frame b_frame modu;
	Buffer.output_buffer chanout b;
	Buffer.output_buffer chanout_frame b_frame;
	close_out chanout;
	close_out chanout_frame
      with
	Sys_error s ->
	  raise (Failure s)

    (** Generate the index.html, modules-frame.html files corresponding
       to the given module list.
       @raise Failure if an error occurs.*)
    method generate_index module_list =
      try
	let chanout = open_out (Filename.concat !Args.target_dir "index.html") in
	let chanout_modules = open_out (Filename.concat !Args.target_dir "modules-frame.html") in
	let b = new_buf () in
	let b_modules = new_buf () in
	let title = 
	  match !Args.title with
	    None -> "" 
	  | Some s -> s
	in
	bs b "<html>\n";
	self#print_header b self#title;
	bs b "<frameset cols=\"20%,80%\">\n";
	bs b "<frameset rows=\"30%,70%\">\n";
	bs b "<frame src=\"modules-frame.html\" name=\"modulesFrame\">\n";
	bs b "<frame src=\"modules-frame.html\" name=\"summaryFrame\">\n";
	bs b "</frameset>\n";
	bs b "<frame src=\"modules-frame.html\" name=\"detailsFrame\">\n";
	bs b "</frameset>\n";
	bs b "<noframes>\n";
	bs b "<h2>\n";
	bs b "Frame Alert</h2>\n";
	bs b "<p>\n";
	bs b "This document is designed to be viewed using the frames feature. If you see this message, you are using a non-frame-capable web client.\n";
	bs b "<br>\n";
	bs b "Link to <a href=\"modules-frame.html\">Non-frame version.</a></noframes>\n";
	bs b "</html>\n";

	let index_if_not_empty l url m =
	  match l with
	    [] -> ()
	  | _ -> bp b_modules "<a href=\"%s\" target=\"detailsFrame\">%s</a><br>\n" url m
	in
	bs b_modules "<html>\n";
	self#print_header b_modules self#title;
	bs b_modules "<body>\n";
	bs b_modules "<h1>";
	bs b_modules title;
	bs b_modules "</h1>\n";
	index_if_not_empty list_types "index_types" Odoc_messages.index_of_types;
	index_if_not_empty list_exceptions "index_exceptions" Odoc_messages.index_of_exceptions;
	index_if_not_empty list_values "index_values" Odoc_messages.index_of_values;
	index_if_not_empty list_attributes "index_attributes" Odoc_messages.index_of_attributes;
	index_if_not_empty list_methods "index_methods" Odoc_messages.index_of_methods;
	index_if_not_empty list_classes "index_classes" Odoc_messages.index_of_classes;
	index_if_not_empty list_class_types "index_class_types" Odoc_messages.index_of_class_types;
	index_if_not_empty list_modules "index_modules" Odoc_messages.index_of_modules;
	index_if_not_empty list_module_types "index_module_types" Odoc_messages.index_of_module_types;
	bs b_modules "<br>\n";
	List.iter
	  (fun m ->
	    let (html, html_frame) = Naming.html_files m.m_name in
	    bp b_modules "<a href=\"%s\" target=\"summaryFrame\">%s</a><br>\n"
	      html_frame m.m_name
	  )
	  module_list;
	bs b_modules "</table>\n</body>\n</html>";
	Buffer.output_buffer chanout b;
	Buffer.output_buffer chanout_modules b_modules;
	close_out chanout;
	close_out chanout_modules
      with
	Sys_error s ->
	  raise (Failure s)

  end

(*
let option_link_classic =
  ("-link-classic", Arg.String (fun d -> link_classic := Some d), 
   "<dir>  add links to the classic html doc in <dir>")

let _ = Args.add_option option_link_classic

let doc_generator = ((new framed_html) :> Args.doc_generator)
let _ = Args.set_doc_generator (Some doc_generator)
*)
