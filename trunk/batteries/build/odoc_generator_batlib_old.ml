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
open Odoc_info;;
open Module;;
open List;;

(*let substitutions : (Name.t, ref_kind * t_module) Hashtbl.t =  Hashtbl.create 100*)
let substitutions : (Name.t, ref_kind * Name.t) Hashtbl.t =  Hashtbl.create 100
let substitute a (r,m) =
  verbose ("I will replace references to module "^a^" with references to "^m.m_name);
  Hashtbl.add substitutions a (r,m.m_name)
let get_substitution a =
  try Some (Hashtbl.find substitutions a)
  with Not_found -> None


(** Collect all substitutions.

    We substitute the documentation of module [M] to that of module [N] if
    - either module [M] contains tag [@replace N]; or
    - module [M] includes module [N] and module [N] could not be resolved.
 *)
let compute_substitutions all_modules =
  let rec handle_module m = match m.m_kind with
    | Module_struct l -> iter
	(function 
	   | Element_module      m    -> handle_module m
	   | Element_module_type _    -> ()
	   | Element_included_module x when x.im_module = None ->
	       (*This is [include M] when [M] is a foreign module.     *)
	       (*Consider that every reference to [M] should lead here.*)
	       substitute x.im_name (RK_module, m);
	       (*Mark that module won't need any inlining.             *)
	       
	   | Element_included_module _-> ()
	   | Element_class           _-> ()
	   | Element_class_type      _-> ()
	   | Element_value           _-> ()
	   | Element_exception       _-> ()
	   | Element_type            _-> ()
	   | Element_module_comment  c-> 
	       try  
		 match List.find (function (Custom ("replace", _)) -> true | _ -> false) c with
		   | Custom (_, originals) -> (*Need to replace [original] with [m]*)
		       List.iter (function Ref(x,_) -> substitute x (RK_module, m)
				          | _       -> ())
			 originals
		   | _ -> assert false
	       with Not_found -> ()) l
    | Module_alias   _ -> () (*handled elsewhere while we walk through modules*)
    | Module_functor _ -> () (*handled elsewhere while we walk through modules?*)
    | Module_apply   _ -> () (*handled elsewhere while we walk through modules?*)
    | Module_with    _ -> () (*handled elsewhere while we walk through modules?*)
    | Module_constraint _ -> ()(*handled elsewhere while we walk through modules?*)
  in iter handle_module all_modules

(*let proceed_substitutions all_modules =
  iter (fun m -> *)

(*let reverse_inclusion = ("<reverse inclusion>", [])

(** If we include a module which we can't find, then any reference to that
    module should be replaced by a reference to the including module. 

    Resolved during elaboration stage.*)
let resolve_include_alias m = match m.m_kind with
  | Module_struct s ->
      iter (function
	      | Element_included_module x when x.im_module = None ->
		  (*[include M] when [M] is a foreign module.             *)
		  (*Consider that every reference to [M] should lead here.*)
		  x.im_module <- Some (Mod m);
		  (*Don't document the inclusion                          *)
		  (
		    match x.im_info with
		      | None   -> x.im_info <- 
			  Some {i_desc       = None;
				i_authors    = [];
				i_version    = None;
				i_sees       = [];
				i_since      = None;
				i_deprecated = None;
				i_params     = [];
				i_raised_exceptions = [];
				i_return_value      = None;
				i_custom            = [reverse_inclusion]}
		      | Some i -> x.im_info <-
			  Some {(i) with i_custom = reverse_inclusion::i.i_custom}
		  )
	      | _ -> ()) s
  | _ -> ()*)


(** If we include a module which we can find, then let it be. Otherwise, consider
    that module as an alias to the container module. *)
(*let resolve_inclusions m =
  match m.m_kind with
    | Module_struct s ->
	let rec aux acc = function
	  | [] -> acc
	  | (Element_included_module x)::t ->
	      (
		match x.im_module with
		  | Some (Mod x')     -> 
		      (*[include M] when [M] is a local module.               *)
		      match x'.m_kind with
			| Module_struct s -> 
			| Module_alias  
		      rev_append @ acc
		      m_info
			m_kind
		  | Some (Modtype x') -> 
		      (*[include M] when [M] is a local signature.            *)
		      
		  | None              -> 
		      (*[include M] when [M] is a foreign module.             *)
		      (*Consider that every reference to [M] should lead here.*)
		      x.im_module <- Some (Mod m);
		      (*Don't document the inclusion                          *)
		      aux t
	      )
	  | h::t -> aux h::acc t

	let new_kind =
	  Module_struct(flatten 
			  (filter_map (function 
   
 | Element_included_module x when x.im_module = None -> 
     x.im_module <- Some (Mod m);                       
     None                                               
 | Element_included_module ({im_module = Some (Mod x')} as x)->
     (                                                  (*If possible, replace the inclusion with its contents  *)
     match x'.m_kind with
       | Module_struct s' -> Some s'
       | _                -> None (*for now*)
     )
 | y -> 
     Some [y](*Keep untouched*))  s))
	in m.m_kind <- new_kind
    | _ -> ()
*)

(*let follow_alias m =
  match m.m_kind with
      Module_alias a ->
        begin
          match a.ma_module with
              None         -> warning ("module "^a.ma_name^" not resolved in cross-reference stage")
            | Some aliased ->
		warning "replacing module information";
                (* replace the info on module by info on the aliased *)
                let info = match aliased with
                    Mod     x  -> x.m_info
                  | Modtype x  -> x.mt_info
		in
                  m.m_info <- info
        end
    | _  -> ()*)

class batlib_generator =
  object(self)
    inherit Odoc_html.html as self

    method html_of_Ref b name ref_opt =
      match get_substitution name with
	| Some (opt, name) -> self#html_of_Ref b name (Some opt)
	| None             -> self#html_of_Ref b name ref_opt

    (*method html_of_module_alias*)

    method html_of_included_module b im =
      verbose ("Handling [include "^im.im_name^"]");
      match im.im_module with
	| None   ->    (*Keep default behavior.*)
	    verbose ("Module inclusion of "^im.im_name^" unknown, keeping default behavior");
	    self#html_of_included_module b im
	| Some i ->    (*Let's inline the contents!*)
	    self#html_of_info b im.im_info;
	    match i with
	      | Mod m -> 
		  verbose ("Module inclusion of "^im.im_name^" is a a struct, inlining");
		  self#html_of_module_kind b (Name.father m.m_name) ~modu:m m.m_kind
	      | _     -> 
		  verbose ("Module inclusion of "^im.im_name^" is a signature, keeping default behavior");
		  self#html_of_included_module b im


    method generate modules =
      flush_all ();
      let all_modules = Odoc_info.Search.modules modules in
	compute_substitutions all_modules;

(*	List.iter resolve_include_alias all_modules;*)
(*	List.iter resolve_inclusions all_modules;
	List.iter follow_alias       all_modules;*)
(*

      and inline_include m =
	match m.m_kind with 
	  | Module_struct
	  | Module_functor
	  | Module_apply
	  | Module_with
	  | Module_constraint
      in
	(*Inline each inclusion*)
	List.iter inline_include all_modules;
	(*Replace each module alias*)
	List.iter follow_alias   all_modules;
	(*Continue with substitution list*)
	List.iter substitute     all_modules;*)
	
	self#generate all_modules

    method html_of_module b ?(info=true) ?(complete=true) ?(with_link=true) m =
(*      Buffer.add_string b "blablabla";*)
      self#html_of_module b ~info ~complete:true ~with_link m
  end;;

let doc_generator = ((new batlib_generator) :> Args.doc_generator);;



let _ = Args.set_doc_generator (Some doc_generator) 

(*let _ = Odoc_args.add_option ("-root", (Arg.Set_string root), "set an optional root module for the documentation") in ()*)

;;

