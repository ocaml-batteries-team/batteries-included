(*
 * Odoc_generator_batlib - custom documentation generator for Batteries
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

(* Incomplete *)


(*From OCamlDoc*)
open Odoc_info;;
module Name   = Odoc_name
open Odoc_info.Value
open Odoc_info.Module
open Odoc_info.Type
open Odoc_info.Class
open Odoc_info.Exception

INCLUDE "../build/odoc_batteries_factored.ml"

(*From the base library*)
open List
open Format

(**
   Apply [f] to the first element of [l] then [g ()] then [f] to the second element of [l],
   then [g ()]...  finally, apply [f] to the last element of [l].

   If [l] contains 0 or 1 element, [g] is never applied.
*)
let interleave f g l =
  match l with
    | []   -> ()
    | [x]  -> f x
    | h::t -> f h;
	List.iter (fun x -> g (); f x) t

let o out   = Format.fprintf out "%s\n"
let p out f = Format.fprintf out f

let get_root l =
  List.find (fun x -> x.m_name = "Batteries") l



class mli_generator = object(self)

  method generate modules =
      match !Odoc_args.dump with
	| Some _ -> assert false
	| None ->
	    verbose "Generation started";
	    let name = Filename.concat !Args.target_dir "generated.mli" in
	    let cout = open_out name                                    in
	    let out  = Format.formatter_of_out_channel cout             in
	      self#handle_root out (get_root (fst (rebuild_structure modules)));
	      flush cout;
	      close_out cout;
	      Printf.eprintf "Output printed to %S\n" name;
	      Odoc_info.verbose (Odoc_messages.file_generated name)


  method handle_root out m =
    self#handle_info_option out m.m_info;
    match m.m_kind with
	Module_struct l -> (*Don't print "struct..end"*)
	  interleave (self#handle_module_element out) (pp_print_newline out) l
      | _ -> assert false (*Normally, the root module should be a structure.*)

  method handle_module_kind out = function
    | Module_struct  l ->
	fprintf out "struct@[<v 2>@\n%a@\n@]end@\n"
	  (fun out ->
	     interleave (self#handle_module_element out) (pp_print_newline out)
	  ) l
    | Module_alias   a -> self#handle_module_alias out a
    | Module_functor (p, k) ->
	fprintf out "functor(%s: %a) ->@\n%a"
	  (Name.simple p.mp_name)
	  self#handle_module_type_kind p.mp_kind
	  self#handle_module_kind k
    | Module_apply (x, y) ->
	fprintf out "%a(%a)"
	  self#handle_module_kind x
	  self#handle_module_kind y
    | Module_with (k, t) ->
	fprintf out "%a with %s"
	  self#handle_module_type_kind k
	  t
    | Module_constraint (_, _) -> assert false

  method handle_info_option out = function
    | None -> ()
    | Some x -> self#handle_info out x

  method handle_info out info =
    fprintf out "(**%s*)@\n" (info_string_of_info info)

  method handle_text_option out = function
    | None -> ()
    | Some x -> self#handle_text out x

  method handle_text out x =
    fprintf out "(**%s*)@\n" (text_string_of_text x)

  method handle_module_element out = function
    | Element_module      x     -> self#handle_module out x
    | Element_module_type x     -> self#handle_module_type out x
    | Element_included_module x -> self#handle_included_module out x
    | Element_class x           -> self#handle_class out x
    | Element_class_type x      -> self#handle_class_type out x
    | Element_value x           -> self#handle_value out x
    | Element_exception x       -> self#handle_exception out x
    | Element_type x            -> self#handle_type out x
    | Element_module_comment x  -> self#handle_module_comment out x

  method handle_module_comment out m =
    self#handle_text out m

  method handle_module_alias out x =
    match x.ma_module with
      | None             -> fprintf out "module alias not found ??\n"
      | Some (Mod m)     -> self#handle_anonymous_module out m
      | Some (Modtype m) -> self#handle_anonymous_module_type out m

  method handle_module out m =
    fprintf out "%amodule %s = %a@\n"
      self#handle_info_option m.m_info
      (Name.simple m.m_name)
      self#handle_module_kind m.m_kind

  method handle_module_type out m =
    fprintf out "%amodule type %s : %a@\n"
      self#handle_info_option m.mt_info
      (Name.simple m.mt_name)
      (fun out m ->
      match m.mt_kind with
	| None   -> fprintf out "module type ??@\n"
	| Some x -> self#handle_module_type_kind out x) m

  method handle_anonymous_module out m =
    fprintf out "%a%a@\n"
      self#handle_info_option m.m_info
      self#handle_module_kind m.m_kind

  method handle_anonymous_module_type out m =
    self#handle_info_option out m.mt_info;
    match m.mt_kind with
      | None   -> fprintf out "module type ??@\n"
      | Some x -> self#handle_module_type_kind out x

  method handle_module_type_kind out = function
    | Module_type_struct l       ->
	fprintf out "sig@[<v 2>%a@]end@\n"
	  (fun out ->
	     interleave (self#handle_module_element out) (pp_print_newline out)
	  ) l
    | Module_type_functor (x,y)     ->
	fprintf out "functor(%s: %a) ->@\n%a"
	  (Name.simple x.mp_name)
	  self#handle_module_type_kind x.mp_kind
	  self#handle_module_type_kind y
    | Module_type_alias x ->
	self#handle_module_type_alias out x
    | Module_type_with (x, y)       ->
	fprintf out "%a with %s"
	  self#handle_module_type_kind x
	  y
	

  method handle_module_type_alias out x =
    match x.mta_module with
      | None -> fprintf out "module type alias ??\n"
      | Some m -> self#handle_module_type out m

  method handle_included_module out x =
    match x.im_module with
      | None             -> fprintf out "include ??\n"
      | Some (Mod m)     -> self#handle_module out m
      | Some (Modtype m) -> self#handle_module_type out m

  method handle_class out x =
    fprintf out "class not implemented yet??\n"

  method handle_class_type out _ =
    fprintf out "class type not implemented yet??\n"

  method handle_value out x =
    fprintf out "%a@[<h>val %s : %a@]@\n"
      self#handle_info_option x.val_info
      (Name.simple x.val_name)
      self#handle_type_expr   x.val_type
      (*x.val_name
      (fun out l ->
	 interleave (self#handle_parameter out) (fun () -> fprintf out " -> ") l
      ) x.val_parameters*)

  method handle_exception out x =
    fprintf out "%a@[<h>exception %s %a @]@\n"
      self#handle_info_option x.ex_info
      (Name.simple x.ex_name)
      self#handle_type_expr_list x.ex_args

  method handle_type out x =
    fprintf out "%a@[<h>type %a%s%a@]@\n"
      self#handle_info_option x.ty_info
      self#handle_type_args   x.ty_parameters
      (Name.simple x.ty_name)
      self#handle_type_kind   x.ty_kind

  method handle_type_arg out = function
    | (x, true, false) -> fprintf out "+%a" self#handle_type_expr x
    | (x, false, true) -> fprintf out "-%a" self#handle_type_expr x
    | (x, false, false)-> self#handle_type_expr out x
    | (x, true, true)  -> fprintf out "type both co and contra variant ??%a" self#handle_type_expr x

  method handle_type_args out = function
    | [] -> ()
    | [x]-> fprintf out "%a" self#handle_type_arg x
    | l  -> fprintf out "(%a)"
	(fun out l ->
	   interleave (self#handle_type_arg out)
	     (fun () -> fprintf out ", ") l) l

  method handle_type_kind out = function
    | Type_abstract  -> ()
    | Type_variant l ->
	fprintf out "@[<v 2>@\n%a@]@\n"
	  (fun out l ->
	     interleave (self#handle_variant_constructor out)
	       (fun () -> fprintf out "@\n|") l
	  ) l
    | Type_record  l ->
	fprintf out "@[<v 2>{%a}@]@\n"
	  (fun out l ->
	     interleave (self#handle_record_field out)
	       (fun () -> fprintf out ";@\n") l
	  ) l

  method handle_variant_constructor out x =
    fprintf out "%s %a%a"
      (Name.simple x.vc_name)
      self#handle_type_expr_list x.vc_args
      self#handle_text_option    x.vc_text

  method handle_type_expr_list out = function
    | [] -> ()
    | l  -> fprintf out " of %a "
	(fun out l ->
	   interleave
	     (self#handle_type_expr out) (fun () -> fprintf out " * ") l
	) l

  method handle_record_field out x =
    fprintf out "%a%s: %a%a"
      self#handle_mutability x.rf_mutable
      (Name.simple x.rf_name)
      self#handle_type_expr x.rf_type
      self#handle_text_option x.rf_text

  method handle_mutability out = function
    | false -> ()
    | true  -> fprintf out "mutable "

  method handle_type_expr out x =
    fprintf out "%s" (string_of_type_expr x)


end;;

warning "Loading batteries.mli generator";;

let generator = (new mli_generator :> Args.doc_generator)

let set_mli_generator () =
    Args.set_doc_generator (Some generator)

let _ =
  Odoc_args.verbose := true;
  set_mli_generator ();
  verbose ("Generator loaded");
  Args.add_option ("-html", Arg.Unit
		     (fun _ -> Odoc_info.verbose "Deactivating built-in html generator";
			set_mli_generator())
		     , "<workaround for ocamlbuild adding -html even when we don't want to>")
