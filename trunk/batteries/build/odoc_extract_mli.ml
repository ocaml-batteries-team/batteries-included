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
module Naming = Odoc_html.Naming
module Name   = Odoc_name
open Odoc_info.Value
open Odoc_info.Module
open Odoc_info.Type
open Odoc_info.Class
open Odoc_info.Exception

(*From the base library*)
open List

let o out   = Printf.fprintf out "%s\n"
let p out f = Printf.fprintf out f

class mli_generator = object(self)

  method generate modules =
    List.iter self#generate_for_toplevel_module modules

  method handle_toplevel_module modu =
    if m.m_is_interface then () (*We're only generating stuff for modules which are not read from interfaces.*)
    else (*For the moment, always print to stdout*)
      begin
	o out (info_string_of_info m.m_info);
	handle_module m
      end

  method handle_module out m =
    o out (info_string_of_info m.m_info);

  method handle_module_kind out = function
    | Module_struct l -> List.iter (self#handle_module_element out) l    
    | Module_alias  a -> 
  (*method handle_module out modu =
    
    o out (info_string_of_info m.m_info);
    self#handle_module_type out m.m_type*)

  method handle_module_type_kind out = function
    Module_type_struct tl) -> List.iter2 (self#handle_module_element2 out) kl tl
    | (Module_alias  ka, Module_type_alias  ta) -> 
	begin
	  p out "module %s : %a\n" a.ma_name;
	  match a.mta_module with
	    | None             -> o out "??"
	    | Some mt          -> self#handle_module_type out mt
	end
    | (_, k) -> self#handle_module_type_kind out k

  method handle_module_type_kind out = function
    | Module_type_struct  l -> List.iter (self#handle_module_element out) k
    | Module_type_functor pt tt ->
	p out "functor(%a) -> %a" (self#handle_module_parameter out) pt (self#handle_module_type_kind out) tt
end

let _ =
  Odoc_args.verbose := false;
  Args.set_doc_generator (Some (new mli_generator :> Args.doc_generator))
