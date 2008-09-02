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

class batlib_generator =
  object(self)
    inherit Odoc_html.html as self

    method html_of_module_comment b text =
      assert false

    method html_of_included_module b im =
      assert false;
      verbose ("Handling [include "^im.im_name^"]");
      match im.im_module with
	| None   ->    (*Keep default behavior.*)
	    warning ("Module inclusion of "^im.im_name^" unknown, keeping default behavior");
	    self#html_of_included_module b im
	| Some i ->    (*Let's inline the contents!*)
	    self#html_of_info b im.im_info;
	    match i with
	      | Mod m -> 
		  warning ("Module inclusion of "^im.im_name^" is a a struct, inlining");
		  self#html_of_module_kind b (Name.father m.m_name) ~modu:m m.m_kind
	      | _     -> 
		  warning ("Module inclusion of "^im.im_name^" is a signature, keeping default behavior");
		  self#html_of_included_module b im


    method generate modules =
      warning "LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL";
      flush_all ();
      assert false;
      let all_modules = Odoc_info.Search.modules modules in
	self#generate all_modules

(*    method html_of_module b ?(info=true) ?(complete=true) ?(with_link=true) m =
      self#html_of_module b ~info ~complete:true ~with_link m*)
  end;;

let doc_generator = ((new batlib_generator) :> Args.doc_generator);;

let _ = Args.set_doc_generator (Some doc_generator) in ()
(*let _ = Odoc_args.add_option ("-root", (Arg.Set_string root), "set an optional root module for the documentation") in ()*)
