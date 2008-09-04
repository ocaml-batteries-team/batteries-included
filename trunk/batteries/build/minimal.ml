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
open Odoc_info
module Naming = Odoc_html.Naming
open Odoc_info.Value
open Odoc_info.Module


class bug_generator =
  object(self)
    inherit Odoc_html.html as super

    method generate modules =
      warning "GENERATION STARTED";
      super#generate modules
  end;;

let doc_generator = ((new bug_generator) :> Args.doc_generator);;
let _ = Args.set_doc_generator (Some doc_generator) in
  Odoc_args.add_option ("-force-generator", Arg.Unit (fun _ -> 
							Args.set_doc_generator (Some doc_generator)),
			"Force the use of a generator (bug in OCamlDoc?)")
