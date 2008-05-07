(*
 * Odoc_generator_batlib - custom documentation generator for Batteries
 * Copyright (C) 2008 Maxence Guesdon
 *                    David Teller
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

let root = ref ""

class batlib_generator =
  object(self)
    inherit Odoc_html.html as html

    method generate modules =
      flush_all ();
      let all_modules = Odoc_info.Search.modules modules in
      let f m =
        match m.m_kind with
          Module_alias a ->
            begin
              match a.ma_module with
                None -> warning "module not resolved in cross-reference stage"
              | Some aliased ->
		  warning "replacing module information";
                  (* replace the info on module by info on the aliased *)
                  let info = match aliased with
                      Mod     m  -> m.m_info
                    | Modtype mt -> mt.mt_info
                  in
                  m.m_info <- info
            end
        | _  -> ()
      in
	List.iter f all_modules(*;
	let chosen_modules =
	  match !root with
	    | None    -> modules
	    | Some nm -> ListLabels.filter ~f:(fun m -> m.m_name = nm) modules in
	  html#generate chosen_modules*)

    method html_of_module b ?(info=true) ?(complete=true) ?(with_link=true) m =
(*      Buffer.add_string b "blablabla";*)
      html#html_of_module b ~info ~complete:true ~with_link m
  end;;

let doc_generator = ((new batlib_generator) :> Args.doc_generator);;



let _ = Args.set_doc_generator (Some doc_generator) in
let _ = Odoc_args.add_option ("root", (Arg.Set_string root), "set an optional root module for the documentation") in
()

;;
