(*
 * Pa_batteries -- Syntax extension for auto-opening Batteries
 * Copyright (C)   2008 David Teller
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

open Camlp4
open PreCast

(*Gasp, this doesn't seem reliable.*)

(*let at_start = ref true

let _ = 
AstFilters.register_str_item_filter
    (Ast.map_str_item (function x -> 
			 if !at_start then
			   let loc = Ast.loc_of_str_item x in
			     at_start := false;
			     <:str_item@loc<open Batteries;; $x$>>			     
		           else x))#str_item*)

open Camlp4

module Id = struct
  let name = "pa_batteries"
  let version = "0.1"
end

let fresh () = Printf.sprintf "OPENIN_%i" (Oo.id (object end))


module Make (Syntax : Sig.Camlp4Syntax) = struct
  include Syntax
  open Sig
  open Ast

    DELETE_RULE Gram implem: "#"; a_LIDENT; opt_expr; semi END
    DELETE_RULE Gram implem: str_item; semi; SELF END
    DELETE_RULE Gram implem: `EOI END

  let stopped_at _loc =
    Some (Loc.move_line 1 _loc)

(*  let implem_next = Gram.Entry.mk "implem_next"*)

  EXTEND Gram
    GLOBAL:implem;
    implem_next:
      [ [ "#"; n = a_LIDENT; dp = opt_expr; semi ->
            ( [ <:str_item< # $n$ $dp$ >> ] , stopped_at _loc)
        | si = str_item; semi; (sil, stopped) = SELF -> (si :: sil, stopped)
        | `EOI -> ([], None)
      ] ];
    implem:
    [ [
	(l,o) = implem_next -> (<:str_item<open Batteries>>::l,o)
      ] ];
  END
end

module M = Register.OCamlSyntaxExtension(Id)(Make)
