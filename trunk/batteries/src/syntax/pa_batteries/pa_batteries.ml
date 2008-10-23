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

module Id = struct
  let name = "pa_batteries"
  let version = "0.1"
end


module Make (Syntax : Sig.Camlp4Syntax) = struct
  include Syntax
  open Sig
  open Ast

    let initialized = ref false (**Used to determine whether toplevel has already been initialized.
				   [true] once we have opened [Batteries] and [Standard]*)


    (*We replace the definitions of [implem], [interf] and [top_phrase].*)

    DELETE_RULE Gram implem: "#"; a_LIDENT; opt_expr; semi END
    DELETE_RULE Gram implem: str_item; semi; SELF END
    DELETE_RULE Gram implem: `EOI END

    DELETE_RULE Gram interf: "#"; a_LIDENT; opt_expr; semi END
    DELETE_RULE Gram interf: sig_item; semi; SELF END
    DELETE_RULE Gram interf: `EOI END

    try  (*First attempt: assuming revised syntax*)
      begin
      DELETE_RULE Gram top_phrase: phrase END;
      DELETE_RULE Gram top_phrase: `EOI END;
      EXTEND Gram
      GLOBAL:top_phrase;
      top_phrase_next:
	[ [ ph = phrase -> Some ph;
	  | `EOI        -> None ] ]
      ;
      top_phrase:
	[ [
	    next = top_phrase_next -> if not !initialized then 
	      match next with
		| None    -> None
		| Some ph -> initialized := true;
		    Some <:str_item<open Batteries;;open Standard;;$ph$>>
	    else next
	  ] ];
      END
      end
    with Not_found -> ()



    try  (*First attempt: assuming original syntax*)
      begin
      DELETE_RULE Gram top_phrase: "#"; a_LIDENT; opt_expr; ";;" END;
      DELETE_RULE Gram top_phrase: LIST1 str_item; ";;"END;
      DELETE_RULE Gram top_phrase: `EOI END;
      EXTEND Gram
      GLOBAL:top_phrase;
      top_phrase_next:
      [ [ "#"; n = a_LIDENT; dp = opt_expr; ";;" ->
            Some <:str_item< # $n$ $dp$ >>
        | l = LIST1 str_item; ";;" -> Some (Ast.stSem_of_list l)
        | `EOI -> None
      ] ];
      top_phrase:
	[ [
	    next = top_phrase_next -> if not !initialized then 
	      match next with
		| None    -> None
		| Some ph -> initialized := true;
		    Some <:str_item<open Batteries;;open Standard;;$ph$>>
	    else next
	  ] ];
      END
      end
    with
	Not_found ->  ()


  let stopped_at _loc =
    Some (Loc.move_line 1 _loc)
      
  EXTEND Gram
    GLOBAL:implem interf;
    implem_next:
      [ [ "#"; n = a_LIDENT; dp = opt_expr; semi ->
            ( [ <:str_item< # $n$ $dp$ >> ] , stopped_at _loc)
        | si = str_item; semi; (sil, stopped) = SELF -> (si :: sil, stopped)
        | `EOI -> ([], None)
      ] ];
    interf_next:
      [ [ "#"; n = a_LIDENT; dp = opt_expr; semi ->
            ([ <:sig_item< # $n$ $dp$ >> ], stopped_at _loc)
        | si = sig_item; semi; (sil, stopped) = SELF -> (si :: sil, stopped)
        | `EOI -> ([], None) ] ]
    ;
    implem:
    [ [
	(l,o) = implem_next -> (<:str_item<open Batteries;;open Standard>>::l,o)
      ] ];

    interf:
    [ [
	(l,o) = interf_next -> (<:sig_item<open Batteries;;open Standard>>::l,o)
      ] ];


  END
end

module M = Register.OCamlSyntaxExtension(Id)(Make)


