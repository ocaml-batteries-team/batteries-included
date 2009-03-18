(* 
 * Batteries_print - Pretty-printers for the toplevel
 * Copyright (C) 2009 David Rajchenbach-Teller, LIFO, Universite d'Orleans
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

open Extlib
open ExtUChar
open ExtUTF8
open ExtString

let print_uchar fmt t =
  Format.fprintf fmt "UChar.of_char '%s'" (UTF8.to_string (UTF8.of_char t))

let print_rope fmt t =
  Format.fprintf fmt "r%S" (Extlib.Rope.to_string t)

let print_ustring fmt t =
  Format.fprintf fmt "u%S" (UTF8.to_string t)

let string_of_cap t = String.Cap.to_string (String.Cap.copy t)

let print_string_cap_rw fmt t =
  Format.fprintf fmt "rw%S" (string_of_cap t)

let print_string_cap_ro fmt t =
  Format.fprintf fmt "ro%S" (string_of_cap t)

open Camlp4_import

(**An inlined version of [Type.type_expr], used to invoke [Toploop.install_printer]*)
module Types =
struct

  (* Type expressions for the core language *)

  type type_expr =
      { mutable desc: type_desc; 
	mutable level: int;
	mutable id: int }
	
  and type_desc =
      Tvar
    | Tarrow of label * type_expr * type_expr * commutable
    | Ttuple of type_expr list
    | Tconstr of Path.t * type_expr list * abbrev_memo ref
    | Tobject of type_expr * (Path.t * type_expr list) option ref
    | Tfield of string * field_kind * type_expr * type_expr
    | Tnil
    | Tlink of type_expr
    | Tsubst of type_expr         (* for copying *)
    | Tvariant of row_desc
    | Tunivar
    | Tpoly of type_expr * type_expr list
	
  and row_desc =
      { row_fields: (label * row_field) list;
	row_more: type_expr;
	row_bound: unit; (* kept for compatibility *)
	row_closed: bool;
	row_fixed: bool;
	row_name: (Path.t * type_expr list) option }
	row_closed: bool;
    row_fixed: bool;
    row_name: (Path.t * type_expr list) option }

 and row_field =
    Rpresent of type_expr option
      | Reither of bool * type_expr list * bool * row_field option ref
          (* 1st true denotes a constant constructor *)
          (* 2nd true denotes a tag in a pattern matching, and
             is erased later *)
      | Rabsent
	  
 and abbrev_memo =
    Mnil
    | Mcons of Path.t * type_expr * type_expr * abbrev_memo
    | Mlink of abbrev_memo ref
	
 and field_kind =
    Fvar of field_kind option ref
      | Fpresent
      | Fabsent
	  
 and commutable =
    Cok
    | Cunknown
    | Clink of commutable ref
	
end

(**An inlined version of [Path.t], used to invoke [Toploop.install_printer]*)
module Path =
struct
  type t =
      Pident of Ident.t
    | Pdot of t * string * int
    | Papply of t * t

  let of_list l =
    let rec aux = function
      | [s] -> Pident s
      | h::t-> Pdot ((aux t), h, -1)
      | _   -> failwith "Path definition error"
    in aux (List.rev l)
      
end



let install_printer (path:Path.t) (typ:Types.type_expr) printer =
  Toploop.install_printer (Obj.magic path) (Obj.magic typ) printer

open Types

let _ = install_printer (Path.of_list "Batteries.Dllist") {desc = -1; id = -1; Tconstr 
