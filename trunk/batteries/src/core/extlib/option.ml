(*
 * Option - functions for the option type
 * Copyright (C) 2003 Nicolas Cannasse
 *               2008 David Teller (Contributor)
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
open Sexplib
open Conv
TYPE_CONV_PATH "Batteries.Data.Persistent.Option" (*For Sexplib, Bin-prot...*)
 
exception No_value

type 'a t = 'a option with sexp

let may f = function
	| None -> ()
	| Some v -> f v

let map f = function
	| None -> None
	| Some v -> Some (f v)

let default v = function
	| None -> v
	| Some v -> v

let is_some = function
	| None -> false
	| _ -> true

let is_none = function
	| None -> true
	| _ -> false

let get_exn s e = match s with
        | None   -> raise e
	| Some v -> v

let get s = get_exn s Not_found

let map_default f v = function
	| None -> v
	| Some v2 -> f v2

let enum = function
        | None   -> Enum.from (fun () -> raise Enum.No_more_elements)
        | Some e -> Enum.singleton e

let of_enum = Enum.get
