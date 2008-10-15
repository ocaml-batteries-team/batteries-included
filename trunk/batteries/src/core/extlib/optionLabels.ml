(*
 * Options - functions for the option type
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

(** Functions for the option type.

    Options are an Ocaml standard type that can be either [None] (undefined)
    or [Some x] where x can be any value. Options are widely used in Ocaml
    to represent undefined values (a little like NULL in C, but in a type
    and memory safe way). This module adds some functions for working with
    options.

    @author Nicolas Cannasse
    @author David Teller
*)

open Option

type 'a t = 'a Option.t

let may ~f o = may f o
let map ~f o = map f o
let map_default ~f x o = map_default f x o

let default  = default
let is_none  = is_none
let is_some  = is_some
let get      = get
let get_exn  = get_exn
let enum     = enum
let of_enum  = of_enum
let t_of_sexp= t_of_sexp
let sexp_of_t= sexp_of_t

exception No_value = Option.No_value
