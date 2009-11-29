(*
 * Copyright (C) 2009 Jeremie Dimino
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

(** Value printers *)

(** This module defines utilities for printing values.

    @author Jeremie Dimino
*)

type 'a t = bool -> unit BatInnerIO.output -> 'a -> unit
  (** Type of a value-printer, the boolean argument indicates whether
      composed expression must be parenthesized. *)

(** {6 Tuple printers} *)

val print_tuple0 : unit t
val print_tuple1 : 'a1 t -> 'a1 t
val print_tuple2 : 'a1 t -> 'a2 t -> ('a1 * 'a2) t
val print_tuple3 : 'a1 t -> 'a2 t -> 'a3 t -> ('a1 * 'a2 * 'a3) t
val print_tuple4 : 'a1 t -> 'a2 t -> 'a3 t -> 'a4 t -> ('a1 * 'a2 * 'a3 * 'a4) t
val print_tuple5 : 'a1 t -> 'a2 t -> 'a3 t -> 'a4 t -> 'a5 t -> ('a1 * 'a2 * 'a3 * 'a4 * 'a5) t
val print_tuple6 : 'a1 t -> 'a2 t -> 'a3 t -> 'a4 t -> 'a5 t -> 'a6 t -> ('a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6) t
val print_tuple7 : 'a1 t -> 'a2 t -> 'a3 t -> 'a4 t -> 'a5 t -> 'a6 t -> 'a7 t -> ('a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 * 'a7) t
val print_tuple8 : 'a1 t -> 'a2 t -> 'a3 t -> 'a4 t -> 'a5 t -> 'a6 t -> 'a7 t -> 'a8 t -> ('a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 * 'a7 * 'a8) t
val print_tuple9 : 'a1 t -> 'a2 t -> 'a3 t -> 'a4 t -> 'a5 t -> 'a6 t -> 'a7 t -> 'a8 t -> 'a9 t -> ('a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 * 'a7 * 'a8 * 'a9) t
val print_tuple10 : 'a1 t -> 'a2 t -> 'a3 t -> 'a4 t -> 'a5 t -> 'a6 t -> 'a7 t -> 'a8 t -> 'a9 t -> 'a10 t -> ('a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 * 'a7 * 'a8 * 'a9 * 'a10) t
