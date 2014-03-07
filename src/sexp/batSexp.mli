(*
 * BatSexp - interface to Sexplib
 * Copyright (C) 2014 Simon Cruanes
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

(** {1 Interface to Sexplib} *)

type t = Sexplib.Sexp.t =
  | Atom of string
  | List of t list

val source : t BatConv.UniversalSource.t
val sink : t BatConv.UniversalSink.t

val into : 'a BatConv.Source.t -> 'a -> t
val from : 'a BatConv.Sink.t -> t -> 'a
val from_opt : 'a BatConv.Sink.t -> t -> 'a option

val to_string : 'a BatConv.Source.t -> 'a -> string
val of_string : 'a BatConv.Sink.t -> string -> 'a option

val sexp_to_string : t -> string
