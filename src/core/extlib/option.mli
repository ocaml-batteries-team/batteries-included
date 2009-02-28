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

type 'a t = 'a option

val may : ('a -> unit) -> 'a option -> unit
(** [may f (Some x)] calls [f x] and [may f None] does nothing. *)

val map : ('a -> 'b) -> 'a option -> 'b option
(** [map f (Some x)] returns [Some (f x)] and [map None] returns [None]. *)

val default : 'a -> 'a option -> 'a
(** [default x (Some v)] returns [v] and [default x None] returns [x]. *)

val map_default : ('a -> 'b) -> 'b -> 'a option -> 'b
(** [map_default f x (Some v)] returns [f v] and [map_default f x None]
	returns [x]. *)

val is_none : 'a option -> bool
(** [is_none None] returns [true] otherwise it returns [false]. *)

val is_some : 'a option -> bool
(** [is_some (Some x)] returns [true] otherwise it returns [false]. *)

val get : 'a option -> 'a
(** [get (Some x)] returns [x] and [get None] raises [No_value]. *)

val get_exn : 'a option -> exn -> 'a
(** [get_exn (Some x) e] returns [x] and [get_exn None e] raises [e]. *)

val enum: 'a option -> 'a Enum.t
(** [enum (Some x)] returns the singleton [x], while [enum None] returns
    the empty enumeration*)

val of_enum: 'a Enum.t -> 'a option
(** [of_enum e] consumes the first element of [e], if it exists, and 
    returns [Some e]. If [e] is empty, return [None] *)

exception No_value
(** Raised when calling [get None]. *)

(** {6 Boilerplate code}*)
(** {7 S-Expressions}*)

val t_of_sexp : (Sexplib.Sexp.t -> 'a) -> Sexplib.Sexp.t -> 'a t
val sexp_of_t : ('a -> Sexplib.Sexp.t) -> 'a t -> Sexplib.Sexp.t

(** {7 Printing}*)

val print : ('a InnerIO.output -> 'b -> unit) -> 'a InnerIO.output -> 'b t -> unit

(** Operations on options, with labels.*)
module Labels : sig
  val may : f:('a -> unit) -> 'a option -> unit
  val map : f:('a -> 'b) -> 'a option -> 'b option
  val map_default : f:('a -> 'b) -> 'b -> 'a option -> 'b
end
