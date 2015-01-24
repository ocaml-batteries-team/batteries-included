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

val some : 'a -> 'a option
(** [some x] returns [Some x].

    @since 2.2.0
 *)

val may : ('a -> unit) -> 'a option -> unit
(** [may f (Some x)] calls [f x] and [may f None] does nothing. *)

val map : ('a -> 'b) -> 'a option -> 'b option
(** [map f (Some x)] returns [Some (f x)] and [map f None] returns [None]. *)

val bind : 'a option -> ('a -> 'b option) -> 'b option
(** [bind (Some x) f] returns [f x] and [bind None f] returns [None].

    @example "Our functions return option types. Compose them to propagate [None]."
    {[
      let pick_long case =
        try
          Some (List.find (fun data -> List.length data > 1000) case)
        with Not_found -> None
      let last_null data = List.rindex_of 0 data
      let interesting_positions dataset =
        List.filter_map
          (fun case -> Option.bind last_null (pick_long case))
          dataset
    ]}
*)

val apply : ('a -> 'a) option -> 'a -> 'a
(** [apply None x] returns [x] and [apply (Some f) x] returns [f x] *)

val filter : ('a -> bool) -> 'a option -> 'a option
(** [filter f None] returns [None], [filter f (Some x)] returns [Some x]
    if [f x] is true, and [None] otherwise. *)

val default : 'a -> 'a option -> 'a
(** [default x (Some v)] returns [v] and [default x None] returns [x]. *)

val ( |? ) : 'a option -> 'a -> 'a
(** Like {!default}, with the arguments reversed.
    [None |? 10] returns [10], while [Some "foo" |? "bar"] returns ["foo"].

    {b Note} This operator does not short circuit like [( || )] and [( && )].
    Both arguments will be evaluated.

    @since 2.0 *)

val default_delayed : (unit -> 'a) -> 'a option -> 'a
(** Like {!default}, but the default value is passed as a thunk that
    is only computed if needed.

    @since 2.1 *)

val map_default : ('a -> 'b) -> 'b -> 'a option -> 'b
(** [map_default f x (Some v)] returns [f v] and [map_default f x None]
    returns [x]. *)

val map_default_delayed : ('a -> 'b) -> (unit -> 'b) -> 'a option -> 'b
(** Like {!map_default}, but the default value is passed as a thunk that
    is only computed if needed.

    @since 2.1 *)

val is_none : 'a option -> bool
(** [is_none None] returns [true] otherwise it returns [false]. *)

val is_some : 'a option -> bool
(** [is_some (Some x)] returns [true] otherwise it returns [false]. *)

val get : 'a option -> 'a
(** [get (Some x)] returns [x].
    @raise Invalid_argument on [get None]. *)

val get_exn : 'a option -> exn -> 'a
(** [get_exn (Some x) e] returns [x] and [get_exn None e] raises [e]. *)

val compare : ?cmp:('a -> 'a -> int) -> 'a option -> 'a option -> int
(** Compare two options, possibly using custom comparators for the
    value.  [None] is always assumed to be less than [Some _].  The
    parameter [cmp] defaults to [Pervasives.compare]. *)

val eq : ?eq:('a -> 'a -> bool) -> 'a option -> 'a option -> bool
(** Test for equality between option types, possibly using a custom
    equality predicate.  The parameter [eq] defaults to
    [Pervasives.(=)].

    @since 1.4.0
*)

val enum: 'a option -> 'a BatEnum.t
(** [enum (Some x)] returns the singleton [x], while [enum None] returns
    the empty enumeration. *)

val of_enum: 'a BatEnum.t -> 'a option
(** [of_enum e] consumes the first element of [e], if it exists, and
    returns [Some e]. If [e] is empty, return [None]. *)

(** {6 The Option Monad} *)

(**
    This module provides everything needed to write and execute computations
    in the Option monad.
*)
module Monad : sig
  type 'a m = 'a option
  (** The type of values in this monad : option *)

  val return : 'a -> 'a m
  (** [return x] puts a value in the Option monad, that is, returns [Some x]. *)

  val bind : 'a m -> ('a -> 'b m) -> 'b m
    (** [bind m f] combines the calculation result [m] with the function [f].
        E.g, in the Option monad :
        [bind (Some 1) (fun x -> if x = 1 then Some 4 else None)] returns Some 4. *)
end

(** {6 Boilerplate code}*)

open BatOrd
val ord : 'a ord -> 'a option ord
(** Comparison between optional values
    @since 2.2.0 *)

(** {7 Printing}*)

val print : ('a BatInnerIO.output -> 'b -> unit) -> 'a BatInnerIO.output -> 'b t -> unit

(** Operations on options, with labels.*)
module Labels : sig
  val may : f:('a -> unit) -> 'a option -> unit
  val map : f:('a -> 'b) -> 'a option -> 'b option
  val map_default : f:('a -> 'b) -> 'b -> 'a option -> 'b
end

module Infix : sig
  val ( |? ) : 'a option -> 'a -> 'a
    (** Like {!default}, with the arguments reversed.
        [None |? 10] returns [10], while [Some "foo" |? "bar"] returns ["foo"]. *)

  val ( >>= ): 'a option -> ('a -> 'b option) -> 'b option
  (** as [Monad.bind] *)
end
