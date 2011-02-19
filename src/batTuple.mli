(*
 * Pair - functions for pairs of values
 * Copyright (C) 2009 Edgar Friendly
 *               2011 Ashish Agarwal
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

(** Tuples. *)

(** Pairs of values. Several of the functions here are also exposed in
    {!Pervasives}, as documented below.
    
    @author Edgar Friendly
    @author Ashish Agarwal
*)
module Tuple2 : sig

  type ('a,'b) t = 'a * 'b

  external fst : 'a * 'b -> 'a = "%field0"
      (** Project out first element of a pair. Equivalent to
          {!Pervasives.fst}. *)

  external snd : 'a * 'b -> 'b = "%field1"
      (** Project out second element of a pair. Equivalent to
          {!Pervasives.snd}. *)

  val map : ('a -> 'b) -> ('a * 'a) -> ('b * 'b)
    (** Map all values in a pair with the same function.
        [map f (a,b) = (f a, f b)]. *)

  val mapn : ('a -> 'c) -> ('b -> 'd) -> 'a * 'b -> 'c * 'd
    (** Map all values in a pair with different functions.
        [mapn f g (a,b) = (f a, g b)]. Equivalent to
        {!BatPervasives.(***)}. *)

  val map1 : ('a -> 'c) -> ('a * 'b) -> ('c * 'b)
    (** Map first item in a pair. [map1 f (a,b) = (f a, b)]. *)
    
  val map2 : ('b -> 'c) -> ('a * 'b) -> ('a * 'c)
    (** Map second item in a pair. [map2 f (a,b) = (a, f b)]. *)

  val curry : ('a * 'b -> 'c) -> 'a -> 'b -> 'c
    (** Convert a function that accepts a pair of arguments into a
        function that accepts two arguments. Equivalent to
        {!Pervasives.curry}. *)

  val uncurry : ('a -> 'b -> 'c) -> 'a * 'b -> 'c
    (** Convert a function that accepts two arguments into a function
        that accepts a pair of arguments. Equivalent to
        {!Pervasives.uncurry}. *)

  val enum : ('a * 'a) -> 'a BatEnum.t
    (** Build a two-element enum from a pair. *)

  val of_enum : 'a BatEnum.t -> ('a * 'a)
    (** Build a pair out of the first two elements of an enum. Raises
        [Failure] if insufficient elements. *)

  val printn : ('o BatIO.output -> 'a -> unit) -> ('o BatIO.output -> 'b -> unit) -> 'o BatIO.output -> ('a * 'b) -> unit
    (** Print a pair using given printing functions. *)

  val print : ('o BatIO.output -> 'a -> unit) -> 'o BatIO.output -> ('a * 'a) -> unit
    (** Print a pair using given printing function. Like [printn] but
        elements must be of same type. *)

  val compare : ?cmp1:('a -> 'a -> int) -> ?cmp2:('b -> 'b -> int) -> ('a * 'b) -> ('a * 'b) -> int
    (** Compare two pairs in lexicographic order, possibly using
        custom comparators for the two fields. *)

  include BatEnum.Enumerable with type 'a enumerable = 'a * 'a
  include BatInterfaces.Mappable with type 'a mappable = 'a * 'a
end
