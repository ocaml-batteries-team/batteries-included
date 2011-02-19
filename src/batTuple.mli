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

(** Tuples. Modules are provided for tuples with 2, 3, 4, and 5
    elements. Each provides the following categories of functions.

    Projection. Functions [fst], [snd], [thrd], [frth], and [fvth]
    extract a single element. Multiple elements can be extracted. For
    example, {!Tuple3.prj13} returns the first and third elements of a
    3-tuple. All possible combinations are provided. Note there are
    no [prj] functions in Tuple2 because [fst] and [snd] already cover
    all possibilities.

    Mapping. Apply a function to one or all elements of a
    tuple. Functions [map1], [map2], etc. map a given function to the
    first, second, etc. element of a tuple. All elements can be mapped
    using [map] or [mapn]. For example, {!Tuple.Tuple3.mapn f g h}
    will apply [f], [g], and [h] to the three elements, respectively,
    of a 3-tuple. Function [map] is similar but applies the same
    function to all elements, which thus requires the elements to be
    of the same type. Tuples satisfy {!BatInterfaces.Mappable}.

    Currying. Every tuple has a [curry] and [uncurry] function, which
    allow converting between functions that take [n] arguments to ones
    that take a single [n]-tuple argument.

    Enumeration. Every [n]-tuple can be converted to an enum with [n]
    elements using its [enum] function, and can be constructed from an
    enum using [of_enum]. Tuples satisfy {!BatEnum.Enumerable}.

    Printing. Function [printn] prints a tuple given a method for
    printing each of its elements. The simpler [print] function can be
    used when all elements are of the same type.

    Comparison. Every tuple has a [compare] function, which can
    optionally be customized by specifying methods for comparing each
    element. {!Pervasives.compare} is used by default.
*)

(** Pairs of values. Several of the functions here are also exposed in
    {!Pervasives}, as documented below.
    
    @author Edgar Friendly
    @author Ashish Agarwal
*)
module Tuple2 : sig

  type ('a,'b) t = 'a * 'b

  external fst : 'a * 'b -> 'a = "%field0"
      (** Equivalent to {!Pervasives.fst}. *)

  external snd : 'a * 'b -> 'b = "%field1"
      (** Equivalent to {!Pervasives.snd}. *)

  val map : ('a -> 'b) -> ('a * 'a) -> ('b * 'b)

  val mapn : ('a -> 'c) -> ('b -> 'd) -> 'a * 'b -> 'c * 'd
    (** Equivalent to {!BatPervasives.(***)}. *)

  val map1 : ('a -> 'c) -> ('a * 'b) -> ('c * 'b)
    
  val map2 : ('b -> 'c) -> ('a * 'b) -> ('a * 'c)

  val curry : ('a * 'b -> 'c) -> 'a -> 'b -> 'c

  val uncurry : ('a -> 'b -> 'c) -> 'a * 'b -> 'c

  val enum : ('a * 'a) -> 'a BatEnum.t

  val of_enum : 'a BatEnum.t -> ('a * 'a)
    (** Raises [Failure] if enum does not contain at least 2
        elements. *)

  val printn : ('o BatIO.output -> 'a -> unit) -> ('o BatIO.output -> 'b -> unit) -> 'o BatIO.output -> ('a * 'b) -> unit

  val print : ('o BatIO.output -> 'a -> unit) -> 'o BatIO.output -> ('a * 'a) -> unit

  val compare : ?cmp1:('a -> 'a -> int) -> ?cmp2:('b -> 'b -> int) -> ('a * 'b) -> ('a * 'b) -> int

  include BatEnum.Enumerable with type 'a enumerable = 'a * 'a
  include BatInterfaces.Mappable with type 'a mappable = 'a * 'a
end

(** Triples. *)
module Tuple3 : sig

  type ('a,'b,'c) t = 'a * 'b * 'c

  val fst : 'a * 'b * 'c -> 'a

  val snd : 'a * 'b * 'c -> 'b

  val thrd : 'a * 'b * 'c -> 'c

  val prj12 : 'a * 'b * 'c -> 'a * 'b

  val prj13 : 'a * 'b * 'c -> 'a * 'c

  val prj23 : 'a * 'b * 'c -> 'b * 'c

  val map : ('a -> 'b) -> ('a * 'a * 'a) -> ('b * 'b * 'b)

  val mapn : ('a -> 'd) -> ('b -> 'e) -> ('c -> 'f) -> 'a * 'b * 'c -> 'd * 'e * 'f

  val map1 : ('a -> 'd) -> ('a * 'b * 'c) -> ('d * 'b * 'c)

  val map2 : ('b -> 'd) -> ('a * 'b * 'c) -> ('a * 'd * 'c)

  val map3 : ('c -> 'd) -> ('a * 'b * 'c) -> ('a * 'b * 'd)

  val curry : ('a * 'b * 'c -> 'd) -> 'a -> 'b -> 'c -> 'd

  val uncurry : ('a -> 'b -> 'c -> 'd) -> 'a * 'b * 'c -> 'd

  val enum : ('a * 'a * 'a) -> 'a BatEnum.t

  val of_enum : 'a BatEnum.t -> ('a * 'a * 'a)
    (** Raises [Failure] if enum does not contain at least 3
        elements. *)

  val printn : ('o BatIO.output -> 'a -> unit) -> ('o BatIO.output -> 'b -> unit) -> ('o BatIO.output -> 'c -> unit) -> 'o BatIO.output -> ('a * 'b * 'c) -> unit

  val print : ('o BatIO.output -> 'a -> unit) -> 'o BatIO.output -> ('a * 'a * 'a) -> unit

  val compare : ?cmp1:('a -> 'a -> int) -> ?cmp2:('b -> 'b -> int) -> ?cmp3:('c -> 'c -> int) -> ('a * 'b * 'c) -> ('a * 'b * 'c) -> int

  include BatEnum.Enumerable with type 'a enumerable = 'a * 'a * 'a
  include BatInterfaces.Mappable with type 'a mappable = 'a * 'a * 'a
end
