(*
 * Tuples - functions for tuples
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

    Projection. Functions [first], [second], [third], [fourth], and [fifth]
    extract a single element. Also, multiple elements can be
    extracted. For example, {!Tuple3.prj13} returns the first and
    third elements of a 3-tuple. All possible combinations are
    provided. Note there are no [prj] functions in Tuple2 because
    [first] and [second] already cover all possibilities.

    Mapping. Apply a function to one or all elements of a
    tuple. Functions [map1], [map2], etc. map a given function to the
    first, second, etc. element of a tuple. All elements can be mapped
    using [map] or [mapn]. For example, {!Tuple3.mapn} [f g h]
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

(** Pairs. Some of the functions here are also exposed in
    {!Pervasives}, as documented below.
    
    @author Edgar Friendly
    @author Ashish Agarwal
*)
module Tuple2 : sig

  type ('a,'b) t = 'a * 'b

  external first : 'a * 'b -> 'a = "%field0"
      (** Equivalent to {!Pervasives.fst}. *)

  external second : 'a * 'b -> 'b = "%field1"
      (** Equivalent to {!Pervasives.snd}. *)

  val map : ('a -> 'b) -> ('a * 'a) -> ('b * 'b)

  val mapn : ('a -> 'c) -> ('b -> 'd) -> 'a * 'b -> 'c * 'd
    (** Equivalent to {!BatPervasives.(***)}. *)

  val map1 : ('a -> 'c) -> ('a * 'b) -> ('c * 'b)
    (** Equivalent to {!BatPervasives.first}. *)
    
  val map2 : ('b -> 'c) -> ('a * 'b) -> ('a * 'c)
    (** Equivalent to {!BatPervasives.second}. *)

  val curry : ('a * 'b -> 'c) -> 'a -> 'b -> 'c
  val uncurry : ('a -> 'b -> 'c) -> 'a * 'b -> 'c

  val enum : ('a * 'a) -> 'a BatEnum.t
  val of_enum : 'a BatEnum.t -> ('a * 'a)
    (** @raise Failure if enum does not contain at least 2
        elements. *)

  val printn : ('o BatIO.output -> 'a -> unit) -> ('o BatIO.output -> 'b -> unit) -> 'o BatIO.output -> ('a * 'b) -> unit
  val print : ('o BatIO.output -> 'a -> unit) -> 'o BatIO.output -> ('a * 'a) -> unit

  val compare : ?cmp1:('a -> 'a -> int) -> ?cmp2:('b -> 'b -> int) -> ('a * 'b) -> ('a * 'b) -> int

  include BatEnum.Enumerable with type 'a enumerable = 'a * 'a
  include BatInterfaces.Mappable with type 'a mappable = 'a * 'a
end

(** Triples.

    @author Ashish Agarwal
*)
module Tuple3 : sig

  type ('a,'b,'c) t = 'a * 'b * 'c

  val first : 'a * 'b * 'c -> 'a
  val second : 'a * 'b * 'c -> 'b
  val third : 'a * 'b * 'c -> 'c

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
    (** @raise Failure if enum does not contain at least 3
        elements. *)

  val printn : ('o BatIO.output -> 'a -> unit) -> ('o BatIO.output -> 'b -> unit) -> ('o BatIO.output -> 'c -> unit) -> 'o BatIO.output -> ('a * 'b * 'c) -> unit
  val print : ('o BatIO.output -> 'a -> unit) -> 'o BatIO.output -> ('a * 'a * 'a) -> unit

  val compare : ?cmp1:('a -> 'a -> int) -> ?cmp2:('b -> 'b -> int) -> ?cmp3:('c -> 'c -> int) -> ('a * 'b * 'c) -> ('a * 'b * 'c) -> int

  include BatEnum.Enumerable with type 'a enumerable = 'a * 'a * 'a
  include BatInterfaces.Mappable with type 'a mappable = 'a * 'a * 'a
end

(** 4-Tuples.

    @author Ashish Agarwal
*)
module Tuple4 : sig

  type ('a,'b,'c,'d) t = 'a * 'b * 'c * 'd

  val first : 'a * 'b * 'c * 'd -> 'a
  val second : 'a * 'b * 'c * 'd -> 'b
  val third : 'a * 'b * 'c * 'd -> 'c
  val fourth : 'a * 'b * 'c * 'd -> 'd

  val prj12 : 'a * 'b * 'c * 'd -> 'a * 'b
  val prj13 : 'a * 'b * 'c * 'd -> 'a * 'c
  val prj14 : 'a * 'b * 'c * 'd -> 'a * 'd
  val prj23 : 'a * 'b * 'c * 'd -> 'b * 'c
  val prj24 : 'a * 'b * 'c * 'd -> 'b * 'd
  val prj34 : 'a * 'b * 'c * 'd -> 'c * 'd

  val prj123 : 'a * 'b * 'c * 'd -> 'a * 'b * 'c
  val prj124 : 'a * 'b * 'c * 'd -> 'a * 'b * 'd
  val prj234 : 'a * 'b * 'c * 'd -> 'b * 'c * 'd

  val map : ('a -> 'b) -> ('a * 'a * 'a * 'a) -> ('b * 'b * 'b * 'b)
  val mapn : ('a -> 'e) -> ('b -> 'f) -> ('c -> 'g) -> ('d -> 'h) -> 'a * 'b * 'c * 'd -> 'e * 'f * 'g * 'h
  val map1 : ('a -> 'e) -> ('a * 'b * 'c * 'd) -> ('e * 'b * 'c * 'd)
  val map2 : ('b -> 'e) -> ('a * 'b * 'c * 'd) -> ('a * 'e * 'c * 'd)
  val map3 : ('c -> 'e) -> ('a * 'b * 'c * 'd) -> ('a * 'b * 'e * 'd)
  val map4 : ('d -> 'e) -> ('a * 'b * 'c * 'd) -> ('a * 'b * 'c * 'e)

  val curry : ('a * 'b * 'c * 'd -> 'e) -> 'a -> 'b -> 'c -> 'd -> 'e
  val uncurry : ('a -> 'b -> 'c -> 'd -> 'e) -> 'a * 'b * 'c * 'd -> 'e

  val enum : ('a * 'a * 'a * 'a) -> 'a BatEnum.t
  val of_enum : 'a BatEnum.t -> ('a * 'a * 'a * 'a)
    (** @raise Failure if enum does not contain at least 4
        elements. *)

  val printn : ('o BatIO.output -> 'a -> unit) -> ('o BatIO.output -> 'b -> unit) -> ('o BatIO.output -> 'c -> unit) -> ('o BatIO.output -> 'd -> unit) -> 'o BatIO.output -> ('a * 'b * 'c * 'd) -> unit
  val print : ('o BatIO.output -> 'a -> unit) -> 'o BatIO.output -> ('a * 'a * 'a * 'a) -> unit

  val compare : ?cmp1:('a -> 'a -> int) -> ?cmp2:('b -> 'b -> int) -> ?cmp3:('c -> 'c -> int) -> ?cmp4:('d -> 'd -> int) -> ('a * 'b * 'c * 'd) -> ('a * 'b * 'c * 'd) -> int

  include BatEnum.Enumerable with type 'a enumerable = 'a * 'a * 'a * 'a
  include BatInterfaces.Mappable with type 'a mappable = 'a * 'a * 'a * 'a
end

(** 5-Tuples.

    @author Ashish Agarwal
*)
module Tuple5 : sig

  type ('a,'b,'c,'d,'e) t = 'a * 'b * 'c * 'd * 'e

  val first : 'a * 'b * 'c * 'd * 'e -> 'a
  val second : 'a * 'b * 'c * 'd * 'e -> 'b
  val third : 'a * 'b * 'c * 'd * 'e -> 'c
  val fourth : 'a * 'b * 'c * 'd * 'e -> 'd
  val fifth : 'a * 'b * 'c * 'd * 'e -> 'e

  val prj12 : 'a * 'b * 'c * 'd * 'e -> 'a * 'b
  val prj13 : 'a * 'b * 'c * 'd * 'e -> 'a * 'c
  val prj14 : 'a * 'b * 'c * 'd * 'e -> 'a * 'd
  val prj15 : 'a * 'b * 'c * 'd * 'e -> 'a * 'e
  val prj23 : 'a * 'b * 'c * 'd * 'e -> 'b * 'c
  val prj24 : 'a * 'b * 'c * 'd * 'e -> 'b * 'd
  val prj25 : 'a * 'b * 'c * 'd * 'e -> 'b * 'e
  val prj34 : 'a * 'b * 'c * 'd * 'e -> 'c * 'd
  val prj35 : 'a * 'b * 'c * 'd * 'e -> 'c * 'e
  val prj45 : 'a * 'b * 'c * 'd * 'e -> 'd * 'e

  val prj123 : 'a * 'b * 'c * 'd * 'e -> 'a * 'b * 'c
  val prj124 : 'a * 'b * 'c * 'd * 'e -> 'a * 'b * 'd
  val prj125 : 'a * 'b * 'c * 'd * 'e -> 'a * 'b * 'e
  val prj134 : 'a * 'b * 'c * 'd * 'e -> 'a * 'c * 'd
  val prj135 : 'a * 'b * 'c * 'd * 'e -> 'a * 'c * 'e
  val prj145 : 'a * 'b * 'c * 'd * 'e -> 'a * 'd * 'e
  val prj234 : 'a * 'b * 'c * 'd * 'e -> 'b * 'c * 'd
  val prj235 : 'a * 'b * 'c * 'd * 'e -> 'b * 'c * 'e
  val prj245 : 'a * 'b * 'c * 'd * 'e -> 'b * 'd * 'e
  val prj345 : 'a * 'b * 'c * 'd * 'e -> 'c * 'd * 'e

  val prj1234 : 'a * 'b * 'c * 'd * 'e -> 'a * 'b * 'c * 'd
  val prj1235 : 'a * 'b * 'c * 'd * 'e -> 'a * 'b * 'c * 'e
  val prj1245 : 'a * 'b * 'c * 'd * 'e -> 'a * 'b * 'd * 'e
  val prj1345 : 'a * 'b * 'c * 'd * 'e -> 'a * 'c * 'd * 'e
  val prj2345 : 'a * 'b * 'c * 'd * 'e -> 'b * 'c * 'd * 'e

  val map : ('a -> 'b) -> ('a * 'a * 'a * 'a * 'a) -> ('b * 'b * 'b * 'b * 'b)
  val mapn : ('a -> 'f) -> ('b -> 'g) -> ('c -> 'h) -> ('d -> 'i) -> ('e -> 'j) -> 'a * 'b * 'c * 'd * 'e -> 'f * 'g * 'h * 'i * 'j
  val map1 : ('a -> 'f) -> ('a * 'b * 'c * 'd * 'e) -> ('f * 'b * 'c * 'd * 'e)
  val map2 : ('b -> 'f) -> ('a * 'b * 'c * 'd * 'e) -> ('a * 'f * 'c * 'd * 'e)
  val map3 : ('c -> 'f) -> ('a * 'b * 'c * 'd * 'e) -> ('a * 'b * 'f * 'd * 'e)
  val map4 : ('d -> 'f) -> ('a * 'b * 'c * 'd * 'e) -> ('a * 'b * 'c * 'f * 'e)
  val map5 : ('e -> 'f) -> ('a * 'b * 'c * 'd * 'e) -> ('a * 'b * 'c * 'd * 'f)

  val curry : ('a * 'b * 'c * 'd * 'e -> 'f) -> 'a -> 'b -> 'c -> 'd -> 'e -> 'f
  val uncurry : ('a -> 'b -> 'c -> 'd -> 'e -> 'f) -> 'a * 'b * 'c * 'd * 'e -> 'f

  val enum : ('a * 'a * 'a * 'a * 'a) -> 'a BatEnum.t
  val of_enum : 'a BatEnum.t -> ('a * 'a * 'a * 'a * 'a)
    (** @raise Failure if enum does not contain at least 5
        elements. *)

  val printn : ('o BatIO.output -> 'a -> unit) -> ('o BatIO.output -> 'b -> unit) -> ('o BatIO.output -> 'c -> unit) -> ('o BatIO.output -> 'd -> unit) -> ('o BatIO.output -> 'e -> unit) -> 'o BatIO.output -> ('a * 'b * 'c * 'd * 'e) -> unit
  val print : ('o BatIO.output -> 'a -> unit) -> 'o BatIO.output -> ('a * 'a * 'a * 'a * 'a) -> unit

  val compare : ?cmp1:('a -> 'a -> int) -> ?cmp2:('b -> 'b -> int) -> ?cmp3:('c -> 'c -> int) -> ?cmp4:('d -> 'd -> int) -> ?cmp5:('e -> 'e -> int) -> ('a * 'b * 'c * 'd * 'e) -> ('a * 'b * 'c * 'd * 'e) -> int

  include BatEnum.Enumerable with type 'a enumerable = 'a * 'a * 'a * 'a * 'a
  include BatInterfaces.Mappable with type 'a mappable = 'a * 'a * 'a * 'a * 'a
end
