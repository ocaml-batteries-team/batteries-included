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

(** Tuples.

    NOTE: API changes are expected in a future release.

    Modules are provided for tuples with 2, 3, 4, and 5
    elements. Each provides the following categories of functions.

    Projection. Functions [first], [second], [third], [fourth], and [fifth]
    extract a single element. Also, multiple elements can be
    extracted. For example, {!Tuple3.get13} returns the first and
    third elements of a 3-tuple. All possible combinations are
    provided.

    Note there are no [get] functions in Tuple2 because [first] and
    [second] already cover all possibilities. However, [swap] is
    provided, which can be thought of as projecting items in a
    different order.

    Mapping. Apply a function to one or all elements of a
    tuple. Functions [map1], [map2], etc. map a given function to the
    first, second, etc. element of a tuple. All elements can be mapped
    using [map] or [mapn]. For example, {!Tuple3.map} [f g h] will
    apply [f], [g], and [h] to the three elements, respectively, of a
    3-tuple. Function [mapn] is similar but applies the same function
    to all elements, which thus requires the elements to be of the
    same type.

    Currying. Every tuple has a [curry] and [uncurry] function, which
    allow converting between functions that take [n] arguments to ones
    that take a single [n]-tuple argument.

    Enumeration. Every [n]-tuple can be converted to an enum with [n]
    elements using its [enum] function, and can be constructed from an
    enum using [of_enum]. Tuples satisfy {!BatEnum.Enumerable}.

    Printing. Function [print] prints a tuple given a method for
    printing each of its elements. The simpler [printn] function can
    be used when all elements are of the same type.

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

  val swap : ('a * 'b) -> ('b * 'a)

  val map : ('a -> 'c) -> ('b -> 'd) -> 'a * 'b -> 'c * 'd
  (** Equivalent to {!BatPervasives.(***)}. *)

  val mapn : ('a -> 'b) -> ('a * 'a) -> ('b * 'b)
  (** Like {!map} but specialized for tuples with elements of the
      same type.

      [mapn f] is equivalent to [map f f].
  *)

  val map1 : ('a -> 'c) -> ('a * 'b) -> ('c * 'b)
  (** [map1 f (x,y)] returns (f x,y) *)

  val map2 : ('b -> 'c) -> ('a * 'b) -> ('a * 'c)
  (** [map2 f (x,y)] returns (x,f y) *)

  val curry : ('a * 'b -> 'c) -> 'a -> 'b -> 'c
  val uncurry : ('a -> 'b -> 'c) -> 'a * 'b -> 'c

  val enum : ('a * 'a) -> 'a BatEnum.t
  val of_enum : 'a BatEnum.t -> ('a * 'a)
  (** @raise Failure if enum does not contain at least 2
      elements. *)

  val print : ?first:string -> ?sep:string -> ?last:string
    -> ('o BatIO.output -> 'a -> unit) -> ('o BatIO.output -> 'b -> unit)
    -> 'o BatIO.output -> ('a * 'b) -> unit

  val printn : ?first:string -> ?sep:string -> ?last:string
    -> ('o BatIO.output -> 'a -> unit)
    -> 'o BatIO.output -> ('a * 'a) -> unit

  val compare : ?cmp1:('a -> 'a -> int) -> ?cmp2:('b -> 'b -> int) -> ('a * 'b) -> ('a * 'b) -> int

  include BatEnum.Enumerable with type 'a enumerable = 'a * 'a

  open BatOrd
  val eq : 'a eq -> 'b eq -> ('a * 'b) eq
  val ord : 'a ord -> 'b ord -> ('a * 'b) ord
  val comp : 'a comp -> 'b comp -> ('a * 'b) comp
  module Eq (T1 : Eq) (T2 : Eq) : Eq with type t = T1.t * T2.t
  module Ord (T1 : Ord) (T2 : Ord) : Ord with type t = T1.t * T2.t
  module Comp (T1 : Comp) (T2 : Comp) : Comp with type t = T1.t * T2.t
end

(** Triples.

    @author Ashish Agarwal
*)
module Tuple3 : sig

  type ('a,'b,'c) t = 'a * 'b * 'c

  val first : 'a * 'b * 'c -> 'a
  val second : 'a * 'b * 'c -> 'b
  val third : 'a * 'b * 'c -> 'c

  val get12 : 'a * 'b * 'c -> 'a * 'b
  val get13 : 'a * 'b * 'c -> 'a * 'c
  val get23 : 'a * 'b * 'c -> 'b * 'c

  val map : ('a -> 'd) -> ('b -> 'e) -> ('c -> 'f) -> 'a * 'b * 'c -> 'd * 'e * 'f

  val mapn : ('a -> 'b) -> ('a * 'a * 'a) -> ('b * 'b * 'b)
  (** Like {!map} but specialized for tuples with elements of the
      same type.

      [mapn f] is equivalent to [map f f f].
  *)

  val map1 : ('a -> 'd) -> ('a * 'b * 'c) -> ('d * 'b * 'c)
  val map2 : ('b -> 'd) -> ('a * 'b * 'c) -> ('a * 'd * 'c)
  val map3 : ('c -> 'd) -> ('a * 'b * 'c) -> ('a * 'b * 'd)

  val curry : ('a * 'b * 'c -> 'd) -> 'a -> 'b -> 'c -> 'd
  val uncurry : ('a -> 'b -> 'c -> 'd) -> 'a * 'b * 'c -> 'd

  val enum : ('a * 'a * 'a) -> 'a BatEnum.t
  val of_enum : 'a BatEnum.t -> ('a * 'a * 'a)
  (** @raise Failure if enum does not contain at least 3
      elements. *)

  val print : ?first:string -> ?sep:string -> ?last:string
    -> ('o BatIO.output -> 'a -> unit) -> ('o BatIO.output -> 'b -> unit) -> ('o BatIO.output -> 'c -> unit)
    -> 'o BatIO.output -> ('a * 'b * 'c) -> unit

  val printn : ?first:string -> ?sep:string -> ?last:string
    -> ('o BatIO.output -> 'a -> unit)
    -> 'o BatIO.output -> ('a * 'a * 'a) -> unit

  val compare : ?cmp1:('a -> 'a -> int) -> ?cmp2:('b -> 'b -> int) -> ?cmp3:('c -> 'c -> int) -> ('a * 'b * 'c) -> ('a * 'b * 'c) -> int

  include BatEnum.Enumerable with type 'a enumerable = 'a * 'a * 'a

  open BatOrd
  val eq : 'a eq -> 'b eq -> 'c eq -> ('a * 'b * 'c) eq
  val ord : 'a ord -> 'b ord -> 'c ord -> ('a * 'b * 'c) ord
  val comp : 'a comp -> 'b comp -> 'c comp -> ('a * 'b * 'c) comp
  module Eq (T1 : Eq) (T2 : Eq) (T3 : Eq)
    : Eq with type t = T1.t * T2.t * T3.t
  module Ord (T1 : Ord) (T2 : Ord) (T3 : Ord)
    : Ord with type t = T1.t * T2.t * T3.t
  module Comp (T1 : Comp) (T2 : Comp) (T3 : Comp)
    : Comp with type t = T1.t * T2.t * T3.t
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

  val get12 : 'a * 'b * 'c * 'd -> 'a * 'b
  val get13 : 'a * 'b * 'c * 'd -> 'a * 'c
  val get14 : 'a * 'b * 'c * 'd -> 'a * 'd
  val get23 : 'a * 'b * 'c * 'd -> 'b * 'c
  val get24 : 'a * 'b * 'c * 'd -> 'b * 'd
  val get34 : 'a * 'b * 'c * 'd -> 'c * 'd

  val get123 : 'a * 'b * 'c * 'd -> 'a * 'b * 'c
  val get124 : 'a * 'b * 'c * 'd -> 'a * 'b * 'd
  val get234 : 'a * 'b * 'c * 'd -> 'b * 'c * 'd

  val map : ('a -> 'e) -> ('b -> 'f) -> ('c -> 'g) -> ('d -> 'h) -> 'a * 'b * 'c * 'd -> 'e * 'f * 'g * 'h

  val mapn : ('a -> 'b) -> ('a * 'a * 'a * 'a) -> ('b * 'b * 'b * 'b)
  (** Like {!map} but specialized for tuples with elements of the
      same type.

      [mapn f] is equivalent to [map f f f f].
  *)

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

  val print : ?first:string -> ?sep:string -> ?last:string
    -> ('o BatIO.output -> 'a -> unit) -> ('o BatIO.output -> 'b -> unit) -> ('o BatIO.output -> 'c -> unit) -> ('o BatIO.output -> 'd -> unit)
    -> 'o BatIO.output -> ('a * 'b * 'c * 'd) -> unit

  val printn : ?first:string -> ?sep:string -> ?last:string
    -> ('o BatIO.output -> 'a -> unit)
    -> 'o BatIO.output -> ('a * 'a * 'a * 'a) -> unit

  val compare : ?cmp1:('a -> 'a -> int) -> ?cmp2:('b -> 'b -> int) -> ?cmp3:('c -> 'c -> int) -> ?cmp4:('d -> 'd -> int) -> ('a * 'b * 'c * 'd) -> ('a * 'b * 'c * 'd) -> int

  include BatEnum.Enumerable with type 'a enumerable = 'a * 'a * 'a * 'a

  open BatOrd
  val eq : 'a eq -> 'b eq -> 'c eq -> 'd eq -> ('a * 'b * 'c * 'd) eq
  val ord : 'a ord -> 'b ord -> 'c ord -> 'd ord -> ('a * 'b * 'c * 'd) ord
  val comp : 'a comp -> 'b comp -> 'c comp -> 'd comp -> ('a * 'b * 'c * 'd) comp
  module Eq (T1 : Eq) (T2 : Eq) (T3 : Eq) (T4 : Eq)
    : Eq with type t = T1.t * T2.t * T3.t * T4.t
  module Ord (T1 : Ord) (T2 : Ord) (T3 : Ord) (T4 : Ord)
    : Ord with type t = T1.t * T2.t * T3.t * T4.t
  module Comp (T1 : Comp) (T2 : Comp) (T3 : Comp) (T4 : Comp)
    : Comp with type t = T1.t * T2.t * T3.t * T4.t
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

  val get12 : 'a * 'b * 'c * 'd * 'e -> 'a * 'b
  val get13 : 'a * 'b * 'c * 'd * 'e -> 'a * 'c
  val get14 : 'a * 'b * 'c * 'd * 'e -> 'a * 'd
  val get15 : 'a * 'b * 'c * 'd * 'e -> 'a * 'e
  val get23 : 'a * 'b * 'c * 'd * 'e -> 'b * 'c
  val get24 : 'a * 'b * 'c * 'd * 'e -> 'b * 'd
  val get25 : 'a * 'b * 'c * 'd * 'e -> 'b * 'e
  val get34 : 'a * 'b * 'c * 'd * 'e -> 'c * 'd
  val get35 : 'a * 'b * 'c * 'd * 'e -> 'c * 'e
  val get45 : 'a * 'b * 'c * 'd * 'e -> 'd * 'e

  val get123 : 'a * 'b * 'c * 'd * 'e -> 'a * 'b * 'c
  val get124 : 'a * 'b * 'c * 'd * 'e -> 'a * 'b * 'd
  val get125 : 'a * 'b * 'c * 'd * 'e -> 'a * 'b * 'e
  val get134 : 'a * 'b * 'c * 'd * 'e -> 'a * 'c * 'd
  val get135 : 'a * 'b * 'c * 'd * 'e -> 'a * 'c * 'e
  val get145 : 'a * 'b * 'c * 'd * 'e -> 'a * 'd * 'e
  val get234 : 'a * 'b * 'c * 'd * 'e -> 'b * 'c * 'd
  val get235 : 'a * 'b * 'c * 'd * 'e -> 'b * 'c * 'e
  val get245 : 'a * 'b * 'c * 'd * 'e -> 'b * 'd * 'e
  val get345 : 'a * 'b * 'c * 'd * 'e -> 'c * 'd * 'e

  val get1234 : 'a * 'b * 'c * 'd * 'e -> 'a * 'b * 'c * 'd
  val get1235 : 'a * 'b * 'c * 'd * 'e -> 'a * 'b * 'c * 'e
  val get1245 : 'a * 'b * 'c * 'd * 'e -> 'a * 'b * 'd * 'e
  val get1345 : 'a * 'b * 'c * 'd * 'e -> 'a * 'c * 'd * 'e
  val get2345 : 'a * 'b * 'c * 'd * 'e -> 'b * 'c * 'd * 'e

  val map : ('a -> 'f) -> ('b -> 'g) -> ('c -> 'h) -> ('d -> 'i) -> ('e -> 'j) -> 'a * 'b * 'c * 'd * 'e -> 'f * 'g * 'h * 'i * 'j

  val mapn : ('a -> 'b) -> ('a * 'a * 'a * 'a * 'a) -> ('b * 'b * 'b * 'b * 'b)
  (** Like {!map} but specialized for tuples with elements of the
      same type.

      [mapn f] is equivalent to [map f f f f f].
  *)

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

  val print : ?first:string -> ?sep:string -> ?last:string
    -> ('o BatIO.output -> 'a -> unit) -> ('o BatIO.output -> 'b -> unit) -> ('o BatIO.output -> 'c -> unit) -> ('o BatIO.output -> 'd -> unit) -> ('o BatIO.output -> 'e -> unit)
    -> 'o BatIO.output -> ('a * 'b * 'c * 'd * 'e) -> unit

  val printn : ?first:string -> ?sep:string -> ?last:string
    -> ('o BatIO.output -> 'a -> unit)
    -> 'o BatIO.output -> ('a * 'a * 'a * 'a * 'a) -> unit

  val compare : ?cmp1:('a -> 'a -> int) -> ?cmp2:('b -> 'b -> int) -> ?cmp3:('c -> 'c -> int) -> ?cmp4:('d -> 'd -> int) -> ?cmp5:('e -> 'e -> int) -> ('a * 'b * 'c * 'd * 'e) -> ('a * 'b * 'c * 'd * 'e) -> int

  include BatEnum.Enumerable with type 'a enumerable = 'a * 'a * 'a * 'a * 'a

  open BatOrd
  val eq : 'a eq -> 'b eq -> 'c eq -> 'd eq -> 'e eq
    -> ('a * 'b * 'c * 'd * 'e) eq
  val ord : 'a ord -> 'b ord -> 'c ord -> 'd ord -> 'e ord
    -> ('a * 'b * 'c * 'd * 'e) ord
  val comp : 'a comp -> 'b comp -> 'c comp -> 'd comp -> 'e comp
    -> ('a * 'b * 'c * 'd * 'e) comp
  module Eq (T1 : Eq) (T2 : Eq) (T3 : Eq) (T4 : Eq) (T5 : Eq)
    : Eq with type t = T1.t * T2.t * T3.t * T4.t * T5.t
  module Ord (T1 : Ord) (T2 : Ord) (T3 : Ord) (T4 : Ord) (T5 : Ord)
    : Ord with type t = T1.t * T2.t * T3.t * T4.t * T5.t
  module Comp (T1 : Comp) (T2 : Comp) (T3 : Comp) (T4 : Comp) (T5 : Comp)
    : Comp with type t = T1.t * T2.t * T3.t * T4.t * T5.t
end
