(*
 * Copyright (C) 2011  Batteries Included Development Team
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

(**
   This module implements a generic finger tree datastructure
   as described here:
   Finger Trees: A Simple General-purpose Data Structure
   http://www.soi.city.ac.uk/~ross/papers/FingerTree.pdf

   The finger tree itself is functorized over the measure
   (so monomorphic over the measure) and polymorphic over the
   measurement function (because there are useful measurements
   functions that are polymorphic over the elements being measured).

   This module also contains an instanciation of a finger tree that
   implements a functional sequence with the following characteristics:
   - amortized constant time addition and deletions at both ends
   - contant time size operation
   - logarithmic lookup, update or deletion of the element at a given index
   - logarithmic splitting and concatenation

   If you are trying to understand the signature at first, whenever you
   see a type [(something, _, _) wrap], just pretend it is simply
   the type [something] (this is what the documentation does).

   Complexities are given assuming that the monoid combination operation
   and the measurement functions are constant time and space.
*)

(** The type of the element of a monoid. *)
type 'a monoid = {
  zero : 'a; (** The neutral element of the monoid. *)
  combine : 'a -> 'a -> 'a ; (** [combine] should be associative, and have [zero] as neutral element. *)
}

exception Empty
(** An exception that is thrown by various operations when
    trying to get a non existing element. *)

module type S =
sig

  type ('a, 'm) fg
  (** The type of finger trees containing elements of type ['a]
      measured with the type [measure]. *)

  type ('wrapped_type, 'a, 'm) wrap
  (** A type meant to avoid duplication of signatures.

      For the generic finger tree, this type will
      be [monoid:'m monoid -> measure:('a -> 'm) -> 'wrapped_type].

      Once the functor has been applied, the resulting module should
      be reexported in such a way that the type is now simply
      ['wrapped_type].
  *)

  (** {6 Construction} *)

  val empty : ('a, 'm) fg
  (** [empty] is the sequence with no elements. *)

  val singleton : 'a -> ('a, 'm) fg
  (** [singleton elt] build the sequence containing [elt] as its sole
      element.

      O(1).
  *)

  val cons : (('a, 'm) fg -> 'a -> ('a, 'm) fg, 'a, 'm) wrap
  (** [cons t elt] adds [elt] to the left of [t].

      O(1) amortized, O(log(n)) worst case time, stack and heap space.
  *)

  val snoc : (('a, 'm) fg -> 'a -> ('a, 'm) fg, 'a, 'm) wrap
  (** [snoc t elt] adds [elt] to the right of [t].

      O(1) amortized, O(log(n)) worst case time, stack and heap space.
  *)

  (** {6 Deconstruction} *)

  val front : (('a, 'm) fg -> ('a * ('a, 'm) fg) option, 'a, 'm) wrap
  (** [front t] returns [None] when [t] is empty,
      or [Some (hd, tl)] when [hd] is the first element of the sequence
      and [tl] is the rest of the sequence.

      O(1) amortized, O(log(n)) worst case time, stack space and heap space.
  *)

  val front_exn : (('a, 'm) fg -> ('a * ('a, 'm) fg), 'a, 'm) wrap
  (** [front_exn t] returns [(hd, tl)] when [hd] is the first element
      of the sequence and [tl] is the rest of the sequence.
      @raise Empty if [t] is empty.

      O(1) amortized, O(log(n)) worst case time, stack space and heap space.
  *)

  val head : ('a, 'm) fg -> 'a option
  (** [head t] returns [None] if [t] is empty,
      or [Some hd] otherwise, where [hd] is the first element
      of the sequence.

      O(1) time, stack space and heap space.
  *)

  val head_exn : ('a, 'm) fg -> 'a
  (** [head_exn t] returns the first element of the sequence.
      @raise Empty if [t] is empty.

      O(1) time, stack space and heap space.
  *)

  val last : ('a, 'm) fg -> 'a option
  (** [last t] returns [None] if [t] is empty,
      or [Some hd] otherwise, where [hd] is the last element
      of the sequence.

      O(1) time, stack space and heap space.
  *)

  val last_exn : ('a, 'm) fg -> 'a
  (** [last_exn t] returns the last element of the sequence.
      @raise Empty if [t] is empty.

      O(1) time, stack space and heap space.
  *)

  val tail : (('a, 'm) fg -> ('a, 'm) fg option, 'a, 'm) wrap
  (** [tail t] returns [None] when [t] is empty,
      or [Some tl] where [tl] is the sequence [t] where the first element
      has been removed.

      O(1) amortized, O(log(n)) worst case time, stack space and heap space.
  *)

  val tail_exn : (('a, 'm) fg -> ('a, 'm) fg, 'a, 'm) wrap
  (** [tail_exn t] returns the sequence [t] where the first element
      has been removed.
      @raise Empty if [t] is empty.

      O(1) amortized, O(log(n)) worst case time, stack space and heap space.
  *)

  val init : (('a, 'm) fg -> ('a, 'm) fg option, 'a, 'm) wrap
  (** [init t] returns [None] if [t] is empty,
      or [Some init] where [init] is the sequence [t] where the last element
      has been removed.

      O(1) amortized, O(log(n)) worst case time, stack space and heap space.
  *)

  val init_exn : (('a, 'm) fg -> ('a, 'm) fg, 'a, 'm) wrap
  (** [init_exn t] returns the sequence [t] where the last element
      has been removed.
      @raise Empty if [t] is empty.

      O(1) amortized, O(log(n)) worst case time, stack space and heap space.
  *)

  val rear : (('a, 'm) fg -> ('a * ('a, 'm) fg) option, 'a, 'm) wrap
  (** [rear t] returns [None] when [t] is empty,
      or [Some (last, init)] where [last] is the last element of the
      sequence and [init] is the rest of the sequence.

      O(1) amortized, O(log(n)) worst case time, stack space and heap space.
  *)

  val rear_exn : (('a, 'm) fg -> ('a * ('a, 'm) fg), 'a, 'm) wrap
  (** [rear t] returns [(last, init)] when [last] is the last element of
      the sequence and [init] is the rest of the sequence.
      @raise Empty if [t] is empty.

      O(1) amortized, O(log(n)) worst case time, stack space and heap space.
  *)

  (** {6 Inspection} *)

  val size : ('a, 'm) fg -> int
  (** [size t] returns the number of elements in the sequence.

      - O(n) time.
      - O(log(n)) stack space.
      - O(1) heap space.
  *)

  val fold_left : ('acc -> 'a -> 'acc) -> 'acc -> ('a, 'm) fg -> 'acc
  (** [fold_left] is equivalent to [List.fold_left].

      - O(n) time.
      - O(1) heap space.
      - O(log(n)) stack space.
  *)

  val fold_right : ('acc -> 'a -> 'acc) -> 'acc -> ('a, 'm) fg -> 'acc
  (** [fold_right] is equivalent to [List.fold_right].

      - O(n) time.
      - O(1) heap space.
      - O(log(n)) stack space.
  *)

  val iter : ('a -> unit) -> ('a, 'm) fg -> unit
  (** [iter] is equivalent to [List.iter].

      - O(n) time.
      - O(1) heap space.
      - O(log(n)) stack space.
  *)

  val iter_right : ('a -> unit) -> ('a, 'm) fg -> unit
  (** [iter_right] is equivalent to [List.iter o List.rev].

      - O(n) time.
      - O(1) heap space.
      - O(log(n)) stack space.
 *)

  (** {6 Conversions} *)

  (** {7 Conversions to other structures} *)

  val enum : ('a, 'm) fg -> 'a BatEnum.t
  (** [enum t] builds an enumeration of the elements of [t] going from
      left to right.

      O(1).

      Forcing the whole enumeration takes:
      - O(n) time.
      - O(1) stack space.
      - O(log(n)) heap space.
  *)

  val backwards : ('a, 'm) fg -> 'a BatEnum.t
  (** [backwards t] builds an enumeration of the elements of [t] going from
      right to left.
      Same complexity as {!enum}.
  *)

  val to_list : ('a, 'm) fg -> 'a list
  (** [to_list t] is equivalent to [BatList.of_enum (enum t)].
      - O(n) time and heap space.
      - O(log(n)) stack space.
  *)

  val to_list_backwards : ('a, 'm) fg -> 'a list
  (** [to_list_backwards t] is equivalent to [BatList.of_enum (backwards t)].
      - O(n) time and heap space.
      - O(log(n)) stack space.
  *)

  (** {7 Conversions from other structures} *)

  val of_enum : ('a BatEnum.t -> ('a, 'm) fg, 'a, 'm) wrap
  (** [of_enum e] build the sequence containing the elements of [e]
      in the same order.

      In addition to the complexity of forcing the enumeration, it is:
      - O(n) time.
      - O(log(n)) stack space.
      - O(n) heap space.
  *)

  val of_backwards : ('a BatEnum.t -> ('a, 'm) fg, 'a, 'm) wrap
  (** [of_backward e] is equivalent to [reverse (of_enum e)].
      - O(n) time.
      - O(log(n)) stack space.
      - O(n) heap space.
  *)

  val of_list : ('a list -> ('a, 'm) fg, 'a, 'm) wrap
  (** [of_list l] is equivalent to [of_enum (BatList.enum l)].
      - O(n) time
      - O(log(n)) stack space
      - O(n) heap space
  *)

  val of_list_backwards : ('a list -> ('a, 'm) fg, 'a, 'm) wrap
  (** [of_list_backwards l] is equivalent to
      [of_enum_backwards (BatList.enum l)].
      - O(n) time.
      - O(log(n)) stack space.
      - O(n) heap space.
  *)

  (** {6 Combining/reorganizing} *)

  val map : (('a -> 'b) -> ('a, 'm) fg -> ('b, 'm) fg, 'b, 'm) wrap
  (** [map] is equivalent to {!List.map}.
      - O(n) time.
      - O(n) heap space.
      - O(log(n)) stack space.
  *)

  val map_right : (('a -> 'b) -> ('a, 'm) fg -> ('b, 'm) fg, 'b, 'm) wrap
  (** [map] is equivalent to [List.map o List.rev].
      - O(n) time.
      - O(n) heap space.
      - O(log(n)) stack space.
  *)

  val append : (('a, 'm) fg -> ('a, 'm) fg -> ('a, 'm) fg, 'a, 'm) wrap
  (** [append] is equivalent to [List.append].

      O(log(min(n,m))) time, stack space, heap space
  *)

  val reverse : (('a, 'm) fg -> ('a, 'm) fg, 'a, 'm) wrap
  (** [reverse t] is equivalent to [of_list (List.rev (to_list t))].
      - O(n) time, heap space.
      - O(log(n)) stack space.
  *)

  (** {6 Boilerplate code} *)

  val print : ?first:string -> ?last:string -> ?sep:string -> ('a BatInnerIO.output -> 'b -> unit) -> 'a BatInnerIO.output -> ('b, _) fg -> unit

  val t_printer : 'a BatValuePrinter.t -> ('a, _) fg BatValuePrinter.t
end

module Generic : sig
  include S
  with type ('wrapped_type, 'a, 'm) wrap = monoid:'m monoid -> measure:('a -> 'm) -> 'wrapped_type

  val lookup : (('m -> bool) -> ('a, 'm) fg -> 'a, 'a, 'm) wrap
  (** [lookup p t], when [p] is monotonic, returns the first element
      of the sequence for which the measure of its predecessors in the
      sequence (itself included) satisfies [p].
      @raise Empty is there is no such element.

      - O(log(n)) time, stack space.
      - O(1) heap space.

      When [p] is not monotonic, take a look at the code or at the paper
      cited above and see if you understand something (lookup is a
      specialized version of splitTree that returns the element without
      building the left and right tree).
  *)

  val measure : (('a, 'm) fg -> 'm, 'a, 'm) wrap
  (** [measure m] gives the measure of the whole tree, whose meaning
      depends on the measure chosen.

      O(1).
  *)

  val split : (('m -> bool) -> ('a, 'm) fg -> ('a, 'm) fg * ('a, 'm) fg, 'a, 'm) wrap
  (**
      [split p t], when [p] is monotonic, returns [(t1, t2)] where
      [t1] is the longest prefix of [t] whose measure does not satifies
      [p], and [t2] is the rest of [t].
      @raise Empty is there is no such element

      O(log(n)) time, stack space, heap space.

      When [p] is not monotonic, take a look at the code or at the paper
      cited above and see if you understand something.
  *)
end

type 'a t
include S with type ('wrapped_type, 'a, 'm) wrap = 'wrapped_type
          and type ('a, 'm) fg = 'a t

val size : 'a t -> int
(** [size t] returns the number of elements in the sequence.

    Unlike the generic [size] on finger trees, this one has complexity O(1).
*)

val split_at : 'a t -> int -> 'a t * 'a t
(** [split_at] is equivalent to [List.split_at].
    @raise Invalid_argument when the index is out of bounds.

    O(log(n)) time, stack space, heap space.
*)

val get : int -> 'a t -> 'a
(** [get i t] returns the [i]-th element of [t].
    @raise Invalid_argument when the index is out of bounds.

    - O(log(n)) time, stack space.
    - O(1) heap space.
*)

val set : int -> 'a -> 'a t -> 'a t
(** [set i v t] returns [t] where the [i]-th element is now [v].
    @raise Invalid_argument when the index is out of bounds.

    O(log(n)) time, stack space, heap space.
*)

val update : int -> ('a -> 'a) -> 'a t -> 'a t
(** [update i f t] returns [t] where the [i]-th element is now [f (get i t)].
    @raise Invalid_argument when the index is out of bounds.

    O(log(n)) time, stack space, heap space.
*)
