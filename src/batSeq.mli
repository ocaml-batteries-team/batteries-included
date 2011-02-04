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

(** Sequence of elements *)

(** A sequence represent a collection of elements, for which you never
    construct the complete representation.

    Basically you should use a sequence when you would prefer using a
    list or a lazy-list but constructing the whole list explicitly
    would explode your memory.

    All functions returning a sequence operates in time and space
    O(1).

    Note that if you want a ``consumable sequence'', you should prefer
    using enumerations (from module {!BatEnum}).

    @author Jeremie Dimino
*)

type 'a t = unit -> 'a node
  (** A sequence is a computation which returns a list-like node *)

and 'a node =
  | Nil
  | Cons of 'a * 'a t

include BatInterfaces.Mappable with type 'a mappable = 'a t

val enum : 'a t -> 'a BatEnum.t
  (** [enum s] returns the enumeration of all element of [s].

      Since enumerations are consumable and sequence are not, it is
      not possible to have the inverse operations, i.e. [of_enum] *)

(** {6 Base operations} *)

val length : 'a t -> int
  (** Return the number of elements of the given sequence. This may
      never return if the sequence is infinite. *)

val hd : 'a t -> 'a
  (** Returns the first element of the sequence or raise [Invalid_argument] if
      the sequence is empty. *)

val tl : 'a t -> 'a t
  (** Returns the sequence without its first elements or raise
      [Invalid_argument] if the sequence is empty. *)

val is_empty : 'a t -> bool
  (** [is_empty e] returns true if [e] does not contains any
      element. *)

val first : 'a t -> 'a
  (** Same as {!hd} *)

val last : 'a t -> 'a
  (** Returns the last element of the sequence, or raise [Invalid_argument] if
      the sequence is empty. *)

val at : 'a t -> int -> 'a
(** [at l n] returns the element at index [n] (starting from [0]) in
    the sequence [l] or raise [Invalid_argument] is the index is
    outside of [l] bounds. *)

val append : 'a t -> 'a t -> 'a t
(** [append s1 s2] returns the sequence which first returns all
    elements of [s1] then all elements of [s2]. *)

val concat : 'a t t -> 'a t
  (** [concat s] returns the sequence which returns all the elements
      of all the elements of [s], in the same order. *)

val flatten : 'a t t -> 'a t
  (** Same as {!concat}. *)

(** {6 Constructors} *)

val nil : 'a t
  (** [nil = fun () -> Nil] *)

val cons : 'a -> 'a t -> 'a t
  (** [cons e s = fun () -> Cons(e, s)] *)

val make : int -> 'a -> 'a t
  (** [make n e] returns the sequence of length [n] where all elements
      are [e] *)

val init : int -> (int -> 'a) -> 'a t
  (** [init n f] returns the sequence returning the results of [f 0],
      [f 1].... [f (n-1)]. Raise [Invalid_argument] if [n < 0]. *)

(** {6 Iterators} *)

val iter : ('a -> unit) -> 'a t -> unit
  (** [iter f s] applies [f] to all the elements of the sequence. Eager. *)

val map : ('a -> 'b) -> 'a t -> 'b t
  (** [map f s] returns the sequence where elements are elements of
      [s] mapped with [f]. Lazy. *)

val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
  (** [fold_left f a (cons b0 (... bn))] is [f (... (f (f a b0) b1) ...)
      bn]. Tail-recursive, eager.
  *)

val fold_right : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
(** [fold_right f (cons a0 (cons a1 (cons a2 ...))) b] is [f a0 (f
    a1 (f a2 ...))].
    Not tail-recursive, eager.
*)

val reduce : ('a -> 'a -> 'a) -> 'a t -> 'a
(** [reduce f (cons e s)] is [fold_left f e s].

    @raise Invalid_argument on empty sequences. *)

val max : 'a t -> 'a
  (** [max s] returns the largest value in [s] as judged by
      [Pervasives.compare] *)

val min : 'a t -> 'a
  (** [min s] returns the smallest value in [s] as judged by
      [Pervasives.compare] *)

(** {6 Sequence scanning}

    Most functions in the following sections have a shortcut semantic
    similar to the behavior of the usual (&&) and (||) operators :
    they will force the sequence until they find an satisfying
    element, and then return immediately.

    For example, [for_all] will only diverge if the sequence begins
    with an infinite number of true elements --- elements for which
    the predicate [p] returns [true].
*)

val for_all : ('a -> bool) -> 'a t -> bool
(** [for_all p (cons a0 (cons a1 ...))] checks if all elements of the
    given sequence satisfy the predicate [p]. That is, it returns
    [(p a0) && (p a1) && ...]. Eager, shortcut.
*)

val exists : ('a -> bool) -> 'a t -> bool
(** [exists p (cons a0 (cons a1 ...))] checks if at least one element of
    the sequence satisfies the predicate [p]. That is, it returns
    [(p a0) || (p a1) || ...]. Eager, shortcut.
*)

val mem : 'a -> 'a t -> bool
(** [mem a l] is true if and only if [a] is equal to an element of
    [l]. Eager, shortcut.
*)

(** {6 Sequence searching} *)

val find : ('a -> bool) -> 'a t -> 'a option
  (** [find p s] returns the first element of [s] such as [p e]
      returns [true], if any. Eager, shortcut.
  *)

val find_map : ('a -> 'b option) -> 'a t -> 'b option
  (** [find_map p s] finds the first element of [s] for which [p e]
      returns [Some r], if any. Eager, short-cut.
  *)

val filter : ('a -> bool) -> 'a t -> 'a t
(** [filter p s] returns the sequence of elements of [s] satisfying
    [p]. Lazy.
    
    {b Note} filter is lazy in that it returns a lazy sequence, but
    each element in the result is eagerly searched in the input
    sequence. Therefore, the access to a given element in the result
    will diverge if it is preceded, in the input sequence, by
    infinitely many false elements (elements on which the predicate
    [p] returns [false]).

    Other functions that may drop an unbound number of elements
    ([filter_map], [take_while], etc.) have the same behavior. 
*)

val filter_map : ('a -> 'b option) -> 'a t -> 'b t
(** [filter_map f s] returns the sequence of elements filtered and
    mapped by [f]. Lazy.
*)

(** {6 Association sequences} *)

val assoc : 'a -> ('a * 'b) t -> 'b option
(** [assoc a s] returns the value associated with key [a] in the
    sequence of pairs [s]. Eager, shortcut.  *)

(** {6 Sequence transformations} *)

val take : int -> 'a t -> 'a t
  (** [take n s] returns up to the [n] first elements from sequence
      [s], if available. Lazy. *)

val drop : int -> 'a t -> 'a t
  (** [drop n s] returns [s] without the first [n] elements, or the
      empty sequence if [s] have less than [n] elements. Lazy. *)

val take_while : ('a -> bool) -> 'a t -> 'a t
  (** [take_while f s] returns the first elements of sequence [s]
      which satisfy the predicate [f]. Lazy. *)

val drop_while : ('a -> bool) -> 'a t -> 'a t
(** [drop_while f s] returns the sequence [s] with the first
    elements satisfying the predicate [f] dropped. Lazy. *)

(** {6 Sequence of pairs} *)

val split : ('a * 'b) t -> 'a t * 'b t
  (** [split s = (map fst s, map snd s)]. Lazy. *)

val combine : 'a t -> 'b t -> ('a * 'b) t
  (** Transform a pair of sequences into a sequence of pairs. Lazy.

      @raise Invalid_argument if given sequences of different length. *)

(** {6 Printing} *)

val print : ?first:string -> ?last:string -> ?sep:string -> ('a BatInnerIO.output -> 'b -> unit) ->  'a BatInnerIO.output -> 'b t -> unit
  (**Print the contents of a sequence*)

val sprint : ?first:string -> ?last:string -> ?sep:string -> ('a BatInnerIO.output -> 'b -> unit) -> 'b t -> string
  (** Using a string printer, print a sequence to a string (as sprintf vs. printf)
      @deprecated use {!BatIO.to_string}.
   *)

val t_printer : 'a BatValue_printer.t -> 'a t BatValue_printer.t

module Exceptionless : sig
  val hd : 'a t -> 'a option
  val tl : 'a t -> 'a t option
  val first : 'a t -> 'a option
  val last : 'a t -> 'a option
  val at : 'a t -> int -> 'a option
  (*
  val make : int -> 'a -> 'a t
  val init : int -> (int -> 'a) -> 'a t
  *)
  val reduce : ('a -> 'a -> 'a) -> 'a t -> 'a option
  val max : 'a t -> 'a option
  val min : 'a t -> 'a option
  val combine : 'a t -> 'b t -> ('a * 'b) t option
end
