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

(** A sequence represents a collection of elements, for which you never
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

##V<4.7## type 'a t = unit -> 'a node
##V>=4.7## type 'a t = 'a Stdlib.Seq.t
(** A sequence is a computation which returns a list-like node *)

and 'a node =
##V>=4.7## 'a Stdlib.Seq.node =
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

val empty : 'a t
(** the empty sequence, containing no elements

    @since 3.3.0 *)

val return : 'a -> 'a t
(** the singleton sequence, containing only the given element

    @since 3.3.0 *)

val cons : 'a -> 'a t -> 'a t
(** [cons e s = fun () -> Cons(e, s)] *)

val make : int -> 'a -> 'a t
(** [make n e] returns the sequence of length [n] where all elements
    are [e] *)

val init : int -> (int -> 'a) -> 'a t
(** [init n f] returns the sequence returning the results of [f 0],
    [f 1].... [f (n-1)].

    @raise Invalid_argument if [n < 0]. *)

val of_list : 'a list -> 'a t
(** Convenience function to build a seq from a list.
    @since 2.2.0 *)

val unfold : ('b -> ('a * 'b) option) -> 'b -> 'a t
(** Build a sequence from a step function and an initial value.
    [unfold f u] returns [empty] if [f u] returns [None],
    or [fun () -> Cons (x, unfold f y)] if [f u] returns [Some (x, y)].

    For example, [unfold (function [] -> None | h::t -> Some (h,t)) l]
    is equivalent to [List.to_seq l].

    @since 3.3.0 *)

val flat_map : ('a -> 'b t) -> 'a t -> 'b t
(** Map each element to a subsequence, then return each element of this
    sub-sequence in turn. This transformation is lazy, it only applies
    when the result is traversed.

    @since 3.3.0 *)

val concat_map: ('a -> 'b t) -> 'a t -> 'b t
(** Alias for {!flat_map}.
    @since 3.4.0 *)

(** {6 Iterators} *)

val iter : ('a -> unit) -> 'a t -> unit
(** [iter f s] applies [f] to all the elements of the sequence. Eager. *)

val iteri : (int -> 'a -> unit) -> 'a t -> unit
(** [iteri f s] is the same as [iter f s], but [f] is given the index
    of each element (starting at 0).
    @since 2.2.0 *)

val iter2 : ('a -> 'b -> unit) -> 'a t -> 'b t -> unit
(** [iter2 f s1 s2] iterates on elements of [s1] and [s2] pairwise, and
    stops when it meets the end of [s1] or [s2]
    @since 2.2.0 *)

val map : ('a -> 'b) -> 'a t -> 'b t
(** [map f s] returns the sequence where elements are elements of
    [s] mapped with [f]. Lazy. *)

val mapi : (int -> 'a -> 'b) -> 'a t -> 'b t
(** [mapi f s] lazily maps elements of [s] into a new sequence,
    using [f]. [f] is also given elements' indexes.
    @since 2.2.0 *)

val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
(** [map2 f s1 s2] returns a sequence of elements, resulting from combininig
    elements of [s1] and [s2] at the same index using [f]. The result is as
    long as the shortest argument.
    @since 2.2.0 *)

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
    [Pervasives.compare]

    @raise Invalid_argument on empty sequences. *)

val min : 'a t -> 'a
(** [min s] returns the smallest value in [s] as judged by
    [Pervasives.compare]

    @raise Invalid_argument on empty sequences. *)

val equal : ?eq:('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  (** [equal ~eq s1 s2] compares elements of [s1] and [s2] pairwise
      using [eq]
      @param eq optional equality function (default {!Pervasives.(=)})
      @since 2.2.0 *)

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

val to_buffer : ?first:string -> ?last:string -> ?sep:string -> ('a -> string) -> Buffer.t -> (unit -> 'a node) -> unit
(** Convert a sequence to a string in the given buffer; eager.
    @since 2.10.0
*)

val to_string : ?first:string -> ?last:string -> ?sep:string -> ('a -> string) -> 'a t -> string
(** Convert the sequence to a string; eager.
    @since 2.10.0
*)

val of_string : ?first:string -> ?last:string -> ?sep:string -> (string -> 'a) -> string -> 'a t
(** Create a sequence by parsing a string.
    @raise Invalid_argument if the string is not prefixed by [first].
    @raise Invalid_argument if the string is not suffixed by [last].
    @since 2.10.0
*)

module Infix : sig
  (** Infix operators matching those provided by {!BatEnum.Infix} *)

  val ( -- ) : int -> int -> int t
  val ( --^ ) : int -> int -> int t
  val ( --. ) : float * float -> float -> float t
  val ( --- ) : int -> int -> int t
  val ( --~ ) : char -> char -> char t
  val ( // ) : 'a t -> ('a -> bool) -> 'a t
  val ( /@ ) : 'a t -> ('a -> 'b) -> 'b t
  val ( @/ ) : ('a -> 'b) -> 'a t -> 'b t
  val ( //@ ) : 'a t -> ('a -> 'b option) -> 'b t
  val ( @// ) : ('a -> 'b option) -> 'a t -> 'b t
end

include module type of Infix

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
  val combine : 'a t -> 'b t -> ('a * 'b) t
end

##V>=4.14##val is_empty : 'a t -> bool
##V>=4.14##(** [is_empty xs] determines whether the sequence [xs] is empty.
##V>=4.14##
##V>=4.14##    It is recommended that the sequence [xs] be persistent.
##V>=4.14##    Indeed, [is_empty xs] demands the head of the sequence [xs],
##V>=4.14##    so, if [xs] is ephemeral, it may be the case that [xs] cannot
##V>=4.14##    be used any more after this call has taken place.
##V>=4.14##
##V>=4.14##    @since 4.14 *)

##V>=4.14##val uncons : 'a t -> ('a * 'a t) option
##V>=4.14##(** If [xs] is empty, then [uncons xs] is [None].
##V>=4.14##
##V>=4.14##    If [xs] is nonempty, then [uncons xs] is
##V>=4.14##    [Some (head xs, tail xs)],
##V>=4.14##    that is, a pair of the head and tail of the sequence [xs].
##V>=4.14##
##V>=4.14##    This equivalence holds if [xs] is persistent.
##V>=4.14##    If [xs] is ephemeral, then [uncons] must be preferred
##V>=4.14##    over separate calls to [head] and [tail],
##V>=4.14##    which would cause [xs] to be queried twice.
##V>=4.14##
##V>=4.14##    @since 4.14 *)

##V>=4.14##val length : 'a t -> int
##V>=4.14##(** [length xs] is the length of the sequence [xs].
##V>=4.14##
##V>=4.14##    The sequence [xs] must be finite.
##V>=4.14##
##V>=4.14##    @since 4.14 *)

##V>=4.14##val iteri : (int -> 'a -> unit) -> 'a t -> unit
##V>=4.14##(** [iteri f xs] invokes [f i x] successively
##V>=4.14##    for every element [x] located at index [i] in the sequence [xs].
##V>=4.14##
##V>=4.14##    It terminates only if the sequence [xs] is finite.
##V>=4.14##
##V>=4.14##    [iteri f xs] is equivalent to
##V>=4.14##    [iter (fun (i, x) -> f i x) (zip (ints 0) xs)].
##V>=4.14##
##V>=4.14##    @since 4.14 *)

##V>=4.14##val fold_lefti : ('b -> int -> 'a -> 'b) -> 'b -> 'a t -> 'b
##V>=4.14##(** [fold_lefti f _ xs] invokes [f _ i x] successively
##V>=4.14##    for every element [x] located at index [i] of the sequence [xs].
##V>=4.14##
##V>=4.14##    An accumulator of type ['b] is threaded through the calls to [f].
##V>=4.14##
##V>=4.14##    It terminates only if the sequence [xs] is finite.
##V>=4.14##
##V>=4.14##    [fold_lefti f accu xs] is equivalent to
##V>=4.14##    [fold_left (fun accu (i, x) -> f accu i x) accu (zip (ints 0) xs)].
##V>=4.14##
##V>=4.14##    @since 4.14 *)

##V>=4.14##val for_all : ('a -> bool) -> 'a t -> bool
##V>=4.14##(** [for_all p xs] determines whether all elements [x] of the sequence [xs]
##V>=4.14##    satisfy [p x].
##V>=4.14##
##V>=4.14##    The sequence [xs] must be finite.
##V>=4.14##
##V>=4.14##    @since 4.14 *)

##V>=4.14##val exists : ('a -> bool) -> 'a t -> bool
##V>=4.14##(** [exists xs p] determines whether at least one element [x]
##V>=4.14##    of the sequence [xs] satisfies [p x].
##V>=4.14##
##V>=4.14##    The sequence [xs] must be finite.
##V>=4.14##
##V>=4.14##    @since 4.14 *)

##V>=4.14##val find : ('a -> bool) -> 'a t -> 'a option
##V>=4.14##(** [find p xs] returns [Some x], where [x] is the first element of the
##V>=4.14##    sequence [xs] that satisfies [p x], if there is such an element.
##V>=4.14##
##V>=4.14##    It returns [None] if there is no such element.
##V>=4.14##
##V>=4.14##    The sequence [xs] must be finite.
##V>=4.14##
##V>=4.14##    @since 4.14 *)

##V>=4.14##val find_map : ('a -> 'b option) -> 'a t -> 'b option
##V>=4.14##(** [find_map f xs] returns [Some y], where [x] is the first element of the
##V>=4.14##    sequence [xs] such that [f x = Some _], if there is such an element,
##V>=4.14##    and where [y] is defined by [f x = Some y].
##V>=4.14##
##V>=4.14##    It returns [None] if there is no such element.
##V>=4.14##
##V>=4.14##    The sequence [xs] must be finite.
##V>=4.14##
##V>=4.14##    @since 4.14 *)

##V>=4.14##val iter2 : ('a -> 'b -> unit) -> 'a t -> 'b t -> unit
##V>=4.14##(** [iter2 f xs ys] invokes [f x y] successively for every pair [(x, y)] of
##V>=4.14##    elements drawn synchronously from the sequences [xs] and [ys].
##V>=4.14##
##V>=4.14##    If the sequences [xs] and [ys] have different lengths, then
##V>=4.14##    iteration stops as soon as one sequence is exhausted;
##V>=4.14##    the excess elements in the other sequence are ignored.
##V>=4.14##
##V>=4.14##    Iteration terminates only if at least one of the sequences
##V>=4.14##    [xs] and [ys] is finite.
##V>=4.14##
##V>=4.14##    [iter2 f xs ys] is equivalent to
##V>=4.14##    [iter (fun (x, y) -> f x y) (zip xs ys)].
##V>=4.14##
##V>=4.14##    @since 4.14 *)

##V>=4.14##val fold_left2 : ('a -> 'b -> 'c -> 'a) -> 'a -> 'b t -> 'c t -> 'a
##V>=4.14##(** [fold_left2 f _ xs ys] invokes [f _ x y] successively
##V>=4.14##    for every pair [(x, y)] of elements drawn synchronously
##V>=4.14##    from the sequences [xs] and [ys].
##V>=4.14##
##V>=4.14##    An accumulator of type ['a] is threaded through the calls to [f].
##V>=4.14##
##V>=4.14##    If the sequences [xs] and [ys] have different lengths, then
##V>=4.14##    iteration stops as soon as one sequence is exhausted;
##V>=4.14##    the excess elements in the other sequence are ignored.
##V>=4.14##
##V>=4.14##    Iteration terminates only if at least one of the sequences
##V>=4.14##    [xs] and [ys] is finite.
##V>=4.14##
##V>=4.14##    [fold_left2 f accu xs ys] is equivalent to
##V>=4.14##    [fold_left (fun accu (x, y) -> f accu x y) (zip xs ys)].
##V>=4.14##
##V>=4.14##    @since 4.14 *)

##V>=4.14##val for_all2 : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool
##V>=4.14##(** [for_all2 p xs ys] determines whether all pairs [(x, y)] of elements
##V>=4.14##    drawn synchronously from the sequences [xs] and [ys] satisfy [p x y].
##V>=4.14##
##V>=4.14##    If the sequences [xs] and [ys] have different lengths, then
##V>=4.14##    iteration stops as soon as one sequence is exhausted;
##V>=4.14##    the excess elements in the other sequence are ignored.
##V>=4.14##    In particular, if [xs] or [ys] is empty, then
##V>=4.14##    [for_all2 p xs ys] is true. This is where
##V>=4.14##    [for_all2] and [equal] differ: [equal eq xs ys] can
##V>=4.14##    be true only if [xs] and [ys] have the same length.
##V>=4.14##
##V>=4.14##    At least one of the sequences [xs] and [ys] must be finite.
##V>=4.14##
##V>=4.14##    [for_all2 p xs ys] is equivalent to [for_all (fun b -> b) (map2 p xs ys)].
##V>=4.14##
##V>=4.14##    @since 4.14 *)

##V>=4.14##val exists2 : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool
##V>=4.14##(** [exists2 p xs ys] determines whether some pair [(x, y)] of elements
##V>=4.14##    drawn synchronously from the sequences [xs] and [ys] satisfies [p x y].
##V>=4.14##
##V>=4.14##    If the sequences [xs] and [ys] have different lengths, then
##V>=4.14##    iteration must stop as soon as one sequence is exhausted;
##V>=4.14##    the excess elements in the other sequence are ignored.
##V>=4.14##
##V>=4.14##    At least one of the sequences [xs] and [ys] must be finite.
##V>=4.14##
##V>=4.14##    [exists2 p xs ys] is equivalent to [exists (fun b -> b) (map2 p xs ys)].
##V>=4.14##
##V>=4.14##    @since 4.14 *)

##V>=4.14##val equal_stdlib : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool
##V>=4.14##(** Provided the function [eq] defines an equality on elements,
##V>=4.14##    [equal eq xs ys] determines whether the sequences [xs] and [ys]
##V>=4.14##    are pointwise equal.
##V>=4.14##
##V>=4.14##    At least one of the sequences [xs] and [ys] must be finite.
##V>=4.14##
##V>=4.14##    @since 4.14 *)

##V>=4.14##val compare : ('a -> 'b -> int) -> 'a t -> 'b t -> int
##V>=4.14##(** Provided the function [cmp] defines a preorder on elements,
##V>=4.14##    [compare cmp xs ys] compares the sequences [xs] and [ys]
##V>=4.14##    according to the lexicographic preorder.
##V>=4.14##
##V>=4.14##    For more details on comparison functions, see {!Array.sort}.
##V>=4.14##
##V>=4.14##    At least one of the sequences [xs] and [ys] must be finite.
##V>=4.14##
##V>=4.14##    @since 4.14 *)

##V>=4.14##val init : int -> (int -> 'a) -> 'a t
##V>=4.14##(** [init n f] is the sequence [f 0; f 1; ...; f (n-1)].
##V>=4.14##
##V>=4.14##    [n] must be nonnegative.
##V>=4.14##
##V>=4.14##    If desired, the infinite sequence [f 0; f 1; ...]
##V>=4.14##    can be defined as [map f (ints 0)].
##V>=4.14##
##V>=4.14##    @raise Invalid_argument if [n] is negative.
##V>=4.14##
##V>=4.14##    @since 4.14 *)

##V>=4.14##val repeat : 'a -> 'a t
##V>=4.14##(** [repeat x] is the infinite sequence
##V>=4.14##    where the element [x] is repeated indefinitely.
##V>=4.14##
##V>=4.14##    [repeat x] is equivalent to [cycle (return x)].
##V>=4.14##
##V>=4.14##    @since 4.14 *)

##V>=4.14##val forever : (unit -> 'a) -> 'a t
##V>=4.14##(** [forever f] is an infinite sequence where every element is produced
##V>=4.14##    (on demand) by the function call [f()].
##V>=4.14##
##V>=4.14##    For instance,
##V>=4.14##    [forever Random.bool] is an infinite sequence of random bits.
##V>=4.14##
##V>=4.14##    [forever f] is equivalent to [map f (repeat ())].
##V>=4.14##
##V>=4.14##    @since 4.14 *)

##V>=4.14##val cycle : 'a t -> 'a t
##V>=4.14##(** [cycle xs] is the infinite sequence that consists of an infinite
##V>=4.14##    number of repetitions of the sequence [xs].
##V>=4.14##
##V>=4.14##    If [xs] is an empty sequence,
##V>=4.14##    then [cycle xs] is empty as well.
##V>=4.14##
##V>=4.14##    Consuming (a prefix of) the sequence [cycle xs] once
##V>=4.14##    can cause the sequence [xs] to be consumed more than once.
##V>=4.14##    Therefore, [xs] must be persistent.
##V>=4.14##
##V>=4.14##    @since 4.14 *)

##V>=4.14##val iterate : ('a -> 'a) -> 'a -> 'a t
##V>=4.14##(** [iterate f x] is the infinite sequence whose elements are
##V>=4.14##    [x], [f x], [f (f x)], and so on.
##V>=4.14##
##V>=4.14##    In other words, it is the orbit of the function [f],
##V>=4.14##    starting at [x].
##V>=4.14##
##V>=4.14##    @since 4.14 *)

##V>=4.14##val mapi : (int -> 'a -> 'b) -> 'a t -> 'b t
##V>=4.14##(** [mapi] is analogous to [map], but applies the function [f] to
##V>=4.14##    an index and an element.
##V>=4.14##
##V>=4.14##    [mapi f xs] is equivalent to [map2 f (ints 0) xs].
##V>=4.14##
##V>=4.14##    @since 4.14 *)

##V>=4.14##val scan : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b t
##V>=4.14##(** If [xs] is a sequence [[x0; x1; x2; ...]], then
##V>=4.14##    [scan f a0 xs] is a sequence of accumulators
##V>=4.14##    [[a0; a1; a2; ...]]
##V>=4.14##    where [a1] is [f a0 x0], [a2] is [f a1 x1], and so on.
##V>=4.14##
##V>=4.14##    Thus, [scan f a0 xs] is conceptually related to
##V>=4.14##    [fold_left f a0 xs]. However, instead of performing an
##V>=4.14##    eager iteration and immediately returning the final accumulator,
##V>=4.14##    it returns a sequence of accumulators.
##V>=4.14##
##V>=4.14##    For instance, [scan (+) 0] transforms a sequence of integers
##V>=4.14##    into the sequence of its partial sums.
##V>=4.14##
##V>=4.14##    If [xs] has length [n]
##V>=4.14##    then [scan f a0 xs] has length [n+1].
##V>=4.14##
##V>=4.14##    @since 4.14 *)

##V>=4.14##val take : int -> 'a t -> 'a t
##V>=4.14##(** [take n xs] is the sequence of the first [n] elements of [xs].
##V>=4.14##
##V>=4.14##    If [xs] has fewer than [n] elements,
##V>=4.14##    then [take n xs] is equivalent to [xs].
##V>=4.14##
##V>=4.14##    [n] must be nonnegative.
##V>=4.14##
##V>=4.14##    @raise Invalid_argument if [n] is negative.
##V>=4.14##
##V>=4.14##    @since 4.14 *)

##V>=4.14##val drop : int -> 'a t -> 'a t
##V>=4.14##(** [drop n xs] is the sequence [xs], deprived of its first [n] elements.
##V>=4.14##
##V>=4.14##    If [xs] has fewer than [n] elements,
##V>=4.14##    then [drop n xs] is empty.
##V>=4.14##
##V>=4.14##    [n] must be nonnegative.
##V>=4.14##
##V>=4.14##    [drop] is lazy: the first [n+1] elements of the sequence [xs]
##V>=4.14##    are demanded only when the first element of [drop n xs] is
##V>=4.14##    demanded. For this reason, [drop 1 xs] is {i not} equivalent
##V>=4.14##    to [tail xs], which queries [xs] immediately.
##V>=4.14##
##V>=4.14##    @raise Invalid_argument if [n] is negative.
##V>=4.14##
##V>=4.14##    @since 4.14 *)

##V>=4.14##val take_while : ('a -> bool) -> 'a t -> 'a t
##V>=4.14##(** [take_while p xs] is the longest prefix of the sequence [xs]
##V>=4.14##    where every element [x] satisfies [p x].
##V>=4.14##
##V>=4.14##    @since 4.14 *)

##V>=4.14##val drop_while : ('a -> bool) -> 'a t -> 'a t
##V>=4.14##(** [drop_while p xs] is the sequence [xs], deprived of the prefix
##V>=4.14##    [take_while p xs].
##V>=4.14##
##V>=4.14##    @since 4.14 *)

##V>=4.14##val group : ('a -> 'a -> bool) -> 'a t -> 'a t t
##V>=4.14##(** Provided the function [eq] defines an equality on elements,
##V>=4.14##    [group eq xs] is the sequence of the maximal runs
##V>=4.14##    of adjacent duplicate elements of the sequence [xs].
##V>=4.14##
##V>=4.14##    Every element of [group eq xs] is a nonempty sequence of equal elements.
##V>=4.14##
##V>=4.14##    The concatenation [concat (group eq xs)] is equal to [xs].
##V>=4.14##
##V>=4.14##    Consuming [group eq xs], and consuming the sequences that it contains,
##V>=4.14##    can cause [xs] to be consumed more than once. Therefore, [xs] must be
##V>=4.14##    persistent.
##V>=4.14##
##V>=4.14##    @since 4.14 *)

##V>=4.14##val memoize : 'a t -> 'a t
##V>=4.14##(** The sequence [memoize xs] has the same elements as the sequence [xs].
##V>=4.14##
##V>=4.14##    Regardless of whether [xs] is ephemeral or persistent,
##V>=4.14##    [memoize xs] is persistent: even if it is queried several times,
##V>=4.14##    [xs] is queried at most once.
##V>=4.14##
##V>=4.14##    The construction of the sequence [memoize xs] internally relies on
##V>=4.14##    suspensions provided by the module {!Lazy}. These suspensions are
##V>=4.14##    {i not} thread-safe. Therefore, the sequence [memoize xs]
##V>=4.14##    must {i not} be queried by multiple threads concurrently.
##V>=4.14##
##V>=4.14##    @since 4.14 *)

##V>=4.14##exception Forced_twice
##V>=4.14##(** This exception is raised when a sequence returned by {!once}
##V>=4.14##    (or a suffix of it) is queried more than once.
##V>=4.14##
##V>=4.14##    @since 4.14 *)

##V>=4.14##val once : 'a t -> 'a t
##V>=4.14##(** The sequence [once xs] has the same elements as the sequence [xs].
##V>=4.14##
##V>=4.14##    Regardless of whether [xs] is ephemeral or persistent,
##V>=4.14##    [once xs] is an ephemeral sequence: it can be queried at most once.
##V>=4.14##    If it (or a suffix of it) is queried more than once, then the exception
##V>=4.14##    [Forced_twice] is raised. This can be useful, while debugging or testing,
##V>=4.14##    to ensure that a sequence is consumed at most once.
##V>=4.14##
##V>=4.14##    @raise Forced_twice if [once xs], or a suffix of it,
##V>=4.14##           is queried more than once.
##V>=4.14##
##V>=4.14##    @since 4.14 *)

##V>=4.14##val transpose : 'a t t -> 'a t t
##V>=4.14##(** If [xss] is a matrix (a sequence of rows), then [transpose xss] is
##V>=4.14##    the sequence of the columns of the matrix [xss].
##V>=4.14##
##V>=4.14##    The rows of the matrix [xss] are not required to have the same length.
##V>=4.14##
##V>=4.14##    The matrix [xss] is not required to be finite (in either direction).
##V>=4.14##
##V>=4.14##    The matrix [xss] must be persistent.
##V>=4.14##
##V>=4.14##    @since 4.14 *)

##V>=4.14##val zip : 'a t -> 'b t -> ('a * 'b) t
##V>=4.14##(** [zip xs ys] is the sequence of pairs [(x, y)]
##V>=4.14##    drawn synchronously from the sequences [xs] and [ys].
##V>=4.14##
##V>=4.14##    If the sequences [xs] and [ys] have different lengths, then
##V>=4.14##    the sequence ends as soon as one sequence is exhausted;
##V>=4.14##    the excess elements in the other sequence are ignored.
##V>=4.14##
##V>=4.14##    [zip xs ys] is equivalent to [map2 (fun a b -> (a, b)) xs ys].
##V>=4.14##
##V>=4.14##    @since 4.14 *)

##V>=4.14##val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
##V>=4.14##(** [map2 f xs ys] is the sequence of the elements [f x y],
##V>=4.14##    where the pairs [(x, y)] are drawn synchronously from the
##V>=4.14##    sequences [xs] and [ys].
##V>=4.14##
##V>=4.14##    If the sequences [xs] and [ys] have different lengths, then
##V>=4.14##    the sequence ends as soon as one sequence is exhausted;
##V>=4.14##    the excess elements in the other sequence are ignored.
##V>=4.14##
##V>=4.14##    [map2 f xs ys] is equivalent to [map (fun (x, y) -> f x y) (zip xs ys)].
##V>=4.14##
##V>=4.14##    @since 4.14 *)

##V>=4.14##val interleave : 'a t -> 'a t -> 'a t
##V>=4.14##(** [interleave xs ys] is the sequence that begins with the first element of
##V>=4.14##    [xs], continues with the first element of [ys], and so on.
##V>=4.14##
##V>=4.14##    When one of the sequences [xs] and [ys] is exhausted,
##V>=4.14##    [interleave xs ys] continues with the rest of the other sequence.
##V>=4.14##
##V>=4.14##    @since 4.14 *)

##V>=4.14##val sorted_merge : ('a -> 'a -> int) -> 'a t -> 'a t -> 'a t
##V>=4.14##(** If the sequences [xs] and [ys] are sorted according to the total preorder
##V>=4.14##    [cmp], then [sorted_merge cmp xs ys] is the sorted sequence obtained by
##V>=4.14##    merging the sequences [xs] and [ys].
##V>=4.14##
##V>=4.14##    For more details on comparison functions, see {!Array.sort}.
##V>=4.14##
##V>=4.14##    @since 4.14 *)

##V>=4.14##val product : 'a t -> 'b t -> ('a * 'b) t
##V>=4.14##(** [product xs ys] is the Cartesian product of the sequences [xs] and [ys].
##V>=4.14##
##V>=4.14##    For every element [x] of [xs] and for every element [y] of [ys],
##V>=4.14##    the pair [(x, y)] appears once as an element of [product xs ys].
##V>=4.14##
##V>=4.14##    The order in which the pairs appear is unspecified.
##V>=4.14##
##V>=4.14##    The sequences [xs] and [ys] are not required to be finite.
##V>=4.14##
##V>=4.14##    The sequences [xs] and [ys] must be persistent.
##V>=4.14##
##V>=4.14##    @since 4.14 *)

##V>=4.14##val map_product : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
##V>=4.14##(** The sequence [map_product f xs ys] is the image through [f]
##V>=4.14##    of the Cartesian product of the sequences [xs] and [ys].
##V>=4.14##
##V>=4.14##    For every element [x] of [xs] and for every element [y] of [ys],
##V>=4.14##    the element [f x y] appears once as an element of [map_product f xs ys].
##V>=4.14##
##V>=4.14##    The order in which these elements appear is unspecified.
##V>=4.14##
##V>=4.14##    The sequences [xs] and [ys] are not required to be finite.
##V>=4.14##
##V>=4.14##    The sequences [xs] and [ys] must be persistent.
##V>=4.14##
##V>=4.14##    [map_product f xs ys] is equivalent to
##V>=4.14##    [map (fun (x, y) -> f x y) (product xs ys)].
##V>=4.14##
##V>=4.14##    @since 4.14 *)

##V>=4.14##val unzip : ('a * 'b) t -> 'a t * 'b t
##V>=4.14##(** [unzip] transforms a sequence of pairs into a pair of sequences.
##V>=4.14##
##V>=4.14##    [unzip xs] is equivalent to [(map fst xs, map snd xs)].
##V>=4.14##
##V>=4.14##    Querying either of the sequences returned by [unzip xs]
##V>=4.14##    causes [xs] to be queried.
##V>=4.14##    Therefore, querying both of them
##V>=4.14##    causes [xs] to be queried twice.
##V>=4.14##    Thus, [xs] must be persistent and cheap.
##V>=4.14##    If that is not the case, use [unzip (memoize xs)].
##V>=4.14##
##V>=4.14##    @since 4.14 *)

##V>=4.14##val split : ('a * 'b) t -> 'a t * 'b t
##V>=4.14##(** [split] is an alias for [unzip].
##V>=4.14##
##V>=4.14##    @since 4.14 *)

##V>=4.14##val partition_map : ('a -> ('b, 'c) Either.t) -> 'a t -> 'b t * 'c t
##V>=4.14##(** [partition_map f xs] returns a pair of sequences [(ys, zs)], where:
##V>=4.14##
##V>=4.14##    - [ys] is the sequence of the elements [y] such that
##V>=4.14##      [f x = Left y], where [x] ranges over [xs];
##V>=4.14##
##V>=4.14##    - [zs] is the sequence of the elements [z] such that
##V>=4.14##      [f x = Right z], where [x] ranges over [xs].
##V>=4.14##
##V>=4.14##    [partition_map f xs] is equivalent to a pair of
##V>=4.14##    [filter_map Either.find_left (map f xs)] and
##V>=4.14##    [filter_map Either.find_right (map f xs)].
##V>=4.14##
##V>=4.14##    Querying either of the sequences returned by [partition_map f xs]
##V>=4.14##    causes [xs] to be queried.
##V>=4.14##    Therefore, querying both of them
##V>=4.14##    causes [xs] to be queried twice.
##V>=4.14##    Thus, [xs] must be persistent and cheap.
##V>=4.14##    If that is not the case, use [partition_map f (memoize xs)].
##V>=4.14##
##V>=4.14##    @since 4.14 *)

##V>=4.14##val partition : ('a -> bool) -> 'a t -> 'a t * 'a t
##V>=4.14##(** [partition p xs] returns a pair of the subsequence of the elements
##V>=4.14##    of [xs] that satisfy [p] and the subsequence of the elements of
##V>=4.14##    [xs] that do not satisfy [p].
##V>=4.14##
##V>=4.14##    [partition p xs] is equivalent to
##V>=4.14##    [filter p xs, filter (fun x -> not (p x)) xs].
##V>=4.14##
##V>=4.14##    Consuming both of the sequences returned by [partition p xs] causes
##V>=4.14##    [xs] to be consumed twice and causes the function [f] to be applied
##V>=4.14##    twice to each element of the list.
##V>=4.14##    Therefore, [f] should be pure and cheap.
##V>=4.14##    Furthermore, [xs] should be persistent and cheap.
##V>=4.14##    If that is not the case, use [partition p (memoize xs)].
##V>=4.14##
##V>=4.14##    @since 4.14 *)

##V>=4.14##(** {1 Converting between sequences and dispensers} *)
##V>=4.14##
##V>=4.14##(** A dispenser is a representation of a sequence as a function of type
##V>=4.14##    [unit -> 'a option]. Every time this function is invoked, it returns
##V>=4.14##    the next element of the sequence. When there are no more elements,
##V>=4.14##    it returns [None]. A dispenser has mutable internal state, therefore
##V>=4.14##    is ephemeral: the sequence that it represents can be consumed at most
##V>=4.14##    once. *)
##V>=4.14##
##V>=4.14##val of_dispenser : (unit -> 'a option) -> 'a t
##V>=4.14##(** [of_dispenser it] is the sequence of the elements produced by the
##V>=4.14##    dispenser [it]. It is an ephemeral sequence: it can be consumed at most
##V>=4.14##    once. If a persistent sequence is needed, use
##V>=4.14##    [memoize (of_dispenser it)].
##V>=4.14##
##V>=4.14##    @since 4.14 *)

##V>=4.14##val to_dispenser : 'a t -> (unit -> 'a option)
##V>=4.14##(** [to_dispenser xs] is a fresh dispenser on the sequence [xs].
##V>=4.14##
##V>=4.14##    This dispenser has mutable internal state,
##V>=4.14##    which is not protected by a lock;
##V>=4.14##    so, it must not be used by several threads concurrently.
##V>=4.14##
##V>=4.14##    @since 4.14 *)

##V>=4.14##(** {1 Sequences of integers} *)

##V>=4.14##val ints : int -> int t
##V>=4.14##(** [ints i] is the infinite sequence of the integers beginning at [i] and
##V>=4.14##    counting up.
##V>=4.14##
##V>=4.14##    @since 4.14 *)
##V>=4.14##
