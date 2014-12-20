(*
Copyright (c) 2013, Simon Cruanes
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

Redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.  Redistributions in binary
form must reproduce the above copyright notice, this list of conditions and the
following disclaimer in the documentation and/or other materials provided with
the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

(** {1 Generators}

Values of type ['a Gen.t] represent a possibly infinite sequence of values
of type 'a. One can only iterate once on the sequence, as it is consumed
by iteration/deconstruction/access. [None] is returned when the generator
is exhausted.

The submodule {!Restart} provides utilities to work with
{b restartable generators}, that is, functions [unit -> 'a Gen.t] that
allow to build as many generators from the same source as needed.
*)

(** {2 Global type declarations} *)

type 'a t = unit -> 'a option
  (** A generator may be called several times, yielding the next value
      each time. It returns [None] when no elements remain *)

type 'a gen = 'a t

type 'a restart_gen = unit -> 'a t
(** Restartable generator. Every time it is invoked with [()], a new
    generator is created and can be consumed. All created generators
    {b must} be equivalent. *)

module type Generable = sig
  type 'a generable

  val gen : 'a generable -> 'a gen
  val of_gen : 'a gen -> 'a generable
end

(** {2 Common signature for transient and restartable generators}

The signature {!S} abstracts on a type ['a t], where the [t] can be
the type of transient or restartable generators. Some functions specify
explicitely that they use ['a gen] (transient generators). *)

module type S = sig
  type 'a t

  val empty : 'a t
    (** Empty generator, with no elements *)

  val singleton : 'a -> 'a t
    (** One-element generator *)

  val repeat : 'a -> 'a t
    (** Repeat same element endlessly *)

  val iterate : 'a -> ('a -> 'a) -> 'a t
    (** [iterate x f] is [[x; f x; f (f x); f (f (f x)); ...]] *)

  val unfold : ('b -> ('a * 'b) option) -> 'b -> 'a t
    (** Dual of {!fold}, with a deconstructing operation. It keeps on
        unfolding the ['b] value into a new ['b], and a ['a] which is yielded,
        until [None] is returned. *)

  val init : ?limit:int -> (int -> 'a) -> 'a t
    (** Calls the function, starting from 0, on increasing indices.
        If [limit] is provided and is a positive int, iteration will
        stop at the limit (excluded).
        For instance [init ~limit:4 id] will yield 0, 1, 2, and 3. *)

  val seq : 'a -> ('a -> 'a) -> ('a -> bool) -> 'a t
  (** [seq init step cond] creates a sequence of data, which starts
      from [init],  extends by [step],  until the condition [cond]
      fails. E.g. [seq 1 ((+) 1) ((>) 100)] returns [1, 2, ... 99]. If [cond
      init] is false, the result is empty. *)

  (** {2 Basic combinators} *)

  val is_empty : _ t -> bool
    (** Check whether the gen is empty. *)

  val fold : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b
    (** Fold on the generator, tail-recursively *)

  val reduce : ('a -> 'a -> 'a) -> 'a t -> 'a
    (** Fold on non-empty sequences (otherwise raise Invalid_argument) *)

  val scan : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b t
    (** Like {!fold}, but keeping successive values of the accumulator *)

  val iter : ('a -> unit) -> 'a t -> unit
    (** Iterate on the gen *)

  val iteri : (int -> 'a -> unit) -> 'a t -> unit
    (** Iterate on elements with their index in the gen, from 0 *)

  val length : _ t -> int
    (** Length of an gen (linear time) *)

  val count : _ t -> int
    (** Alias to {!length} *)

  val map : ('a -> 'b) -> 'a t -> 'b t
    (** Lazy map. No iteration is performed now, the function will be called
        when the result is traversed. *)

  val append : 'a t -> 'a t -> 'a t
    (** Append the two gens; the result contains the elements of the first,
        then the elements of the second gen. *)

  val flatten : 'a gen t -> 'a t
    (** Flatten the enumeration of generators *)

  val flat_map : ('a -> 'b gen) -> 'a t -> 'b t
    (** Monadic bind; each element is transformed to a sub-gen
        which is then iterated on, before the next element is processed,
        and so on. *)

  val mem : ?eq:('a -> 'a -> bool) -> 'a -> 'a t -> bool
    (** Is the given element, member of the gen? *)

  val take : int -> 'a t -> 'a t
    (** Take at most n elements *)

  val drop : int -> 'a t -> 'a t
    (** Drop n elements *)

  val nth : int -> 'a t -> 'a
    (** n-th element, or Not_found
        @raise Not_found if the generator contains less than [n] arguments *)

  val take_nth : int -> 'a t -> 'a t
    (** [take_nth n g] returns every element of [g] whose index
        is a multiple of [n]. For instance [take_nth 2 (1--10) |> to_list]
        will return [1;3;5;7;9] *)

  val filter : ('a -> bool) -> 'a t -> 'a t
    (** Filter out elements that do not satisfy the predicate.  *)

  val take_while : ('a -> bool) -> 'a t -> 'a t
    (** Take elements while they satisfy the predicate *)

  val drop_while : ('a -> bool) -> 'a t -> 'a t
    (** Drop elements while they satisfy the predicate *)

  val filter_map : ('a -> 'b option) -> 'a t -> 'b t
    (** Maps some elements to 'b, drop the other ones *)

  val zip_index : 'a t -> (int * 'a) t
    (** Zip elements with their index in the gen *)

  val unzip : ('a * 'b) t -> 'a t * 'b t
    (** Unzip into two sequences, splitting each pair *)

  val partition : ('a -> bool) -> 'a t -> 'a t * 'a t
    (** [partition p l] returns the elements that satisfy [p],
        and the elements that do not satisfy [p] *)

  val for_all : ('a -> bool) -> 'a t -> bool
    (** Is the predicate true for all elements? *)

  val exists : ('a -> bool) -> 'a t -> bool
    (** Is the predicate true for at least one element? *)

  val min : ?lt:('a -> 'a -> bool) -> 'a t -> 'a
    (** Minimum element, according to the given comparison function.
        @raise Invalid_argument if the generator is empty *)

  val max : ?lt:('a -> 'a -> bool) -> 'a t -> 'a
    (** Maximum element, see {!min}
        @raise Invalid_argument if the generator is empty *)

  val eq : ?eq:('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    (** Equality of generators. *)

  val lexico : ?cmp:('a -> 'a -> int) -> 'a t -> 'a t -> int
    (** Lexicographic comparison of generators. If a generator is a prefix
        of the other one, it is considered smaller. *)

  val compare : ?cmp:('a -> 'a -> int) -> 'a t -> 'a t -> int
    (** Synonym for {! lexico} *)

  val find : ('a -> bool) -> 'a t -> 'a option
    (** [find p e] returns the first element of [e] to satisfy [p],
        or None. *)

  val sum : int t -> int
    (** Sum of all elements *)

  (** {2 Multiple iterators} *)

  val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
    (** Map on the two sequences. Stops once one of them is exhausted.*)

  val iter2 : ('a -> 'b -> unit) -> 'a t -> 'b t -> unit
    (** Iterate on the two sequences. Stops once one of them is exhausted.*)

  val fold2 : ('acc -> 'a -> 'b -> 'acc) -> 'acc -> 'a t -> 'b t -> 'acc
    (** Fold the common prefix of the two iterators *)

  val for_all2 : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool
    (** Succeeds if all pairs of elements satisfy the predicate.
        Ignores elements of an iterator if the other runs dry. *)

  val exists2 : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool
    (** Succeeds if some pair of elements satisfy the predicate.
        Ignores elements of an iterator if the other runs dry. *)

  val zip_with : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
    (** Combine common part of the gens (stops when one is exhausted) *)

  val zip : 'a t -> 'b t -> ('a * 'b) t
    (** Zip together the common part of the gens *)

  val combine : ('a t * 'b t) -> ('a * 'b) t
    (** Uncurried version of {! zip} *)

  (** {2 Complex combinators} *)

  val merge : 'a gen t -> 'a t
    (** Pick elements fairly in each sub-generator. The merge of gens
        [e1, e2, ... ] picks elements in [e1], [e2],
        in [e3], [e1], [e2] .... Once a generator is empty, it is skipped;
        when they are all empty, and none remains in the input,
        their merge is also empty. 
        For instance, [merge [1;3;5] [2;4;6]] will be, in disorder, [1;2;3;4;5;6]. *)

  val intersection : ?cmp:('a -> 'a -> int) -> 'a t -> 'a t -> 'a t
    (** Intersection of two sorted sequences. Only elements that occur in both
        inputs appear in the output *)

  val sorted_merge : ?cmp:('a -> 'a -> int) -> 'a t -> 'a t -> 'a t
    (** Merge two sorted sequences into a sorted sequence *)

  val sorted_merge_n : ?cmp:('a -> 'a -> int) -> 'a t list -> 'a t
    (** Sorted merge of multiple sorted sequences *)

  val tee : ?n:int -> 'a t -> 'a gen list
    (** Duplicate the gen into [n] generators (default 2). The generators
        share the same underlying instance of the gen, so the optimal case is
        when they are consumed evenly *)

  val round_robin : ?n:int -> 'a t -> 'a gen list
    (** Split the gen into [n] generators in a fair way. Elements with
        [index = k mod n] with go to the k-th gen. [n] default value
        is 2. *)

  val interleave : 'a t -> 'a t -> 'a t
    (** [interleave a b] yields an element of [a], then an element of [b],
        and so on. When a generator is exhausted, this behaves like the
        other generator. *)

  val intersperse : 'a -> 'a t -> 'a t
    (** Put the separator element between all elements of the given gen *)

  val product : 'a t -> 'b t -> ('a * 'b) t
    (** Cartesian product, in no predictable order. Works even if some of the
        arguments are infinite. *)

  val group : ?eq:('a -> 'a -> bool) -> 'a t -> 'a list t
    (** Group equal consecutive elements together. *)

  val group_by : ('a -> 'b) -> 'a t -> 'a list t
    (** group together consecutive elements that have the same
        image by the given function *)

  val uniq : ?eq:('a -> 'a -> bool) -> 'a t -> 'a t
    (** Remove consecutive duplicate elements. Basically this is
        like [fun e -> map List.hd (group e)]. *)

  val sort : ?cmp:('a -> 'a -> int) -> 'a t -> 'a t
    (** Sort according to the given comparison function. The gen must be finite. *)

  val sort_uniq : ?cmp:('a -> 'a -> int) -> 'a t -> 'a t
    (** Sort and remove duplicates. The gen must be finite. *)

  val chunks : int -> 'a t -> 'a array t
    (** [chunks n e] returns a generator of arrays of length [n], composed
        of successive elements of [e]. The last array may be smaller
        than [n] *)

  (* TODO later
  val permutations : 'a t -> 'a gen t
    (** Permutations of the gen. Each permutation becomes unavailable once
        the next one is produced. *)

  val combinations : int -> 'a t -> 'a t t
    (** Combinations of given length. *)

  val powerSet : 'a t -> 'a t t
    (** All subsets of the gen (in no particular order) *)
  *)

  (** {2 Basic conversion functions} *)

  val of_list : 'a list -> 'a t
    (** Enumerate elements of the list *)

  val to_list : 'a t -> 'a list
    (** non tail-call trasnformation to list, in the same order *)

  val to_rev_list : 'a t -> 'a list
    (** Tail call conversion to list, in reverse order (more efficient) *)

  val to_array : 'a t -> 'a array
    (** Convert the gen to an array (not very efficient) *)

  val of_array : ?start:int -> ?len:int -> 'a array -> 'a t
    (** Iterate on (a slice of) the given array *)

  val rand_int : int -> int t
    (** Random ints in the given range. *)

  val int_range : int -> int -> int t
    (** [int_range a b] enumerates integers between [a] and [b], included. [a]
        is assumed to be smaller than [b]. *)

  module Infix : sig
    val (--) : int -> int -> int t
      (** Synonym for {! int_range} *)

    val (>>=) : 'a t -> ('a -> 'b gen) -> 'b t
      (** Monadic bind operator *)
  end

  val (--) : int -> int -> int t
    (** Synonym for {! int_range} *)

  val (>>=) : 'a t -> ('a -> 'b gen) -> 'b t
    (** Monadic bind operator *)

  val pp : ?start:string -> ?stop:string -> ?sep:string -> ?horizontal:bool ->
           (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
    (** Pretty print the content of the generator on a formatter. *)
end

(** {2 Transient generators} *)

val get : 'a t -> 'a option
  (** Get the next value *)

val next : 'a t -> 'a option
  (** Synonym for {!get} *)

val get_exn : 'a t -> 'a
  (** Get the next value, or fails
      @raise Invalid_argument if no element remains *)

val junk : 'a t -> unit
  (** Drop the next value, discarding it. *)

val repeatedly : (unit -> 'a) -> 'a t
  (** Call the same function an infinite number of times (useful for instance
      if the function is a random generator). *)

include S with type 'a t := 'a gen
  (** Operations on {b transient} generators *)

(** {2 Restartable generators} *)

module Restart : sig
  type 'a t = unit -> 'a gen

  type 'a restartable = 'a t

  include S with type 'a t := 'a restartable

  val cycle : 'a t -> 'a t
    (** Cycle through the gen, endlessly. The gen must not be empty. *)

  val lift : ('a gen -> 'b) -> 'a t -> 'b

  val lift2 : ('a gen -> 'b gen -> 'c) -> 'a t -> 'b t -> 'c
end

(** {2 Utils} *)

val persistent : 'a t -> 'a Restart.t
  (** Store content of the transient generator in memory, to be able to iterate
      on it several times later. If possible, consider using combinators
      from {!Restart} directly instead. *)

val start : 'a Restart.t -> 'a t
  (** Create a new transient generator.
      [start gen] is the same as [gen ()] but is included for readability. *)

val print : ?first:string -> ?last:string -> ?sep:string ->
            ('a BatInnerIO.output -> 'b -> 'c) ->
            'a BatInnerIO.output -> 'b gen -> unit
(** Pretty-printing of a generator *)

val suffix_action : (unit -> unit) -> 'a t -> 'a t
(** Same generator, but once it's exhausted the given action will
    be executed once *)
