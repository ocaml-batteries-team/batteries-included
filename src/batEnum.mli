(*
 * BatEnum - enumeration over abstract collection of elements.
 * Copyright (C) 2003 Nicolas Cannasse
 *               2009 David Rajchenbach-Teller, LIFO, Universite d'Orleans
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
    Enumeration over abstract collection of elements.

    Enumerations are a representation of finite or infinite sequences
    of elements. In Batteries Included, enumerations are used
    pervasively, both as a uniform manner of reading and manipulating
    the contents of a data structure, or as a simple manner of reading
    or writing sequences of characters, numbers, strings, etc. from/to
    files, network connections or other inputs/outputs.

    Enumerations are typically computed as needed, which allows the
    definition and manipulation of huge (possibly infinite) sequences.
    Manipulating an enumeration is a uniform and often comfortable way
    of extracting subsequences (function {!filter} or operator [//] et
    al), converting sequences into other sequences (function {!map} or
    operators [/@] and [@/] et al), gathering information (function
    {!scanl} et al) or performing loops (functions {!iter} and
    {!map}).

    For instance, function {!BatRandom.enum_int} creates an
    infinite enumeration of random numbers. Combined with [//]
    and {!map}, we may turn this into an infinite enumeration of
    squares of random even numbers:
    [map (fun x -> x * x) ( (Random.enum_int 100) // even )]

    Similarly, to obtain an enumeration of 50 random integers,
    we may use {!take}, as follows:
    [take 50 (Random.enum_int 100)]

    As most data structures in Batteries can be enumerated and built
    from enumerations, these operations may be used also on lists,
    arrays, hashtables, etc. When designing a new data structure, it
    is usuallly a good idea to allow enumeration and construction
    from an enumeration.

    {b Note} Enumerations are not thread-safe. You should not attempt
    to access one enumeration from different threads.

    @author Nicolas Cannasse
    @author David Rajchenbach-Teller
*)

type 'a t

(** A signature for data structures which may be converted to and from [enum].

    If you create a new data structure, you should make it compatible
    with [Enumerable].
*)
module type Enumerable = sig
  type 'a enumerable (** The data structure, e.g. ['a List.t] *)

  val enum : 'a enumerable -> 'a t
    (** Return an enumeration of the elements of the data structure *)

  val of_enum : 'a t -> 'a enumerable
    (** Build a data structure from an enumeration *)
end

include Enumerable with type 'a enumerable = 'a t
include BatInterfaces.Mappable with type 'a mappable = 'a t


(** {6 Final functions}

 These functions consume the enumeration until
 it ends or an exception is raised by the first
 argument function.
*)

val iter : ('a -> unit) -> 'a t -> unit
(** [iter f e] calls the function [f] with each elements of [e] in turn. *)

val iter2 : ('a -> 'b -> unit) -> 'a t -> 'b t -> unit
(** [iter2 f e1 e2] calls the function [f] with the next elements of [e1] and
 [e2] repeatedly until one of the two enumerations ends. *)

val exists: ('a -> bool) -> 'a t -> bool
(** [exists f e] returns [true] if there is some [x] in [e] such
    that [f x]*)

val for_all: ('a -> bool) -> 'a t -> bool
(** [for_all f e] returns [true] if for every [x] in [e], [f x] is true*)

val fold : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b
(** A general loop on an enumeration.

    If [e] is empty, [fold f v e] returns [v]. Otherwise, [fold v e]
    returns [f (... (f (f v a0) a1) ...) aN] where [a0,a1..aN] are the
    elements of [e]. This function may be used, for instance, to
    compute the sum of all elements of an enumeration [e] as follows:
    [fold ( + ) 0 e]. Eager.
*)

val reduce : ('a -> 'a -> 'a) -> 'a t -> 'a
(** A simplified version of [fold], which uses the first element
    of the enumeration as a default value.

      [reduce f e] throws [Not_found] if [e] is empty, returns its only
      element if e is a singleton, otherwise [f (... (f (f a0 a1)
      a2)...) aN] where [a0,a1..aN] are the elements of [e]. *)

val sum : int t -> int
  (** [sum] returns the sum of the given int enum.  If the argument is
      empty, returns 0. Eager *)

val fold2 : ('a -> 'b -> 'c -> 'c) -> 'c -> 'a t -> 'b t -> 'c
  (** [fold2] is similar to [fold] but will fold over two enumerations at the
      same time until one of the two enumerations ends. *)

val scanl : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b t
(** A variant of [fold] producing an enumeration of its intermediate values.
    If [e] contains [x0], [x1], ..., [scanl f init e] is the enumeration
    containing [init], [f init x0], [f (f init x0) x1]... Lazy. *)

val scan : ('a -> 'a -> 'a) -> 'a t -> 'a t
(** [scan] is similar to [scanl] but without the [init] value: if [e]
    contains [x0], [x1], [x2] ..., [scan f e] is the enumeration containing
    [x0], [f x0 x1], [f (f x0 x1) x2]...

    For instance, [scan ( * ) (1 -- 10)] will produce an enumeration
    containing the successive values of the factorial function.*)


(** Indexed functions : these functions are similar to previous ones
 except that they call the function with one additional argument which
 is an index starting at 0 and incremented after each call to the function. *)

val iteri : (int -> 'a -> unit) -> 'a t -> unit

val iter2i : ( int -> 'a -> 'b -> unit) -> 'a t -> 'b t -> unit

val foldi : (int -> 'a -> 'b -> 'b) -> 'b -> 'a t -> 'b

val fold2i : (int -> 'a -> 'b -> 'c -> 'c) -> 'c -> 'a t -> 'b t -> 'c

(** {6 Useful functions} *)

val find : ('a -> bool) -> 'a t -> 'a
  (** [find f e] returns the first element [x] of [e] such that [f x] returns
      [true], consuming the enumeration up to and including the
      found element, or, raises [Not_found] if no such element exists
      in the enumeration, consuming the whole enumeration in the search.

      Since [find] (eagerly) consumes a prefix of the enumeration, it
      can be used several times on the same enumeration to find the
      next element. *)

val is_empty : 'a t -> bool
(** [is_empty e] returns true if [e] does not contains any element.
    Forces at most one element. *)

val peek : 'a t -> 'a option
  (** [peek e] returns [None] if [e] is empty or [Some x] where [x] is
      the next element of [e]. The element is not removed from the
      enumeration. *)

val get : 'a t -> 'a option
  (** [get e] returns [None] if [e] is empty or [Some x] where [x] is
      the next element of [e], in which case the element is removed
      from the enumeration. *)

val push : 'a t -> 'a -> unit
  (** [push e x] will add [x] at the beginning of [e]. *)

val junk : 'a t -> unit
  (** [junk e] removes the first element from the enumeration, if any. *)

val clone : 'a t -> 'a t
  (** [clone e] creates a new enumeration that is copy of [e]. If [e]
      is consumed by later operations, the clone will not get affected. *)

val force : 'a t -> unit
  (** [force e] forces the application of all lazy functions and the
      enumeration of all elements, exhausting the enumeration.

      An efficient intermediate data structure
      of enumerated elements is constructed and [e] will now enumerate over
      that data structure. *)

val take : int -> 'a t -> 'a t
  (** [take n e] returns the prefix of [e] of length [n], or [e]
      itself if [n] is greater than the length of [e] *)

val drop : int -> 'a t -> unit
(** [drop n e] removes the first [n] element from the enumeration, if any. *)

val skip: int -> 'a t -> 'a t
(** [skip n e] removes the first [n] element from the enumeration, if any,
    then returns [e].

    This function has the same behavior as [drop] but is often easier to
    compose with, e.g., [skip 5 |- take 3] is a new function which skips
    5 elements and then returns the next 3 elements.*)

val take_while : ('a -> bool) -> 'a t -> 'a t
  (** [take_while f e] produces a new enumeration in which only remain
      the first few elements [x] of [e] such that [f x] *)

val drop_while : ('a -> bool) -> 'a t -> 'a t
  (** [drop_while p e] produces a new enumeration in which only
      all the first elements such that [f e] have been junked.*)

val span : ('a -> bool) -> 'a t -> 'a t * 'a t
  (** [span test e] produces two enumerations [(hd, tl)], such that
      [hd] is the same as [take_while test e] and [tl] is the same
      as [drop_while test e]. *)

val break : ('a -> bool) -> 'a t -> 'a t * 'a t
  (** Negated span.
      [break test e] is equivalent to [span (fun x -> not (test x)) e] *)

val group : ('a -> 'b) -> 'a t -> 'a t t
(** [group test e] divides [e] into an enumeration of enumerations,
    where each sub-enumeration is the longest continuous enumeration
    of elements whose [test] results are the same.

    [Enum.group (x -> x mod 2) [1;2;4;1] = [[1];[2;4];[1]]]

    [Enum.group (fun x -> x mod 3) [1;2;4;1] = [[1];[2];[4;1]]]

    [Enum.group (fun s -> s.[0]) ["cat"; "canary"; "dog"; "dodo"; "ant"; "cow"] = [["cat"; "canary"];["dog";"dodo"];["ant"];["cow"]]]

*)

val group_by : ('a -> 'a -> bool) -> 'a t -> 'a t t
(** [group_by eq e] divides [e] into an enumeration of enumerations,
    where each sub-enumeration is the longest continuous enumeration
    of elements that are equal, as judged by [eq]
*)

val clump : int -> ('a -> unit) -> (unit -> 'b) -> 'a t -> 'b t
(** [clump size add get e] runs [add] on [size] (or less at the end)
    elements of [e] and then runs [get] to produce value for the
    result enumeration.  Useful to convert a char enum into string
    enum. *)

(** {6 Lazy constructors}

    These functions are lazy which means that they will create a new modified
    enumeration without actually enumerating any element until they are asked
    to do so by the programmer (using one of the functions above).

    When the resulting enumerations of these functions are consumed, the
    underlying enumerations they were created from are also consumed. *)

val map : ('a -> 'b) -> 'a t -> 'b t
  (** [map f e] returns an enumeration over [(f a0, f a1, ...)] where
      [a0,a1...] are the elements of [e]. Lazy.
   *)

val mapi : (int -> 'a -> 'b) -> 'a t -> 'b t
(** [mapi] is similar to [map] except that [f] is passed one extra argument
    which is the index of the element in the enumeration, starting from 0 :
    mapi f e returns an enumeration over [(f 0 a0, f 1 a1, ...)] where
    [a0,a1...] are the elements of [e]. *)

val filter : ('a -> bool) -> 'a t -> 'a t
  (** [filter f e] returns an enumeration over all elements [x] of [e] such
      as [f x] returns [true]. Lazy.

    {b Note} filter is lazy in that it returns a lazy enumeration, but
    each element in the result is eagerly searched in the input
    enumeration. Therefore, the access to a given element in the result
    will diverge if it is preceded, in the input enumeration, by
    infinitely many false elements (elements on which the predicate
    [p] returns [false]).

    Other functions that may drop an unbound number of elements
    ([filter_map], [take_while], etc.) have the same behavior.
*)

val filter_map : ('a -> 'b option) -> 'a t -> 'b t
(** [filter_map f e] returns an enumeration over all elements [x] such as
    [f y] returns [Some x] , where [y] is an element of [e].

    [filter_map] works on infinite enumerations; see [filter].
*)

val append : 'a t -> 'a t -> 'a t
  (** [append e1 e2] returns an enumeration that will enumerate over all
      elements of [e1] followed by all elements of [e2]. Lazy.

      {b Note} The behavior of appending [e] to itself or to something
      derived from [e] is not specified. In particular, cloning [append e e]
      may destroy any sharing between the first and the second argument.
  *)

val prefix_action : (unit -> unit) -> 'a t -> 'a t
(** [prefix_action f e] will behave as [e] but guarantees that [f ()]
    will be invoked exactly once before the current first element of [e]
    is read.

    If [prefix_action f e] is cloned, [f] is invoked only once, during
    the cloning. If [prefix_action f e] is counted, [f] is invoked
    only once, during the counting.

    May be used for signalling that reading starts or for performing
    delayed evaluations.*)

val suffix_action : (unit -> unit) -> 'a t -> 'a t
(** [suffix_action f e] will behave as [e] but guarantees that [f ()]
    will be invoked after the contents of [e] are exhausted.

    If [suffix_action f e] is cloned, [f] is invoked only once, when
    the original enumeration is exhausted. If [suffix_action f e]
    is counted, [f] is only invoked if the act of counting
    requires a call to [force].

    May be used for signalling that reading stopped or for performing
    delayed evaluations.*)


val concat : 'a t t -> 'a t
  (** [concat e] returns an enumeration over all elements of all enumerations
      of [e]. *)

val flatten : 'a t t -> 'a t
  (** Synonym of {!concat}*)

(** {6 Constructors}

    In this section the word {i shall} denotes a semantic
    requirement. The correct operation of the functions in this
    interface are conditional on the client meeting these
    requirements.
*)

exception No_more_elements
  (** This exception {i shall} be raised by the [next] function of [make]
      or [from] when no more elements can be enumerated, it {i shall not}
      be raised by any function which is an argument to any
      other function specified in the interface.
  *)

exception Infinite_enum
(** As a convenience for debugging, this exception {i may} be raised by
    the [count] function of [make] when attempting to count an infinite enum.*)

val empty : unit -> 'a t
(** The empty enumeration : contains no element *)

val make : next:(unit -> 'a) -> count:(unit -> int) -> clone:(unit -> 'a t) -> 'a t
(** This function creates a fully defined enumeration.

    {ul {li the [next] function {i shall} return the next element of the
    enumeration or raise [No_more_elements] if the underlying data structure
    does not have any more elements to enumerate.}
    {li the [count] function {i shall} return the actual number of remaining
    elements in the enumeration or {i may} raise [Infinite_enum] if it is known
    that the enumeration is infinite.}
    {li the [clone] function {i shall} create a clone of the enumeration
    such as operations on the original enumeration will not affect the
    clone. }}

    For some samples on how to correctly use [make], you can have a look
    at implementation of [BatList.enum].
*)

val from : (unit -> 'a) -> 'a t
  (** [from next] creates an enumeration from the [next] function.
      [next] {i shall} return the next element of the enumeration or raise
      [No_more_elements] when no more elements can be enumerated. Since the
      enumeration definition is incomplete, a call to [count] will result in
      a call to [force] that will enumerate all elements in order to
      return a correct value. *)

val from_while : (unit -> 'a option) -> 'a t
(** [from_while next] creates an enumeration from the [next] function.
    [next] {i shall} return [Some x] where [x] is the next element of the
    enumeration or [None] when no more elements can be enumerated. Since the
    enumeration definition is incomplete, a call to [clone] or [count] will
    result in a call to [force] that will enumerate all elements in order to
    return a correct value. *)

val from_loop: 'b -> ('b -> ('a * 'b)) -> 'a t
  (**[from_loop data next] creates a (possibly infinite) enumeration from
     the successive results of applying [next] to [data], then to the
     result, etc. The list ends whenever the function raises
     {!BatEnum.No_more_elements}*)

val seq : 'a -> ('a -> 'a) -> ('a -> bool) -> 'a t
  (** [seq init step cond] creates a sequence of data, which starts
      from [init],  extends by [step],  until the condition [cond]
      fails. E.g. [seq 1 ((+) 1) ((>) 100)] returns [1, 2, ... 99]. If [cond
      init] is false, the result is empty. *)


val unfold: 'b -> ('b -> ('a * 'b) option) -> 'a t
  (**As [from_loop], except uses option type to signal the end of the enumeration.

     [unfold data next] creates a (possibly infinite) enumeration from
     the successive results of applying [next] to [data], then to the
     result, etc. The enumeration ends whenever the function returns [None]

     Example: [Enum.unfold n (fun x -> if x = 1 then None else Some
     (x, if x land 1 = 1 then 3 * x + 1 else x / 2))] returns the
     hailstone sequence starting at [n].

  *)

val init : int -> (int -> 'a) -> 'a t
(** [init n f] creates a new enumeration over elements
  [f 0, f 1, ..., f (n-1)] *)

val singleton : 'a -> 'a t
(** Create an enumeration consisting in exactly one element.*)

val repeat : ?times:int -> 'a -> 'a t
  (** [repeat ~times:n x] creates a enum sequence filled with [n] times of
      [x]. It return infinite enum when [~times] is absent. It returns empty
      enum when [times <= 0] *)

val cycle : ?times:int -> 'a t -> 'a t
  (** [cycle] is similar to [repeat], except that the content to fill is a
      subenum rather than a single element. Note that [times] represents the
      times of repeating not the length of enum. *)

val delay : (unit -> 'a t) -> 'a t
  (** [delay (fun () -> e)] produces an enumeration which behaves as [e].
      The enumeration itself will only be computed when consumed.

      A typical use of this function is to explore lazily non-trivial
      data structures, as follows:

      [type 'a tree = Leaf
                    | Node of 'a * 'a tree * 'a tree

      let enum_tree =
      let rec aux = function
      | Leaf           -> BatEnum.empty ()
      | Node (n, l, r) -> BatEnum.append (BatEnum.singleton n)
      (BatEnum.append (delay (fun () -> aux l))
      (delay (fun () -> aux r)))
      ]

  *)

val to_object: 'a t -> (<next:'a; count:int; clone:'b> as 'b)
(**[to_object e] returns a representation of [e] as an object.*)

val of_object: (<next:'a; count:int; clone:'b> as 'b) -> 'a t
(**[of_object e] returns a representation of an object as an enumeration*)

val enum : 'a t -> 'a t
(** identity : added for consistency with the other data structures *)
val of_enum : 'a t -> 'a t
(** identity : added for consistency with the other data structures *)

(** {6 Counting} *)

val count : 'a t -> int
  (** [count e] returns the number of remaining elements in [e] without
      consuming the enumeration.

      Depending of the underlying data structure that is implementing the
      enumeration functions, the count operation can be costly, and even sometimes
      can cause a call to [force]. *)

val fast_count : 'a t -> bool
  (** For users worried about the speed of [count] you can call the [fast_count]
      function that will give an hint about [count] implementation. Basically, if
      the enumeration has been created with [make] or [init] or if [force] has
      been called on it, then [fast_count] will return true. *)

val hard_count : 'a t -> int
  (** [hard_count] returns the number of remaining in elements in [e],
      consuming the whole enumeration somewhere along the way. This
      function is always at least as fast as the fastest of either
      [count] or a [fold] on the elements of [t].

      This function is useful when you have opened an enumeration for
      the sole purpose of counting its elements (e.g. the number of
      lines in a file).*)

(**
   {6 Utilities }
*)

val range : ?until:int -> int -> int t
(** [range p until:q] creates an enumeration of integers [[p, p+1, ..., q]].
    If [until] is omitted, the enumeration is not bounded. Behaviour is
    not-specified once [max_int] has been reached.*)

val dup : 'a t -> 'a t * 'a t
  (** [dup stream] returns a pair of streams which are identical to [stream]. Note
      that stream is a destructive data structure, the point of [dup] is to
      return two streams can be used independently. *)

val combine : 'a t * 'b t -> ('a * 'b) t
  (** [combine] transform a pair of stream into a stream of pairs of corresponding
      elements. If one stream is short, excess elements of the longer stream are
      ignored. *)

val uncombine : ('a * 'b) t -> 'a t * 'b t
  (** [uncombine] is the opposite of [combine] *)

val merge : ('a -> 'a -> bool) -> 'a t -> 'a t -> 'a t
  (** [merge test (a, b)] merge the elements from [a] and [b] into a single
      enumeration. At each step, [test] is applied to the first element of
      [a] and the first element of [b] to determine which should get first
      into resulting enumeration. If [a] or [b] runs out of elements,
      the process will append all elements of the other enumeration to
      the result.
  *)

val uniq : 'a t -> 'a t
  (** [uniq e] returns a duplicate of [e] with repeated values
      omitted. (similar to unix's [uniq] command) *)

val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
  (** [compare cmp a b] compares enumerations [a] and [b]
      by lexicographical order using comparison [cmp].

      @return 0 if [a] and [b] are equal wrt [cmp]
      @return -1 if [a] is empty and [b] is not
      @return 1 if [b] is empty and [a] is not
      @return [cmp x y], where [x] is the first element of [a]
      and [y] is the first element of [b], if [cmp x y <> 0]
      @return [compare cmp a' b'], where [a'] and [b'] are
      respectively equal to [a] and [b] without their first
      element, if both [a] and [b] are non-empty and [cmp x y = 0],
      where [x] is the first element of [a] and [y] is the first
      element of [b]
  *)

val switch : ('a -> bool) -> 'a t -> 'a t * 'a t
  (** [switch test enum] splits [enum] into two enums, where the first enum have
      all the elements satisfying [test], the second enum is opposite. The
      order of elements in the source enum is preserved. *)

val partition : ('a -> bool) -> 'a t -> 'a t * 'a t
  (** as [switch]

      @added v1.4.0
  *)

(*val switchn: int -> ('a -> int) -> 'a t -> 'a t array
  (** [switchn] is the array version of [switch]. [switch n f fl] split [fl] to an array of [n] enums, [f] is
      applied to each element of [fl] to decide the id of its destination
      enum. *)*)

val arg_min : ('a -> 'b) -> 'a t -> 'a
val arg_max : ('a -> 'b) -> 'a t -> 'a
(** [arg_min f xs] returns the [x] in [xs] for which [f x] is minimum.
    Similarly for [arg_max], except it returns the maximum.  If
    multiple values reach the maximum, one of them is
    returned. (currently the first, but this is not guaranteed)

    Example: [-5 -- 5 |> arg_min (fun x -> x * x + 6 * x - 5) = -3]
    Example: [List.enum ["cat"; "canary"; "dog"; "dodo"; "ant"; "cow"] |> arg_max String.length = "canary"]

    @added v1.4.0
    @raises Invalid_argument if the input enum is empty
*)

(** {6 Trampolining} *)

val while_do : ('a -> bool) -> ('a t -> 'a t) -> 'a t -> 'a t
(** [while_do cont f e] is a loop on [e] using [f] as body and [cont] as
    condition for continuing.

    If [e] contains elements [x0], [x1], [x2]..., then if [cont x0] is [false],
    [x0] is returned as such and treatment stops. On the other hand, if [cont x0]
    is [true], [f x0] is returned and the loop proceeds with [x1]...

    Note that f is used as halting condition {i after} the
    corresponding element has been added to the result stream.
*)

(** {6 Infix operators} *)

(** Infix versions of some functions

	This module groups together all infix operators so that
    you can open it without opening the whole batEnum module.
*)

module Infix : sig
  val ( -- ) : int -> int -> int t
(** As [range], without the label.

    [5 -- 10] is the enumeration 5,6,7,8,9,10.
    [10 -- 5] is the empty enumeration*)

  val ( --^ ) : int -> int -> int t
(** As [(--)] but without the right endpoint

    [5 --^ 10] is the enumeration 5,6,7,8,9.
*)

  val ( --. ) : (float * float) -> float -> float t
(** [(a, step) --. b)] creates a float enumeration from [a] to [b] with an
    increment of [step] between elements.

    [(5.0, 1.0) --. 10.0] is the enumeration 5.0,6.0,7.0,8.0,9.0,10.0.
    [(10.0, -1.0) --. 5.0] is the enumeration 10.0,9.0,8.0,7.0,6.0,5.0.
    [(10.0, 1.0) --. 1.0] is the empty enumeration. *)

  val ( --- ) : int -> int -> int t
(** As [--], but accepts enumerations in reverse order.

    [5 --- 10] is the enumeration 5,6,7,8,9,10.
    [10 --- 5] is the enumeration 10,9,8,7,6,5.*)

  val ( --~ ) : char -> char -> char t
(** As ( -- ), but for characters.*)

  val ( // ) : 'a t -> ('a -> bool) -> 'a t
(** Filtering (pronounce this operator name "such that").

    For instance, [(1 -- 37) // odd] is the enumeration of all odd
    numbers between 1 and 37.*)

  val ( /@ ) : 'a t -> ('a -> 'b) -> 'b t

  val ( @/ ) : ('a -> 'b) -> 'a t -> 'b t
  (**
     Mapping operators.

     These operators have the same meaning as function {!map} but are
     sometimes more readable than this function, when chaining
     several transformations in a row.
  *)

  val ( //@ ) : 'a t -> ('a -> 'b option) -> 'b t

  val ( @// ) : ('a -> 'b option) -> 'a t -> 'b t
  (**
    Map combined with filter. Same as {!filter_map}.
  *)
end

val ( -- ) : int -> int -> int t
val ( --^ ) : int -> int -> int t
val ( --. ) : (float * float) -> float -> float t
val ( --- ) : int -> int -> int t
val ( --~ ) : char -> char -> char t
val ( // ) : 'a t -> ('a -> bool) -> 'a t
val ( /@ ) : 'a t -> ('a -> 'b) -> 'b t
val ( @/ ) : ('a -> 'b) -> 'a t -> 'b t
val ( //@ ) : 'a t -> ('a -> 'b option) -> 'b t
val ( @// ) : ('a -> 'b option) -> 'a t -> 'b t

(** {6 Monad related modules} *)

(** Monadic operations on Enumerations containing monadic elements

    This module will let you use sequence and fold_monad functions over enumerations.
*)
module WithMonad : functor (Mon : BatMonad.S) -> sig
  type 'a m = 'a Mon.m
(** Type of the monadic elements. *)

  val sequence : 'a m t -> 'a t m
(** [sequence e] evaluates each monadic elements (of type ['a m]) contained in the enumeration [e] to get a monadic enumeration of ['a] elements,
    of type ['a BatEnum.t m]. *)

  val fold_monad : ('a -> 'b -> 'a m) -> 'a -> 'b t -> 'a m
(** [fold_monad f init e] does a folding of the enumeration [e] applying step by step the function [f] that gives back results in the [Mon] monad,
    with the [init] initial element. The result is a value in the [Mon] monad. *)
end

(** The BatEnum Monad

    This module provides everything needed for writing and executing
    computations in the BatEnum Monad.
*)
module Monad : sig
  type 'a m = 'a t
(** The type of the BatEnum monad's elements, thus [BatEnum.t]. *)

  val return : 'a -> 'a m
(** This function puts a single value in the BatEnum monad, that is to say it creates an enumeration containing a single element. *)

  val bind : 'a m -> ('a -> 'b m) -> 'b m
(** [bind m f] takes the result of the monadic computation m, puts the f function in the monadic context passing it the result of m and then
    returning a monadic result. *)
end


(** {6 Boilerplate code}*)

val print :  ?first:string -> ?last:string -> ?sep:string -> ('a BatInnerIO.output -> 'b -> unit) -> 'a BatInnerIO.output -> 'b t -> unit
(** Print and consume the contents of an enumeration.*)

val t_printer : 'a BatValue_printer.t -> 'a t BatValue_printer.t

(** {6 Override modules}*)

(**
   The following modules replace functions defined in {!BatEnum} with functions
   behaving slightly differently but having the same name. This is by design:
   the functions meant to override the corresponding functions of {!BatEnum}.
*)

(** Operations on {!BatEnum} without exceptions.*)
module Exceptionless : sig
  val find : ('a -> bool) -> 'a t -> 'a option
    (** [find f e] returns [Some x] where [x] is the first element [x] of [e]
	such that [f x] returns [true], consuming the enumeration up to and
	including the found element, or [None] if no such element exists
	in the enumeration, consuming the whole enumeration in the search.

	Since [find] consumes a prefix of the enumeration, it can be used several
	times on the same enumeration to find the next element. *)
end


(** Operations on {!BatEnum} with labels.

    This module overrides a number of functions of {!BatEnum} by
    functions in which some arguments require labels. These labels are
    there to improve readability and safety and to let you change the
    order of arguments to functions. In every case, the behavior of the
    function is identical to that of the corresponding function of {!BatEnum}.
*)
module Labels : sig
  val iter:       f:('a -> unit) -> 'a t -> unit
  val iter2:      f:('a -> 'b -> unit) -> 'a t -> 'b t -> unit
  val exists:     f:('a -> bool) -> 'a t -> bool
  val for_all:    f:('a -> bool) -> 'a t -> bool
  val fold:       f:('b -> 'a -> 'b) -> init:'b -> 'a t -> 'b
  val fold2:      f:('a -> 'b -> 'c -> 'c) -> init:'c -> 'a t -> 'b t -> 'c
  val iteri:      f:(int -> 'a -> unit) -> 'a t -> unit
  val iter2i:     f:( int -> 'a -> 'b -> unit) -> 'a t -> 'b t -> unit
  val foldi:      f:(int -> 'a -> 'b -> 'b) -> init:'b -> 'a t -> 'b
  val fold2i:     f:(int -> 'a -> 'b -> 'c -> 'c) -> init:'c -> 'a t -> 'b t -> 'c
  val find:       f:('a -> bool) -> 'a t -> 'a
  val take_while: f:('a -> bool) -> 'a t -> 'a t
  val drop_while: f:('a -> bool) -> 'a t -> 'a t
  val map:        f:('a -> 'b) -> 'a t -> 'b t
  val mapi:       f:(int -> 'a -> 'b) -> 'a t -> 'b t
  val filter:     f:('a -> bool) -> 'a t -> 'a t
  val filter_map: f:('a -> 'b option) -> 'a t -> 'b t
  val from:       f:(unit -> 'a) -> 'a t
  val from_while: f:(unit -> 'a option) -> 'a t
  val from_loop:  init:'b -> f:('b -> ('a * 'b)) -> 'a t
  val seq:        init:'a -> f:('a -> 'a) -> cnd:('a -> bool) -> 'a t
  val unfold:     init:'b -> f:('b -> ('a * 'b) option) -> 'a t
  val init:       int -> f:(int -> 'a) -> 'a t
  val switch:     f:('a -> bool) -> 'a t -> 'a t * 'a t
  val compare:    ?cmp:('a -> 'a -> int) -> 'a t -> 'a t -> int

  module LExceptionless : sig
    val find : f:('a -> bool) -> 'a t -> 'a option
  end
end

(**/**)

(** {6 For system use only, not for the casual user}

    For compatibility with {!Stream}
*)

val iapp : 'a t -> 'a t -> 'a t
val icons : 'a -> 'a t -> 'a t
val ising : 'a -> 'a t

val lapp :  (unit -> 'a t) -> 'a t -> 'a t
val lcons : (unit -> 'a) -> 'a t -> 'a t
val lsing : (unit -> 'a) -> 'a t

val slazy : (unit -> 'a t) -> 'a t


(**/**)
