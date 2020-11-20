(*
 * DynArray - Resizeable Ocaml arrays
 * Copyright (C) 2003 Brian Hurt
 * Copyright (C) 2003 Nicolas Cannasse
 * Copyright (C) 2008 David Teller
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

(** Dynamic arrays.

    A dynamic array is equivalent to an OCaml array that will resize itself
    when elements are added or removed, except that floats are boxed and
    that no initialization element is required.

    For all the traversal functions (iter, fold, map, etc.), what happens
    when the array that is being traversed is mutated is not defined.

    @author Brian Hurt
    @author Nicolas Cannasse
    @author David Teller (boilerplate code)
    @author andrepd
*)

type 'a t

include BatEnum.Enumerable with type 'a enumerable = 'a t
include BatInterfaces.Mappable with type 'a mappable = 'a t

exception Invalid_arg of int * string * string
(** When an operation on an array fails, [Invalid_arg] is raised. The
    integer is the value that made the operation fail, the first string
    contains the function name that has been called and the second string
    contains the parameter name that made the operation fail.
*)

(** {6 Array creation} *)

val create : unit -> 'a t
(** [create()] returns a new empty dynamic array. *)

val make : int -> 'a t
(** [make count] returns an array with some memory already allocated so
    up to [count] elements can be stored into it without resizing.

    @raise DynArray.Invalid_arg if make is called with a negative argument. *)

val init : int -> (int -> 'a) -> 'a t
(** [init n f] returns an array of [n] elements filled with values
    returned by [f 0 , f 1, ... f (n-1)].

    @raise DynArray.Invalid_arg if init is called with a negative argument. *)

val singleton : 'a -> 'a t
(** Create an array consisting of exactly one element.
    @since NEXT_RELEASE *)

(** {6 Array manipulation functions} *)

val get : 'a t -> int -> 'a
(** [get darr idx] gets the element in [darr] at index [idx]. If [darr] has
    [len] elements in it, then the valid indexes range from [0] to [len-1].

    @raise DynArray.Invalid_arg if called with an invalid index. *)

val set : 'a t -> int -> 'a -> unit
(** [set darr idx v] sets the element of [darr] at index [idx] to value
    [v].  The previous value is overwritten.

    @raise DynArray.Invalid_arg if called with an invalid index. *)

val upd : 'a t -> int -> ('a -> 'a) -> unit
(** [upd darr idx f] sets the element of [darr] at index [idx] to value
    [f (get darr idx)]).  The previous value is overwritten.

    @raise DynArray.Invalid_arg if called with an invalid index.
    @since NEXT_RELEASE *)

val length : 'a t -> int
(** Return the number of elements in the array. *)

val empty : 'a t -> bool
(** Return true if the number of elements in the array is 0. *)

val first : 'a t -> 'a
(** [first darr] returns the first element of [darr].

    @raise DynArray.Invalid_arg if length of the array is 0.
    @since NEXT_RELEASE *)

val last : 'a t -> 'a
(** [last darr] returns the last element of [darr].

    @raise DynArray.Invalid_arg if length of the array is 0. *)

val left : 'a t -> int -> 'a t
(** [left r len] returns the array containing the [len] first characters of [r].
    If [r] contains less than [len] characters, it returns [r].

    @raise DynArray.Invalid_arg if called with an invalid index.
    @since NEXT_RELEASE *)

val right : 'a t -> int -> 'a t
(** [right r len] returns the array containing the [len] last characters of [r].
    If [r] contains less than [len] characters, it returns [r].

    @raise DynArray.Invalid_arg if called with an invalid index.
    @since NEXT_RELEASE *)

val head : 'a t -> int -> 'a t
(** Alias for {!left}
    @since NEXT_RELEASE *)

val tail : 'a t -> int -> 'a t
(** [tail r pos] returns the array containing all but the [pos] first characters of [r].

    @raise DynArray.Invalid_arg if called with an invalid index.
    @since NEXT_RELEASE *)

val insert : 'a t -> int -> 'a -> unit
(** [insert darr idx v] inserts [v] into [darr] at index [idx].  All elements
    of [darr] with an index greater than or equal to [idx] have their
    index incremented (are moved up one place) to make room for the new
    element.

    @raise DynArray.Invalid_arg if called with an invalid index. *)

val add : 'a t -> 'a -> unit
(** [add darr v] appends [v] onto the end of [darr].  [v] becomes the new
    last element of [darr]. *)

val append : 'a t -> 'a t -> unit
(** [append src dst] adds all elements of [src] to the end of [dst]. *)

(*val concat : 'a array list -> 'a array
(** Same as [append], but concatenates a list of arrays. *)*)

val delete : 'a t -> int -> unit
(** [delete darr idx] deletes the element of [darr] at [idx].  All elements
    with an index greater than [idx] have their index decremented (are
    moved down one place) to fill in the hole.

    @raise DynArray.Invalid_arg if called with an invalid index. *)

val delete_last : 'a t -> unit
(** [delete_last darr] deletes the last element of [darr]. This is equivalent
    of doing [delete darr ((length darr) - 1)].

    @raise DynArray.Invalid_arg if length of the array is 0. *)

val delete_range : 'a t -> int -> int -> unit
(** [delete_range darr idx len] deletes [len] elements starting at index [idx].
    All elements with an index greater than [idx+len] are moved to fill
    in the hole.

    @raise DynArray.Invalid_arg if called with an invalid length or index. *)

val clear : 'a t -> unit
(** remove all elements from the array and resize it to 0. *)

val blit : 'a t -> int -> 'a t -> int -> int -> unit
(** [blit src srcidx dst dstidx len] copies [len] elements from [src]
    starting with index [srcidx] to [dst] starting at [dstidx].

    @raise DynArray.Invalid_arg if called with an invalid length or indices. *)

val compact : 'a t -> unit
(** [compact darr] ensures that the space allocated by the array is minimal. *)



(** {6 Array copy and conversion} *)

val enum : 'a t -> 'a BatEnum.t
(** [enum darr] returns the enumeration of [darr] elements. *)

val of_enum : 'a BatEnum.t -> 'a t
(** [of_enum e] returns an array that holds, in order, the elements of [e]. *)

(* val backwards : 'a array -> 'a BatEnum.t
(** Returns an enumeration of the elements of an array, from last to first. *)

val of_backwards : 'a BatEnum.t -> 'a array
(** Build an array from an enumeration, with the first element of
    the enumeration as the last element of the array and vice
    versa. *) *)

val range : 'a t -> int BatEnum.t
(** [range a] returns an enumeration of all valid indices of the given
    array, that is, [range a = 0 --^ length a]
    @since NEXT_RELEASE *)

val to_list : 'a t -> 'a list
(** [to_list darr] returns the elements of [darr] in order as a list. *)

val of_list : 'a list -> 'a t
(** [of_list lst] returns a dynamic array with the elements of [lst] in
    it in order. *)

val to_array : 'a t -> 'a array
(** [to_array darr] returns the elements of [darr] in order as an array. *)

val of_array : 'a array -> 'a t
(** [of_array arr] returns an array with the elements of [arr] in it
    in order. *)

val copy : 'a t -> 'a t
(** [copy a] returns a fresh copy of [a], such that no modification of
    [a] affects the copy, or vice versa (all new memory is allocated for
    the copy). *)

val sub : 'a t -> int -> int -> 'a t
(** [sub a start len] returns an array holding the subset of [len]
    elements from [a] starting with the element at index [idx]. 

    @raise DynArray.Invalid_arg if [start] and [len] do not
    designate a valid subarray of [a]; that is, if
    [start < 0], or [len < 0], or [start + len > Array.length a]. *)

val fill : 'a t -> int -> int -> 'a -> unit
(** [fill a start len x] modifies the array [a] in place,
    storing [x] in elements number [start] to [start + len - 1].

    @raise DynArray.Invalid_arg if [start] and [len] do not
    designate a valid subarray of [a].
    @since NEXT_RELEASE *)

val split : ('a * 'b) t -> 'a t * 'b t
(** [split a] converts the array of pairs [a] into a pair of arrays.
    @since NEXT_RELEASE *)

val combine : 'a t -> 'b t -> ('a * 'b) t
(** [combine a b] converts arrays [[a0,...aN] [b0,...,bN]] into 
    an array of pairs [[(a0,b0),...,(aN,bN)]]. 

    @raise Pervasives.Invalid_argument if the two arrays have different lengths.
    @since NEXT_RELEASE *)



(** {6 Array functional support} *)

val iter : ('a -> unit) -> 'a t -> unit
(** [iter f darr] calls the function [f] on every element of [darr].  It
    is equivalent to [for i = 0 to length darr - 1 do f (get darr i) done;] *)

val iteri : (int -> 'a -> unit) -> 'a t -> unit
(** [iteri f darr] calls the function [f] on every element of [darr].  It
    is equivalent to [for i = 0 to length darr - 1 do f i (get darr i) done;]
    *)

val map : ('a -> 'b) -> 'a t -> 'b t
(** [map f darr] applies the function [f] to every element of [darr]
    and creates a dynamic array from the results - similar to [List.map] or
    [Array.map]. *)

val mapi : (int -> 'a -> 'b) -> 'a t -> 'b t
(** [mapi f darr] applies the function [f] to every element of [darr]
    and creates a dynamic array from the results - similar to [List.mapi] or
    [Array.mapi]. *)

val modify : ('a -> 'a) -> 'a t -> unit
(** [modify f a] replaces every element [x] of [a] with [f x].
    @since NEXT_RELEASE *)

val modifyi : (int -> 'a -> 'a) -> 'a t -> unit
(** Same as {!modify}, but the function is applied to the index of
    the element as the first argument, and the element itself as
    the second argument.
    @since NEXT_RELEASE *)

val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
(** [fold_left f x darr] computes [f ( ... ( f ( f a0 x) a1) ) ... )
    aN], where [a0,a1..aN] are the indexed elements of [darr]. *)

val fold_right : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
(** [fold_right f darr x] computes [ f a0 (f a1 ( ... ( f aN x )
    ... ) ) ], where [a0,a1..aN] are the indexed elements of
    [darr]. *)

val fold_lefti : ('a -> int -> 'b -> 'a) -> 'a -> 'b t -> 'a
(** As [fold_left], but with the index of the element as additional argument.
    @since NEXT_RELEASE *)

val fold_righti : (int -> 'b -> 'a -> 'a) -> 'b t -> 'a -> 'a
(** As [fold_right], but with the index of the element as additional argument.
    @since NEXT_RELEASE *)

val reduce : ('a -> 'a -> 'a) -> 'a t -> 'a
(** [reduce f a] is [fold_left f a0 [a1, ... aN]].  This
    is useful for merging a group of things that have no
    reasonable default value to return if the group is empty.

    @raise Invalid_argument on empty arrays.
    @since NEXT_RELEASE *)

val keep : ('a -> bool) -> 'a t -> unit
(** [keep p darr] removes in place all the element [x] of [darr]
    such that [p x = false]

    {b Note} In previous versions, this function used to be called
    {!filter}. As this caused incompatibilities with comprehension
    of dynamic arrays, the function name has been changed.
*)

val filter : ('a -> bool) -> 'a t -> 'a t
(** [filter p a] returns all the elements of the array [a]
    that satisfy the predicate [p].  The order of the elements
    in the input array is preserved.

    {b Note} This function replaces another function called [filter],
    available in previous versions of the library. As the old function
    was incompatible with comprehension of dynamic arrays, its name
    was changed to {!keep}.
*)

val find_all : ('a -> bool) -> 'a t -> 'a t
(** [find_all] is another name for [filter].
    @since NEXT_RELEASE *)

val filteri : (int -> 'a -> bool) -> 'a t -> 'a t
(** As [filter] but with the index passed to the predicate.
    @since NEXT_RELEASE *)

val filter_map : ('a -> 'b option) -> 'a t -> 'b t
(** [filter_map f e] returns an array consisting of all elements
    [x] such that [f y] returns [Some x] , where [y] is an element
    of [e]. *)

val partition : ('a -> bool) -> 'a t -> 'a t * 'a t
(** [partition p a] returns a pair of arrays [(a1, a2)], where
    [a1] is the array of all the elements of [a] that
    satisfy the predicate [p], and [a2] is the array of all the
    elements of [a] that do not satisfy [p].
    The order of the elements in the input array is preserved.

    @since NEXT_RELEASE *)

val for_all : ('a -> bool) -> 'a t -> bool
(** [for_all p [a0; a1; ...; an]] checks if all elements of the
    array satisfy the predicate [p].  That is, it returns [ (p a0)
    && (p a1) && ... && (p an)].
    @since NEXT_RELEASE *)

val exists : ('a -> bool) -> 'a t -> bool
(** [exists p [a0; a1; ...; an]] checks if at least one element of
    the array satisfies the predicate [p].  That is, it returns [(p
    a0) || (p a1) || ... || (p an)].
    @since NEXT_RELEASE *)

val find : ('a -> bool) -> 'a t -> 'a
(** [find p a] returns the first element of array [a] that
    satisfies the predicate [p].

    @raise Not_found if there is no value that satisfies [p] in
    the array [a].
    @since NEXT_RELEASE *)

val findi : ('a -> bool) -> 'a t -> int
(** [findi p a] returns the index of the first element of array [a]
    that satisfies the predicate [p].

    @raise Not_found if there is no value that satisfies [p] in the
    array [a].
    @since NEXT_RELEASE *)

val index_of : ('a -> bool) -> 'a t -> int
(** Alias for {!findi} *)

val mem : 'a -> 'a t -> bool
(** [mem m a] is true if and only if [m] is equal to an element of [a].
    @since NEXT_RELEASE *)

val memq : 'a -> 'a t -> bool
(** Same as {!mem} but uses physical equality instead of
    structural equality to compare array elements.
    @since NEXT_RELEASE *)

val rev : 'a t -> 'a t
(** Array reversal.
    @since NEXT_RELEASE *)

val rev_in_place : 'a t -> unit
(** In-place array reversal.  The given array is updated.
    @since NEXT_RELEASE *)

val max : 'a t -> 'a
(** [max a] returns the largest value in [a] as judged by
    [Pervasives.compare]

    @raise Pervasives.Invalid_argument on empty input
    @since NEXT_RELEASE *)

val min : 'a t -> 'a
(** [min a] returns the smallest value in [a] as judged by
    [Pervasives.compare]

    @raise Pervasives.Invalid_argument on empty input
    @since NEXT_RELEASE *)

val min_max : 'a t -> 'a * 'a
(** [min_max a] returns the (smallest, largest) pair of values from [a]
    as judged by [Pervasives.compare]

    @raise Pervasives.Invalid_argument on empty input
    @since NEXT_RELEASE *)

val sum : int t -> int
(** [sum l] returns the sum of the integers of [l].
    @since NEXT_RELEASE *)

val fsum : float t -> float
(** [fsum l] returns the sum of the floats of [l].
    @since NEXT_RELEASE *)

val kahan_sum : float t -> float
(** [kahan_sum l] returns a numerically-accurate
    sum of the floats of [l].

    You should consider using Kahan summation when you really care
    about very small differences in the result, while the result or
    one of the intermediate sums can be very large (which usually
    results in loss of precision of floating-point addition).

    The worst-case rounding error is constant, instead of growing with
    (the square root of) the length of the input array as with {!
    fsum}. On the other hand, processing each element requires four
    floating-point operations instead of one. See
    {{: https://en.wikipedia.org/wiki/Kahan_summation_algorithm }
    the wikipedia article} on Kahan summation for more details.

    @since NEXT_RELEASE
*)

val avg : int t -> float
(** [avg l] returns the average of [l]
    @since NEXT_RELEASE *)

val favg : float t -> float
(** [favg l] returns the average of [l]
    @since NEXT_RELEASE *)




(** {6 Operations on two arrays} *)

val iter2 : ('a -> 'b -> unit) -> 'a t -> 'b t -> unit
(** [iter2 f [a0, a1, ..., an] [b0, b1, ..., bn]]
    performs calls [f a0 b0, f a1 b1, ..., f an bn] in that order.

    @raise Pervasives.Invalid_argument if the two arrays have different lengths.
    @since NEXT_RELEASE *)

val iter2i : (int -> 'a -> 'b -> unit) -> 'a t -> 'b t -> unit
(** [iter2i f [a0, a1, ..., an] [b0, b1, ..., bn]]
    performs calls [f 0 a0 b0, f 1 a1 b1, ..., f n an bn] in that
    order.

    @raise Pervasives.Invalid_argument if the two arrays have different lengths.
    @since NEXT_RELEASE *)

val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
(** As {!map} but on two arrays.

    @raise Pervasives.Invalid_argument if the two arrays have different lengths.
    @since NEXT_RELEASE *)

val map2i : (int -> 'a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
(** As {!mapi} but on two arrays.

    @raise Pervasives.Invalid_argument if the two arrays have different lengths.
    @since NEXT_RELEASE *)

val for_all2 : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool
(** As {!for_all} but on two arrays.

    @raise Pervasives.Invalid_argument if the two arrays have different lengths.
    @since NEXT_RELEASE *)

val exists2 : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool
(** As {!exists} but on two arrays.

    @raise Pervasives.Invalid_argument if the two arrays have different lengths.
    @since NEXT_RELEASE *)

val cartesian_product : 'a t -> 'b t -> ('a * 'b) t
(** Cartesian product of the two arrays.
    @since NEXT_RELEASE *)




(** {6 Array resizers} *)

type resizer_t = currslots:int -> oldlength:int -> newlength:int -> int
(** The type of a resizer function.

    Resizer functions are called whenever elements are added to
    or removed from the dynamic array to determine what the current number of
    storage spaces in the array should be.  The three named arguments
    passed to a resizer are the current number of storage spaces in
    the array, the length of the array before the elements are
    added or removed, and the length the array will be after the
    elements are added or removed.  If elements are being added, newlength
    will be larger than oldlength, if elements are being removed,
    newlength will be smaller than oldlength. If the resizer function
    returns exactly oldlength, the size of the array is only changed when
    adding an element while there is not enough space for it.

    By default, all dynamic arrays are created with the [default_resizer].
    When a dynamic array is created from another dynamic array (using [copy],
    [map] , etc. ) the resizer of the copy will be the same as the original
    dynamic array resizer. To change the resizer, use the [set_resizer]
    function.
*)

val set_resizer : 'a t -> resizer_t -> unit
(** Change the resizer for this array. *)

val get_resizer : 'a t -> resizer_t
(** Get the current resizer function for a given array *)

val default_resizer : resizer_t
(** The default resizer function the library is using - in this version
    of DynArray, this is the [exponential_resizer] but should change in
    next versions. *)

val exponential_resizer : resizer_t
(** The exponential resizer- The default resizer except when the resizer
    is being copied from some other darray.

    [exponential_resizer] works by doubling or halving the number of
    slots until they "fit".  If the number of slots is less than the
    new length, the number of slots is doubled until it is greater
    than the new length (or Sys.max_array_size is reached).

    If the number of slots is more than four times the new length,
    the number of slots is halved until it is less than four times the
    new length.

    Allowing darrays to fall below 25% utilization before shrinking them
    prevents "thrashing".  Consider the case where the caller is constantly
    adding a few elements, and then removing a few elements, causing
    the length to constantly cross above and below a power of two.
    Shrinking the array when it falls below 50% would causing the
    underlying array to be constantly allocated and deallocated.
    A few elements would be added, causing the array to be reallocated
    and have a usage of just above 50%.  Then a few elements would be
    remove, and the array would fall below 50% utilization and be
    reallocated yet again.  The bulk of the array, untouched, would be
    copied and copied again.  By setting the threshold at 25% instead,
    such "thrashing" only occurs with wild swings- adding and removing
    huge numbers of elements (more than half of the elements in the array).

    [exponential_resizer] is a good performing resizer for most
    applications.  A list allocates 2 words for every element, while an
    array (with large numbers of elements) allocates only 1 word per
    element (ignoring unboxed floats).  On insert, [exponential_resizer]
    keeps the amount of wasted "extra" array elements below 50%, meaning
    that less than 2 words per element are used.  Even on removals
    where the amount of wasted space is allowed to rise to 75%, that
    only means that darray is using 4 words per element.  This is
    generally not a significant overhead.

    Furthermore, [exponential_resizer] minimizes the number of copies
    needed- appending n elements into an empty darray with initial size
    0 requires between n and 2n elements of the array be copied- O(n)
    work, or O(1) work per element (on average).  A similar argument
    can be made that deletes from the end of the array are O(1) as
    well (obviously deletes from anywhere else are O(n) work- you
    have to move the n or so elements above the deleted element down).

*)

val step_resizer : int -> resizer_t
(** The stepwise resizer- another example of a resizer function, this
    time of a parameterized resizer.

    The resizer returned by [step_resizer step] returns the smallest
    multiple of [step] larger than [newlength] if [currslots] is less
    then [newlength]-[step] or greater than [newlength].

    For example, to make an darray with a step of 10, a length
    of len, and a null of null, you would do:
    [make] ~resizer:([step_resizer] 10) len null
*)

val conservative_exponential_resizer : resizer_t
(** [conservative_exponential_resizer] is an example resizer function
    which uses the oldlength parameter.  It only shrinks the array
    on inserts- no deletes shrink the array, only inserts.  It does
    this by comparing the oldlength and newlength parameters.  Other
    than that, it acts like [exponential_resizer].
*)

val create_with : resizer_t -> 'a t
(** create a new dynamic array that uses the given resizer.

    @since 2.3.0
*)


(** {6 Unsafe operations} **)

val unsafe_get : 'a t -> int -> 'a
val unsafe_set : 'a t -> int -> 'a -> unit
val unsafe_upd : 'a t -> int -> ('a -> 'a) -> unit
(** @since NEXT_RELEASE *)


(** {6 Boilerplate code} *)

(** {7 Printing} *)

val print :  ?first:string -> ?last:string -> ?sep:string -> ('a BatInnerIO.output -> 'b -> unit) -> 'a BatInnerIO.output -> 'b t -> unit

(** Operations on {!DynArray} without exceptions. *)
module Exceptionless : sig
  val find : ('a -> bool) -> 'a t -> 'a option
  (** [find p a] returns [Some x], where [x] is the first element of
      array [a] that satisfies the predicate [p], or [None] if there
      is no such element.
      @since NEXT_RELEASE *)

  val findi : ('a -> bool) -> 'a t -> int option
  (** [findi p a] returns [Some n], where [n] is the index of the
      first element of array [a] that satisfies the predicate [p],
      or [None] if there is no such element.
      @since NEXT_RELEASE *)
end

(**/**)
val invariants : _ t -> unit
val bool_invariants : _ t -> bool
(**/**)
