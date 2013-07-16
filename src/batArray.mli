(*
 * BatArray - additional and modified functions for arrays.
 * Copyright (C) 1996 Xavier Leroy
 *               2005 Richard W.M. Jones (rich @ annexia.org)
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

(** {6 Array operations}

    Arrays are mutable data structures with a fixed size, which
    support fast access and modification, and are used pervasively in
    imperative computing. While arrays are completely supported in
    OCaml, it is often a good idea to investigate persistent
    alternatives, such as lists or hash maps.

    This module replaces Stdlib's
    {{:http://caml.inria.fr/pub/docs/manual-ocaml/libref/Array.html}Array}
    module.

    A variant of arrays, arrays with capabilities, is provided in
    module {!BatArray.Cap}. This notion of capabilities permit the
    transformation of a mutable array into a read-only or a write-only
    arrays, without loss of speed and with the possibility of
    distributing different capabilities to different expressions.

    @author Xavier Leroy
    @author Richard W.M. Jones
    @author David Teller

*)

type 'a t = 'a array (** The type of arrays. *)

include BatEnum.Enumerable with type 'a enumerable = 'a t
include BatInterfaces.Mappable with type 'a mappable = 'a t

external length : 'a array -> int = "%array_length"
(** Return the length (number of elements) of the given array. *)

external get : 'a array -> int -> 'a = "%array_safe_get"
(** [Array.get a n] returns the element number [n] of array [a].
    The first element has number 0.
    The last element has number [Array.length a - 1].
    You can also write [a.(n)] instead of [Array.get a n].

    @raise Invalid_argument
    if [n] is outside the range 0 to [(Array.length a - 1)]. *)

external set : 'a array -> int -> 'a -> unit = "%array_safe_set"
(** [Array.set a n x] modifies array [a] in place, replacing
    element number [n] with [x].
    You can also write [a.(n) <- x] instead of [Array.set a n x].

    @raise Invalid_argument
    if [n] is outside the range 0 to [Array.length a - 1]. *)

external make : int -> 'a -> 'a array = "caml_make_vect"
(** [Array.make n x] returns a fresh array of length [n],
    initialized with [x].
    All the elements of this new array are initially
    physically equal to [x] (in the sense of the [==] predicate).
    Consequently, if [x] is mutable, it is shared among all elements
    of the array, and modifying [x] through one of the array entries
    will modify all other entries at the same time.

    @raise Invalid_argument if [n < 0] or [n > Sys.max_array_length].
    If the value of [x] is a floating-point number, then the maximum
    size is only [Sys.max_array_length / 2].*)

external create : int -> 'a -> 'a array = "caml_make_vect"
(** @deprecated [Array.create] is an alias for {!Array.make}. *)

val init : int -> (int -> 'a) -> 'a array
(** [Array.init n f] returns a fresh array of length [n],
    with element number [i] initialized to the result of [f i].
    In other terms, [Array.init n f] tabulates the results of [f]
    applied to the integers [0] to [n-1].

    @raise Invalid_argument if [n < 0] or [n > Sys.max_array_length].
    If the return type of [f] is [float], then the maximum
    size is only [Sys.max_array_length / 2].*)

val make_matrix : int -> int -> 'a -> 'a array array
(** [Array.make_matrix dimx dimy e] returns a two-dimensional array
    (an array of arrays) with first dimension [dimx] and
    second dimension [dimy]. All the elements of this new matrix
    are initially physically equal to [e].
    The element ([x,y]) of a matrix [m] is accessed
    with the notation [m.(x).(y)].

    @raise Invalid_argument if [dimx] or [dimy] is negative or
    greater than [Sys.max_array_length].
    If the value of [e] is a floating-point number, then the maximum
    size is only [Sys.max_array_length / 2]. *)

val create_matrix : int -> int -> 'a -> 'a array array
(** @deprecated [Array.create_matrix] is an alias for {!Array.make_matrix}. *)

val append : 'a array -> 'a array -> 'a array
(** [Array.append v1 v2] returns a fresh array containing the
    concatenation of the arrays [v1] and [v2]. *)

val concat : 'a array list -> 'a array
(** Same as [Array.append], but concatenates a list of arrays. *)

val sub : 'a array -> int -> int -> 'a array
(** [Array.sub a start len] returns a fresh array of length [len],
    containing the elements number [start] to [start + len - 1]
    of array [a].

    @raise Invalid_argument if [start] and [len] do not
    designate a valid subarray of [a]; that is, if
    [start < 0], or [len < 0], or [start + len > Array.length a]. *)

val copy : 'a array -> 'a array
(** [Array.copy a] returns a copy of [a], that is, a fresh array
    containing the same elements as [a]. *)

val fill : 'a array -> int -> int -> 'a -> unit
(** [Array.fill a ofs len x] modifies the array [a] in place,
    storing [x] in elements number [ofs] to [ofs + len - 1].

    @raise Invalid_argument if [ofs] and [len] do not
    designate a valid subarray of [a]. *)

val blit : 'a array -> int -> 'a array -> int -> int -> unit
(** [Array.blit v1 o1 v2 o2 len] copies [len] elements
    from array [v1], starting at element number [o1], to array [v2],
    starting at element number [o2]. It works correctly even if
    [v1] and [v2] are the same array, and the source and
    destination chunks overlap.

    @raise Invalid_argument if [o1] and [len] do not
    designate a valid subarray of [v1], or if [o2] and [len] do not
    designate a valid subarray of [v2]. *)

val to_list : 'a array -> 'a list
(** [Array.to_list a] returns the list of all the elements of [a]. *)

val of_list : 'a list -> 'a array
(** [Array.of_list l] returns a fresh array containing the elements
    of [l]. *)

val max : 'a array -> 'a
(** [max a] returns the largest value in [a] as judged by
    [Pervasives.compare]

    @raise Invalid_argument on empty input *)

val min : 'a array -> 'a
(** [min a] returns the smallest value in [a] as judged by
    [Pervasives.compare]

    @raise Invalid_argument on empty input *)

val sum : int array -> int
(** [sum l] returns the sum of the integers of [l] *)

val fsum : float array -> float
(** [fsum l] returns the sum of the floats of [l] *)

val avg : int array -> float
(** [avg l] returns the average of [l]

    @since 2.1
 *)

val favg : float array -> float
(** [favg l] returns the average of [l]

    @since 2.1
*)

val left : 'a array -> int -> 'a array
(**[left r len] returns the array containing the [len] first
   characters of [r]. If [r] contains less than [len] characters, it
   returns [r].

   Examples:
   [Array.left [|0;1;2;3;4;5;6|] 4 = [|0;1;2;3|]]
   [Array.left [|1;2;3|] 0 = [||]]
   [Array.left [|1;2;3|] 10 = [|1;2;3|]]
*)

val right : 'a array -> int -> 'a array
(**[left r len] returns the array containing the [len] last characters of [r].
   If [r] contains less than [len] characters, it returns [r].

   Example: [Array.right [|1;2;3;4;5;6|] 4 = [|3;4;5;6|]]
*)

val head : 'a array -> int -> 'a array
(**as {!left}*)

val tail : 'a array -> int -> 'a array
(**[tail r pos] returns the array containing all but the [pos] first characters of [r]

   Example: [Array.tail [|1;2;3;4;5;6|] 4 = [|5;6|]]
*)

val iter : ('a -> unit) -> 'a array -> unit
(** [Array.iter f a] applies function [f] in turn to all
    the elements of [a].  It is equivalent to
    [f a.(0); f a.(1); ...; f a.(Array.length a - 1); ()]. *)

val map : ('a -> 'b) -> 'a array -> 'b array
(** [Array.map f a] applies function [f] to all the elements of [a],
    and builds an array with the results returned by [f]:
    [[| f a.(0); f a.(1); ...; f a.(Array.length a - 1) |]]. *)

val iteri : (int -> 'a -> unit) -> 'a array -> unit
(** Same as {!Array.iter}, but the
    function is applied to the index of the element as first argument,
    and the element itself as second argument. *)

val mapi : (int -> 'a -> 'b) -> 'a array -> 'b array
(** Same as {!Array.map}, but the
    function is applied to the index of the element as first argument,
    and the element itself as second argument. *)

val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b array -> 'a
(** [Array.fold_left f x a] computes
    [f (... (f (f x a.(0)) a.(1)) ...) a.(n-1)],
    where [n] is the length of the array [a]. *)

val fold_right : ('b -> 'a -> 'a) -> 'b array -> 'a -> 'a
(** [Array.fold_right f a x] computes
    [f a.(0) (f a.(1) ( ... (f a.(n-1) x) ...))],
    where [n] is the length of the array [a]. *)

val modify : ('a -> 'a) -> 'a array -> unit
(** [modify f a] replaces every element [x] of [a] with [f x]. *)

val modifyi : (int -> 'a -> 'a) -> 'a array -> unit
(** Same as {!modify}, but the function is applied to the index of
    the element as the first argument, and the element itself as
    the second argument. *)

val fold_lefti : ('a -> int -> 'b -> 'a) -> 'a -> 'b array -> 'a
(** As [fold_left], but with the index of the element as additional argument *)

val fold_righti : (int -> 'b -> 'a -> 'a) -> 'b array -> 'a -> 'a
(** As [fold_right], but with the index of the element as additional argument *)

val reduce : ('a -> 'a -> 'a) -> 'a array -> 'a
(** [Array.reduce f a] is [fold_left f a.(0) [|a.(1); ..; a.(n-1)|]].  This
    is useful for merging a group of things that have no
    reasonable default value to return if the group is empty.

    @raise Invalid_argument on empty arrays. *)

val singleton : 'a -> 'a array
(** Create an array consisting of exactly one element.

    @since 2.1
*)


(** {6 Sorting} *)


val sort : ('a -> 'a -> int) -> 'a array -> unit
(** Sort an array in increasing order according to a comparison
    function.  The comparison function must return 0 if its arguments
    compare as equal, a positive integer if the first is greater,
    and a negative integer if the first is smaller (see below for a
    complete specification).  For example, {!Pervasives.compare} is
    a suitable comparison function, provided there are no floating-point
    NaN values in the data.  After calling [Array.sort], the
    array is sorted in place in increasing order.
    [Array.sort] is guaranteed to run in constant heap space
    and (at most) logarithmic stack space.

    The current implementation uses Heap Sort.  It runs in constant
    stack space.

    Specification of the comparison function:
    Let [a] be the array and [cmp] the comparison function.  The following
    must be true for all x, y, z in a :
    -   [cmp x y] > 0 if and only if [cmp y x] < 0
    -   if [cmp x y] >= 0 and [cmp y z] >= 0 then [cmp x z] >= 0

    When [Array.sort] returns, [a] contains the same elements as before,
    reordered in such a way that for all i and j valid indices of [a] :
    -   [cmp a.(i) a.(j)] >= 0 if and only if i >= j
*)

val stable_sort : ('a -> 'a -> int) -> 'a array -> unit
(** Same as {!Array.sort}, but the sorting algorithm is stable (i.e.
    elements that compare equal are kept in their original order) and
    not guaranteed to run in constant heap space.

    The current implementation uses Merge Sort. It uses [n/2]
    words of heap space, where [n] is the length of the array.
    It is usually faster than the current implementation of {!Array.sort}.
*)

val fast_sort : ('a -> 'a -> int) -> 'a array -> unit
(** Same as {!Array.sort} or {!Array.stable_sort}, whichever is faster
    on typical input.
*)

val decorate_stable_sort : ('a -> 'b) -> 'a array -> 'a array
(** [decorate_stable_sort f a] returns a sorted copy of [a] such that if [f
    x < f y] then [x] is earlier in the result than [y].  This
    function is useful when [f] is expensive, as it only computes [f
    x] once for each element in the array.  See
    [:[http://en.wikipedia.org/wiki/Schwartzian_transform]Schwartzian
    Transform].

    It is unnecessary to have an additional comparison function as
    argument, as the builtin [Pervasives.compare] is used to compare
    the ['b] values.  This is deemed sufficient. *)

val decorate_fast_sort : ('a -> 'b) -> 'a array -> 'a array
(** As {!Array.decorate_stable_sort}, but uses fast_sort internally. *)


(**{6 Operations on two arrays}*)

val iter2 : ('a -> 'b -> unit) -> 'a array -> 'b array -> unit
(** [Array.iter2 f [|a0; a1; ...; an|] [|b0; b1; ...; bn|]]
    performs calls [f a0 b0; f a1 b1; ...; f an bn] in that order.

    @raise Invalid_argument if the two arrays have different lengths. *)

val iter2i : (int -> 'a -> 'b -> unit) -> 'a array -> 'b array -> unit
(** [Array.iter2i f [|a0; a1; ...; an|] [|b0; b1; ...; bn|]]
    performs calls [f 0 a0 b0; f 1 a1 b1; ...; f n an bn] in that
    order.

    @raise Invalid_argument if the two arrays have different
    lengths. *)

val for_all2 : ('a -> 'b -> bool) -> 'a array -> 'b array -> bool
(** As {!Array.for_all} but on two arrays.

    @raise Invalid_argument if the two arrays have different lengths.*)


val exists2 : ('a -> 'b -> bool) -> 'a array -> 'b array -> bool
(** As {!Array.exists} but on two arrays.

    @raise Invalid_argument if the two arrays have different lengths. *)

val map2 : ('a -> 'b -> 'c) -> 'a array -> 'b array -> 'c array
(** As {!Array.map} but on two arrays.

    @raise Invalid_argument if the two arrays have different lengths. *)

(**{6 Predicates}*)

val for_all : ('a -> bool) -> 'a array -> bool
(** [for_all p [|a0; a1; ...; an|]] checks if all elements of the
    array satisfy the predicate [p].  That is, it returns [ (p a0)
    && (p a1) && ... && (p an)]. *)

val exists : ('a -> bool) -> 'a array -> bool
(** [exists p [|a0; a1; ...; an|]] checks if at least one element of
    the array satisfies the predicate [p].  That is, it returns [(p
    a0) || (p a1) || ... || (p an)]. *)

val find : ('a -> bool) -> 'a array -> 'a
(** [find p a] returns the first element of array [a] that
    satisfies the predicate [p].

    @raise Not_found if there is no value that satisfies [p] in
    the array [a]. *)

val mem : 'a -> 'a array -> bool
(** [mem m a] is true if and only if [m] is equal to an element of [a]. *)

val memq : 'a -> 'a array -> bool
(** Same as {!Array.mem} but uses physical equality instead of
    structural equality to compare array elements.  *)

val findi : ('a -> bool) -> 'a array -> int
(** [findi p a] returns the index of the first element of array [a]
    that satisfies the predicate [p].
    @raise Not_found if there is no value that satisfies [p] in the
    array [a].  *)

val filter : ('a -> bool) -> 'a array -> 'a array
(** [filter p a] returns all the elements of the array [a]
    that satisfy the predicate [p].  The order of the elements
    in the input array is preserved.  *)

val filteri : (int -> 'a -> bool) -> 'a array -> 'a array
(** As [filter] but with the index passed to the predicate. *)

val filter_map : ('a -> 'b option) -> 'a array -> 'b array
(** [filter_map f e] returns an array consisting of all elements
    [x] such that [f y] returns [Some x] , where [y] is an element
    of [e]. *)

val find_all : ('a -> bool) -> 'a array -> 'a array
(** [find_all] is another name for {!Array.filter}. *)

val partition : ('a -> bool) -> 'a array -> 'a array * 'a array
(** [partition p a] returns a pair of arrays [(a1, a2)], where
    [a1] is the array of all the elements of [a] that
    satisfy the predicate [p], and [a2] is the array of all the
    elements of [a] that do not satisfy [p].
    The order of the elements in the input array is preserved. *)

(** {6 Array transformations} *)

val rev : 'a array -> 'a array
(** Array reversal.*)

val rev_in_place : 'a array -> unit
(** In-place array reversal.  The array argument is updated. *)

(** {6 Conversions} *)

val enum : 'a array -> 'a BatEnum.t
(** Returns an enumeration of the elements of an array.
    Behavior of the enumeration is undefined if the contents of the array changes afterwards.*)

val of_enum : 'a BatEnum.t -> 'a array
(** Build an array from an enumeration. *)

val backwards : 'a array -> 'a BatEnum.t
(** Returns an enumeration of the elements of an array, from last to first. *)

val of_backwards : 'a BatEnum.t -> 'a array
(** Build an array from an enumeration, with the first element of
    the enumeration as the last element of the array and vice
    versa. *)

(** {6 Utilities} *)

val range : 'a array -> int BatEnum.t
(** [range a] returns an enumeration of all valid indexes into the given
    array.  For example, [range [|2;4;6;8|] = 0--3].*)

val insert : 'a array -> 'a -> int -> 'a array
(** [insert xs x i] returns a copy of [xs] except the value [x] is
    inserted in position [i] (and all later indices are shifted to the
    right). *)

(** {6 Boilerplate code}*)

val print : ?first:string -> ?last:string -> ?sep:string ->
  ('a, 'b) BatIO.printer -> ('a t, 'b) BatIO.printer
(** Print the contents of an array, with [~first] preceeding the first
    item (default: "[|"), [~last] following the last item (default:
    "|]") and [~sep] separating items (default: "; ").  A printing
    function must be provided to print the items in the array.

    Example: IO.to_string (Array.print Int.print) [|2;4;66|] = "[|2; 4; 66|]"
*)

val compare : 'a BatOrd.comp -> 'a array BatOrd.comp
(** [compare c] generates the lexicographical order on arrays induced
    by [c]. That is, given a comparison function for the elements of
    an array, this will return a comparison function for arrays of
    that type.  *)

val ord : 'a BatOrd.ord -> 'a array BatOrd.ord
(** Hoist an element comparison function to compare arrays of those
    elements, with shorter arrays less than longer ones, and
    lexicographically for arrays of the same size.  This is a
    different ordering than [compare], but is often faster. *)

val equal : 'a BatOrd.eq -> 'a array BatOrd.eq
(** Hoist a equality test for elements to arrays.  Arrays are only
    equal if their lengths are the same and corresponding elements
    test equal. *)

(** {6 Override modules}*)

(** The following modules replace functions defined in {!Array} with
    functions behaving slightly differently but having the same
    name. This is by design: the functions are meant to override the
    corresponding functions of {!Array}.
*)

(** Operations on {!Array} without exceptions.*)
module Exceptionless : sig

  val find : ('a -> bool) -> 'a t -> 'a option
  (** [find p a] returns [Some x], where [x] is the first element of
	array [a] that satisfies the predicate [p], or [None] if there
	is no such element.*)

  val findi : ('a -> bool) -> 'a t -> int option
    (** [findi p a] returns [Some n], where [n] is the index of the
		first element of array [a] that satisfies the predicate [p],
		or [None] if there is no such element.*)
end

(** Operations on {!Array} with labels.

    This module overrides a number of functions of {!Array} by
    functions in which some arguments require labels. These labels are
    there to improve readability and safety and to let you change the
    order of arguments to functions. In every case, the behavior of the
    function is identical to that of the corresponding function of {!Array}.
*)
module Labels : sig
  val init :  int -> f:(int -> 'a) -> 'a array
  val create: int -> init:'a -> 'a array
  val make_matrix :   dimx:int -> dimy:int -> 'a -> 'a array array
  val create_matrix : dimx:int -> dimy:int -> 'a -> 'a array array
  val sub :  'a array -> pos:int -> len:int -> 'a array
  val fill : 'a array -> pos:int -> len:int -> 'a -> unit
  val blit : src:'a array -> src_pos:int -> dst:'a array ->
    dst_pos:int -> len:int -> unit
  val iter :       f:('a -> unit) -> 'a array -> unit
  val map :        f:('a -> 'b) -> 'a array -> 'b array
  val iteri :      f:(int -> 'a -> unit) -> 'a array -> unit
  val mapi :       f:(int -> 'a -> 'b) -> 'a array -> 'b array
  val modify :     f:('a -> 'a) -> 'a array -> unit
  val modifyi :    f:(int -> 'a -> 'a) -> 'a array -> unit
  val fold_left :  f:('a -> 'b -> 'a) -> init:'a -> 'b array -> 'a
  val fold_right : f:('b -> 'a -> 'a) -> 'b array -> init:'a -> 'a
  val sort :        cmp:('a -> 'a -> int) -> 'a array -> unit
  val stable_sort : cmp:('a -> 'a -> int) -> 'a array -> unit
  val fast_sort :   cmp:('a -> 'a -> int) -> 'a array -> unit
  val iter2:      f:('a -> 'b -> unit) -> 'a t -> 'b t -> unit
  val exists:     f:('a -> bool) -> 'a t -> bool
  val for_all:    f:('a -> bool) -> 'a t -> bool
  val iter2i:     f:( int -> 'a -> 'b -> unit) -> 'a t -> 'b t -> unit
  val find:       f:('a -> bool) -> 'a t -> 'a
  val findi:      f:('a -> bool) -> 'a t -> int
  val map:        f:('a -> 'b) -> 'a t -> 'b t
  val mapi:       f:(int -> 'a -> 'b) -> 'a t -> 'b t
  val filter:     f:('a -> bool) -> 'a t -> 'a t
  val filter_map: f:('a -> 'b option) -> 'a t -> 'b t
  module LExceptionless : sig
    val find:       f:('a -> bool) -> 'a t -> 'a option
    val findi:      f:('a -> bool) -> 'a t -> int option
  end
end


(** {5 Capabilities for arrays.}

    This modules provides the same set of features as {!Array}, but
    with the added twist that arrays can be made read-only or
    write-only.  Read-only arrays may then be safely shared and
    distributed.

    There is no loss of performance involved.
*)
module Cap :
sig
  (**
     Only the capability-specific functions are documented here.
     See the complete [Array] module for the documentation of other functions.
  *)

  type ('a, 'b) t constraint 'b = [< `Read | `Write]
  (**The type of arrays with capabilities.
     An [('a, [`Read | `Write])] array behaves as a regular ['a array],
     while a [('a, [`Read]) array] only has read-only capabilities
     and a [('a, [`Write]) array] only has write-only capabilities.*)

  (**{6 Base operations}*)

  external length : ('a, [> ]) t -> int = "%array_length"
  external get : ('a, [> `Read]) t -> int -> 'a = "%array_safe_get"
  external set : ('a, [> `Write]) t -> int -> 'a -> unit = "%array_safe_set"

  (**{6 Constructors}*)

  external make : int -> 'a -> ('a, _) t = "caml_make_vect"
  external create : int -> 'a -> ('a, _) t = "caml_make_vect"

  external of_array  : 'a array -> ('a, _ ) t = "%identity"
  (** Adopt a regular array as a capability array, allowing
	to decrease capabilities if necessary.

	This operation involves no copying. In other words, in
	[let cap = of_array a in ...], any modification in [a]
	will also have effect on [cap] and reciprocally.*)

  external to_array  : ('a, [`Read | `Write]) t -> 'a array = "%identity"
  (** Return a capability array as an array.

	This operation requires both read and write permissions
	on the capability array and involves no copying. In other
	words, in [let a = of_array cap in ...], any modification
	in [a] will also have effect on [cap] and reciprocally.*)

  external read_only :  ('a, [>`Read])  t -> ('a, [`Read])  t = "%identity"
  (** Drop to read-only permissions.

	This operation involves no copying.*)

  external write_only : ('a, [>`Write]) t -> ('a, [`Write]) t = "%identity"
  (** Drop to write-only permissions.

	This operation involves no copying.*)

  val init : int -> (int -> 'a) -> ('a, _) t
  val make_matrix : int -> int -> 'a -> (('a, _)t, _) t
  val create_matrix : int -> int -> 'a ->  (('a, _)t, _) t

  (** {6 Iterators}*)

  val iter : ('a -> unit) -> ('a, [> `Read]) t -> unit
  val map : ('a -> 'b) -> ('a, [>`Read]) t -> ('b, _) t
  val iteri : (int -> 'a -> unit) -> ('a, [> `Read]) t -> unit
  val mapi : (int -> 'a -> 'b) -> ('a, [> `Read]) t -> ('b, _) t
  val modify : ('a -> 'a) -> ('a, [`Read | `Write]) t -> unit
  val modifyi : (int -> 'a -> 'a) -> ('a, [`Read | `Write]) t -> unit
  val fold_left : ('a -> 'b -> 'a) -> 'a -> ('b, [> `Read]) t -> 'a
  val fold_right : ('b -> 'a -> 'a) -> ('b, [> `Read]) t -> 'a -> 'a

  (**{6 Operations on two arrays}*)

  val iter2 : ('a -> 'b -> unit) -> ('a, [> `Read]) t -> ('b, [> `Read]) t -> unit
  val iter2i : (int -> 'a -> 'b -> unit) -> ('a, [> `Read]) t -> ('b, [> `Read]) t -> unit

  (**{6 Predicates}*)

  val for_all : ('a -> bool) -> ('a, [> `Read]) t -> bool
  val exists : ('a -> bool) -> ('a, [> `Read]) t -> bool
  val find : ('a -> bool) -> ('a, [> `Read]) t -> 'a
  val mem : 'a -> ('a, [> `Read]) t -> bool
  val memq : 'a -> ('a, [> `Read]) t -> bool
  val findi : ('a -> bool) -> ('a, [> `Read]) t -> int
  val filter : ('a -> bool) -> ('a, [> `Read]) t -> ('a, _) t
  val filter_map : ('a -> 'b option) -> ('a, [> `Read]) t -> ('b, _) t
  val find_all : ('a -> bool) -> ('a, [> `Read]) t -> ('a, _) t
  val partition : ('a -> bool) -> ('a, [> `Read]) t -> ('a, _) t * ('a, _)t

  (** {6 Array transformations} *)

  val rev : ('a, [> `Read]) t -> ('a, _) t
  val rev_in_place : ('a, [`Read | `Write]) t -> unit
  val append : ('a, [> `Read]) t ->  ('a, [> `Read]) t -> ('a, _) t
  val concat : ('a, [> `Read]) t list -> ('a, _) t
  val sub : ('a, [> `Read]) t -> int -> int -> ('a, _) t
  val copy : ('a, [> `Read]) t -> 'a array
  val fill : ('a, [> `Write]) t -> int -> int -> 'a -> unit
  val blit : ('a, [> `Read]) t -> int -> ('a, [>`Write]) t -> int -> int -> unit

  (** {6 Conversions} *)

  val enum : ('a, [> `Read]) t -> 'a BatEnum.t
  val of_enum : 'a BatEnum.t -> ('a, _) t
  val backwards : ('a, [> `Read]) t -> 'a BatEnum.t
  val of_backwards : 'a BatEnum.t -> ('a, _) t
  val to_list : ('a, [> `Read]) t -> 'a list
  val of_list : 'a list -> ('a, _) t

  (** {6 Utilities} *)

  val sort : ('a -> 'a -> int) -> ('a, [> `Read | `Write]) t -> unit
  val stable_sort : ('a -> 'a -> int) -> ('a, [ `Read | `Write]) t -> unit
  val fast_sort : ('a -> 'a -> int) -> ('a, [`Read | `Write]) t -> unit

  (** {6 Boilerplate code}*)

  val print : ?first:string -> ?last:string -> ?sep:string -> ('a BatIO.output -> 'b -> unit) ->  'a BatIO.output -> ('b, [>`Read]) t -> unit

  val compare : 'a BatOrd.comp -> ('a, [> `Read]) t BatOrd.comp
  val ord : 'a BatOrd.ord -> ('a, [> `Read]) t BatOrd.ord
  val equal : 'a BatOrd.eq -> ('a, [> `Read]) t BatOrd.eq

  (** {6 Override modules}*)

  (** Operations on {!BatArray.Cap} without exceptions.*)
  module Exceptionless : sig
    val find : ('a -> bool) -> ('a, [> `Read]) t -> 'a option
    val findi : ('a -> bool) -> ('a, [> `Read]) t -> int option
  end

  (** Operations on {!BatArray.Cap} with labels. *)
  module Labels : sig
    val init : int -> f:(int -> 'a) -> ('a, _) t
    val make: int -> init:'a -> ('a, _) t
    val create: int -> init:'a -> ('a, _) t
    val make_matrix : dimx:int -> dimy:int -> 'a -> (('a, _)t, _) t
    val create_matrix : dimx:int -> dimy:int -> 'a -> (('a, _)t, _) t
    val sub : ('a, [> `Read]) t -> pos:int -> len:int -> ('a, _) t
    val fill : ('a, [> `Write]) t -> pos:int -> len:int -> 'a -> unit
    val blit : src:('a, [> `Read]) t -> src_pos:int -> dst:('a, [>`Write]) t ->
      dst_pos:int -> len:int -> unit
    val iter : f:('a -> unit) -> ('a, [> `Read]) t -> unit
    val map : f:('a -> 'b) -> ('a, [>`Read]) t -> ('b, _) t
    val iteri : f:(int -> 'a -> unit) -> ('a, [> `Read]) t -> unit
    val mapi : f:(int -> 'a -> 'b) -> ('a, [> `Read]) t -> ('b, _) t
    val modify : f:('a -> 'a) -> ('a, [`Read | `Write]) t -> unit
    val modifyi : f:(int -> 'a -> 'a) -> ('a, [`Read | `Write]) t -> unit
    val fold_left : f:('a -> 'b -> 'a) -> init:'a ->  ('b, [> `Read]) t -> 'a
    val fold_right : f:('b -> 'a -> 'a) -> ('b, [> `Read]) t -> init:'a -> 'a
    val sort : cmp:('a -> 'a -> int) -> ('a, [> `Read | `Write]) t -> unit
    val stable_sort : cmp:('a -> 'a -> int) -> ('a, [ `Read | `Write]) t -> unit
    val fast_sort : cmp:('a -> 'a -> int) -> ('a, [`Read | `Write]) t -> unit
    val iter2:      f:('a -> 'b -> unit) -> ('a, [> `Read]) t -> ('b, [> `Read]) t -> unit
    val iter2i:     f:( int -> 'a -> 'b -> unit) -> ('a, [> `Read]) t -> ('b, [> `Read]) t -> unit
    val exists:     f:('a -> bool) -> ('a, [> `Read]) t -> bool
    val for_all:    f:('a -> bool) -> ('a, [> `Read]) t -> bool
    val find:       f:('a -> bool) -> ('a, [> `Read]) t -> 'a
    val map:        f:('a -> 'b) -> ('a, [>`Read]) t -> ('b, _) t
    val mapi:       f:(int -> 'a -> 'b) -> ('a, [>`Read]) t -> ('b, _) t
    val filter:     f:('a -> bool) -> ('a, [>`Read]) t -> ('a, _) t
    val filter_map: f:('a -> 'b option) -> ('a, [>`Read]) t -> ('b, _) t
  end
  (**/**)
  (** {6 Undocumented functions} *)

  external unsafe_get : ('a, [> `Read]) t -> int -> 'a = "%array_unsafe_get"
  external unsafe_set : ('a, [> `Write])t -> int -> 'a -> unit = "%array_unsafe_set"

  (**/**)
end

module Incubator : sig
  module Eq (T : BatOrd.Eq) : sig
    type t = T.t array
    val eq : T.t array BatOrd.eq
  end

  module Ord (T : BatOrd.Ord) : sig
    type t = T.t array
    val ord : T.t array BatOrd.ord
  end
end


(**/**)
(** {6 Undocumented functions} *)

(* for tests *)
val is_sorted_by : ('a -> 'b) -> 'a array -> bool

(* The following is for system use only. Do not call directly. *)

external unsafe_get : 'a array -> int -> 'a = "%array_unsafe_get"
external unsafe_set : 'a array -> int -> 'a -> unit = "%array_unsafe_set"

(**/**)
