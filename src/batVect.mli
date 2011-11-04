(* 
 * Vect - Extensible arrays based on ropes
 * Copyright (C) 2007 Mauricio Fernandez <mfp@acm.org>
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

(* Vect: extensible arrays based on ropes as described in

   Boehm, H., Atkinson, R., and Plass, M. 1995. Ropes: an alternative to
   strings.  Softw. Pract. Exper. 25, 12 (Dec. 1995), 1315-1330.

   Motivated by Luca de Alfaro's extensible array implementation Vec.
 *)

(** Extensible vectors with constant-time append/prepend.

  This module implements extensible arrays which work very much like ropes
  as described in
  {b Boehm, H., Atkinson, R., and Plass, M.} 1995. {e Ropes: an alternative to
  strings.}  Softw. Pract. Exper. 25, 12 (Dec. 1995), 1315-1330.

  These vectors have some interesting properties:
- lower space overhead than other structures based on balanced trees such as Vec.
  The overhead can be adjusted, allowing to make get faster at the expense of set
  and viceversa.
- appending or prepending a small vector to an arbitrarily large one in amortized
  constant time
- concat, substring, insert, remove operations in amortized logarithmic time
- access to and modification of vectors in logarithmic time

{8 Functional nature and persistence}

All operations but [destructive_set] (provided for efficient ephemeral usage)
are non-destructive: the original vect is never modified.  When a new vect is
returned as the result of an operation, it will share as much data as possible
with its "parent". For instance, if a vect of length [n] undergoes [m]
operations (assume [n >> m]) like set, append or prepend, the modified vector
will only require [O(m)] space in addition to that taken by the original vect.

However, Vect is an amortized data structure, and its use in a persistent setting
can easily degrade its amortized time bounds. It is thus mainly intended to be used
ephemerally. In some cases, it is possible to use Vect persistently with the same
amortized bounds by explicitly rebalancing vects to be reused using [balance].
Special care must be taken to avoid calling [balance] too frequently; in the limit,
calling [balance] after each modification would defeat the purpose of amortization.

  @author Mauricio Fernandez
  *)

type 'a t
  (** The type of a polymorphic vect. *)

exception Out_of_bounds
  (** Raised when an operation violates the bounds of the vect. *)

val max_length : int
  (** Maximum length of the vect. *)

(** {6 Creation and conversions} *)

val empty : 'a t 
  (** The empty vect. *)

val singleton : 'a -> 'a t
  (** Returns a Vect of length 1 holding only the given element.*)

val of_array : 'a array -> 'a t
  (** [of_array s] returns a vect corresponding to the array [s].
      Operates in [O(n)] time. *)

val to_array : 'a t -> 'a array
  (**  [to_array r] returns an array corresponding to the vect [r]. *)

val to_list : 'a t -> 'a list
  (** Returns a list with the elements contained in the vect. *)

val of_list : 'a list -> 'a t

val make : int -> 'a -> 'a t
  (** [make i c] returns a vect of length [i] whose elements are all equal to
      [c]; it is similar to Array.make *)

val init : int -> (int -> 'a) -> 'a t
  (** [init n f] returns a fresh vect of length [n],
      with element number [i] initialized to the result of [f i].
      In other terms, [init n f] tabulates the results of [f]
      applied to the integers [0] to [n-1].
      
      @raise Invalid_argument if [n < 0] or [n > max_length].*)

(** {6 Properties } *)

val is_empty : 'a t -> bool
  (** Returns whether the vect is empty or not. *)

val height : 'a t -> int
  (** Returns the height (depth) of the vect. *)

val length : 'a t -> int
  (** Returns the length of the vect ([O(1)]). *)

(** {6 Operations } *)

val balance : 'a t -> 'a t
  (** [balance r] returns a balanced copy of the [r] vect. Note that vects are
    automatically rebalanced when their height exceeds a given threshold, but
    [balance] allows to invoke that operation explicity. *)

val concat : 'a t -> 'a t -> 'a t
  (** [concat r u] concatenates the [r] and [u] vects. In general, it operates
      in [O(log(min n1 n2))] amortized time.
      Small vects are treated specially and can be appended/prepended in
      amortized [O(1)] time. *)

val append : 'a -> 'a t -> 'a t
  (** [append c r] returns a new vect with the [c] element at the end
      in amortized [O(1)] time. *)

val prepend : 'a -> 'a t -> 'a t
  (** [prepend c r] returns a new vect with the [c] character at the
      beginning in amortized [O(1)] time. *)

val get : 'a t -> int -> 'a
  (** [get v n] returns the (n+1)th element from the vect [v]; i.e.
      [get v 0] returns the first element.
      Operates in worst-case [O(log size)] time.
      Raises Out_of_bounds if a character out of bounds is requested. *)

val at : 'a t -> int -> 'a
  (** as [get] *)

val set : 'a t -> int -> 'a -> 'a t
  (** [set v n c] returns a copy of the [v] vect where the (n+1)th element
      (see also [get]) has been set to [c].
      Operates in worst-case [O(log size)] time. *)

val modify : 'a t -> int -> ('a -> 'a) -> 'a t
  (** [modify v n f] is equivalent to [set v n (f (get v n))], but
      more efficient.  Operates in worst-case [O(log size)] time. *)


val destructive_set : 'a t -> int -> 'a -> unit
  (** [destructive_set n e v] sets the element of index [n] in the [v] vect
      to [e]. {b This operation is destructive}, and will also affect vects
      sharing the modified leaf with [v]. Use with caution. *)

val sub : int -> int -> 'a t -> 'a t
  (** [sub m n r] returns a sub-vect of [r] containing all the elements
      whose indexes range from [m] to [m + n - 1] (included).
    Raises Out_of_bounds in the same cases as Array.sub.
    Operates in worst-case [O(log size)] time.  *)

val insert : int -> 'a t -> 'a t -> 'a t
  (** [insert n r u] returns a copy of the [u] vect where [r] has been
      inserted between the elements with index [n] and [n + 1] in the
      original vect. The length of the new vect is
      [length u + length r].
      Operates in amortized [O(log(size r) + log(size u))] time. *)

val remove : int -> int -> 'a t -> 'a t
  (** [remove m n r] returns the vect resulting from deleting the
      elements with indexes ranging from [m] to [m + n - 1] (included)
      from the original vect [r]. The length of the new vect is
      [length r - n].
      Operates in amortized [O(log(size r))] time. *)

(** {6 Conversion}*)

val enum : 'a t -> 'a BatEnum.t
  (** Returns an enumeration of the elements of the vector. 
      Behavior of the enumeration is undefined if the contents of the vector changes afterwards.*)

val of_enum : 'a BatEnum.t -> 'a t
  (** Build a vector from an enumeration.*)

val backwards : 'a t -> 'a BatEnum.t
  (** Returns an enumeration of the elements of a vector, from last to first. 
      Behavior of the enumeration is undefined if the contents of the vector changes afterwards.*)

val of_backwards : 'a BatEnum.t -> 'a t
  (** Build a vector from an enumeration, from last to first.*)

(** {6 Iteration and higher-order functions } *)

val iter : ('a -> unit) -> 'a t -> unit
  (** [iter f r] applies [f] to all the elements in the [r] vect,
    in order. *)

val iteri : (int -> 'a -> unit) -> 'a t -> unit
  (** Operates like iter, but also passes the index of the character
      to the given function. *)

val rangeiter : ('a -> unit) -> int -> int -> 'a t -> unit
  (** [rangeiter f m n r] applies [f] to all the elements whose
      indices [k] satisfy [m] <= [k] < [m + n].
      It is thus equivalent to [iter f (sub m n r)], but does not
      create an intermediary vect. [rangeiter] operates in worst-case
      [O(n + log m)] time, which improves on the [O(n log m)] bound
      from an explicit loop using [get].
      Raises Out_of_bounds in the same cases as [sub]. *)

val fold : ('b -> 'a -> 'b ) -> 'b -> 'a t -> 'b
  (** [fold f a r] computes [ f (... (f (f a r0) r1)...) rN-1 ]
      where [rn = Vect.get n r ] and [N = length r]. *)

val fold_left : ('b -> 'a -> 'b ) -> 'b -> 'a t -> 'b
  (** [fold_left f a r] computes [ f (... (f (f a r0) r1)...) rN-1 ]
      where [rn = Vect.get n r ] and [N = length r]. *)
  
val reduce : ('a -> 'a -> 'a) -> 'a t -> 'a
  (** as {!fold_left}, but no initial value - just applies reducing
      function to elements from left to right. *)
  
val fold_right : ('a -> 'b -> 'b ) -> 'a t -> 'b -> 'b
  (** [fold_right f r a] computes [ f (r0 ... (f rN-2 (f rN-1 a)) ...)) ]
      where [rn = Vect.get n r ] and [N = length r]. *)

val foldi : (int -> 'b -> 'a -> 'b) -> 'b -> 'a t -> 'b
  (** As {!fold}, but with the position of each value passed to the
      folding function *)

val map : ('a -> 'b) -> 'a t -> 'b t
  (** [map f v] returns a vect isomorphic to [v] where each element of index
      [i] equals [f (get v i)]. Therefore, the height of the returned vect
      is the same as that of the original one. Operates in [O(n)] time. *)

val mapi : (int -> 'a -> 'b) -> 'a t -> 'b t
  (** Same as {!map}, but the
      function is applied to the index of the element as first argument,
      and the element itself as second argument. *)

(* NOT PROVIDED?
val id_map : ('a -> 'a) -> 'a t -> 'a t
  (** [id_map f v] returns a vect isomorphic to [v] where each element of index
      [i] equals [f (get v i)]. It is very similar to [map], but tries to share
      as much data as possible with the original vect; for example,
      [id_map (fun x -> x) v == v]. This can lead to significative space savings
      if [f] leaves many values unmodified.
      For each element, the new value [f x] and the old one [x] are compared
      with [<>].  Operates in [O(n)] time. *)
*)

  (**{6 Predicates}*)
    
  val for_all : ('a -> bool) -> 'a t -> bool
    (** [for_all p [a0; a1; ...; an]] checks if all elements of the array
	satisfy the predicate [p].  That is, it returns
	[ (p a0) && (p a1) && ... && (p an)]. *)

  val exists : ('a -> bool) -> 'a t -> bool
    (** [exists p [a0; a1; ...; an]] checks if at least one element of
	the array satisfies the predicate [p].  That is, it returns
	[ (p a0) || (p a1) || ... || (p an)]. *)

  val find : ('a -> bool) -> 'a t -> 'a
    (** [find p a] returns the first element of array [a]
	that satisfies the predicate [p].
	@raise Not_found  if there is no value that satisfies [p] in the
	array [a]. *)
    
  val mem : 'a -> 'a t -> bool
    (** [mem m a] is true if and only if [m] is equal to an element of [a]. *)
    
  val memq : 'a -> 'a t -> bool
    (** Same as {!Array.mem} but uses physical equality instead of
	structural equality to compare array elements.  *)
    
  val findi : ('a -> bool) -> 'a t -> int
    (** [findi p a] returns the index of the first element of array [a]
	that satisfies the predicate [p].
	@raise Not_found if there is no value that satisfies [p] in the
	array [a].  *)
    
  val filter : ('a -> bool) -> 'a t -> 'a t
    (** [filter f v] returns a vect with the elements [x] from [v] such that
	[f x] returns [true]. Operates in [O(n)] time. *)

  val filter_map : ('a -> 'b option) -> 'a t -> 'b t
    (** [filter_map f e] returns a vect consisting in all elements
	[x] such that [f y] returns [Some x] , where [y] is an element
	of [e]. *)

  val find_all : ('a -> bool) -> 'a t -> 'a t
    (** [find_all] is another name for {!Array.filter}. *)

  val partition : ('a -> bool) -> 'a t -> 'a t * 'a t
    (** [partition p a] returns a pair of arrays [(a1, a2)], where
	[a1] is the array of all the elements of [a] that
	satisfy the predicate [p], and [a2] is the array of all the
	elements of [a] that do not satisfy [p].
	The order of the elements in the input array is preserved. *)

(** {6 Convenience Functions} *)
  val first : 'a t -> 'a
  val last : 'a t -> 'a
    (** These return the first and last values in the vector *)
  val shift : 'a t -> 'a * 'a t
    (** Return the first element of a vector and its last [n-1] elements. *)
  val pop : 'a t -> 'a * 'a t
    (** Return the last element of a vector and its first [n-1] elements. *)

(** {6 Boilerplate code}*)

(** {7 Printing}*)
  
val print : ?first:string -> ?last:string -> ?sep:string -> ('a BatInnerIO.output -> 'b -> unit) ->  'a BatInnerIO.output -> 'b t -> unit


(** {6 Functorial interface} *)

module type RANDOMACCESS =
sig
  type 'a t
  val empty : 'a t
  val get : 'a t -> int -> 'a
  val unsafe_get : 'a t -> int -> 'a
  val set : 'a t -> int -> 'a -> unit
  val unsafe_set : 'a t -> int -> 'a -> unit
  val append : 'a t -> 'a t -> 'a t
  val concat : 'a t list -> 'a t
  val length : 'a t -> int
  val copy : 'a t -> 'a t
  val sub : 'a t -> int -> int -> 'a t
  val make : int -> 'a -> 'a t
  val iter : ('a -> unit) -> 'a t -> unit
  val map : ('a -> 'b) -> 'a t -> 'b t
  val fold_right : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val enum : 'a t -> 'a BatEnum.t
  val backwards : 'a t -> 'a BatEnum.t
  val of_enum : 'a BatEnum.t -> 'a t
  val of_backwards : 'a BatEnum.t -> 'a t
end

module Make :
  functor (R : RANDOMACCESS) ->
    functor (PARAM : sig val max_height : int val leaf_size : int end) ->
sig
  type 'a t
  (** The type of a polymorphic vect. *)

  exception Out_of_bounds
  (** Raised when an operation violates the bounds of the vect. *)

  val max_length : int
  (** Maximum length of the vect. *)

(** {6 Creation and conversions} *)

  val empty : 'a t
  (** The empty vect. *)

  val singleton : 'a -> 'a t
  (** Returns a vect of length 1 holding only the given element.*)

  val of_container : 'a R.t -> 'a t
  (** [of_array s] returns a vect corresponding to the container [s].
    Operates in [O(n)] time. *)

  val to_container : 'a t -> 'a R.t
  (**  [to_container r] returns a container corresponding to the vect [r]. *)

  val to_list : 'a t -> 'a list
  (** Returns a list with the elements contained in the vect. *)

  val make : int -> 'a -> 'a t
  (** [make i c] returns a vect of length [i] whose elements are all equal to
    [c]; it is similar to Array.make *)

(** {6 Properties } *)

  val is_empty : 'a t -> bool
  (** Returns whether the vect is empty or not. *)

  val height : 'a t -> int
  (** Returns the height (depth) of the vect. *)

  val length : 'a t -> int
  (** Returns the length of the vect ([O(1)]). *)

  val balance : 'a t -> 'a t
  (** [balance r] returns a balanced copy of the [r] vect. Note that vects are
    automatically rebalanced when their height exceeds a given threshold, but
    [balance] allows to invoke that operation explicity. *)

(** {6 Operations } *)

  val concat : 'a t -> 'a t -> 'a t
  (** [concat r u] concatenates the [r] and [u] vects. In general, it operates
    in [O(log(min n1 n2))] amortized time.
    Small vects are treated specially and can be appended/prepended in
    amortized [O(1)] time. *)

  val append : 'a -> 'a t -> 'a t
  (** [append c r] returns a new vect with the [c] element at the end
    in amortized [O(1)] time. *)

  val prepend : 'a -> 'a t -> 'a t
  (** [prepend c r] returns a new vect with the [c] character at the
    beginning in amortized [O(1)] time. *)

  val get : int -> 'a t -> 'a
  (** [get n r] returns the element at index [n] (starting from 0) in the vect [r].
   Operates in worst-case [O(log size)] time.
    Raises Out_of_bounds if a character out of bounds is requested. *)

  val set : 'a t -> int -> 'a  -> 'a t
  (** [set r n c] returns a copy of the [r] vect where the element at
      index [n] (see also [get]) has been set to [c].  Operates in
      worst-case [O(log size)] time. *)

  val sub : int -> int -> 'a t -> 'a t
  (** [sub m n r] returns a sub-vect of [r] containing all the elements
      whose indexes range from [m] to [m + n - 1] (included).
      Raises Out_of_bounds in the same cases as Array.sub.
      Operates in worst-case [O(log size)] time.  *)

  val insert : int -> 'a t -> 'a t -> 'a t
  (** [insert n r u] returns a copy of the [u] vect where [r] has been
    inserted between the elements with index [n] and [n + 1] in the
    original vect. The length of the new vect is
    [length u + length r].
    Operates in amortized [O(log(size r) + log(size u))] time. *)

  val remove : int -> int -> 'a t -> 'a t
  (** [remove m n r] returns the vect resulting from deleting the
    elements with indexes ranging from [m] to [m + n - 1] (included)
    from the original vect [r]. The length of the new vect is
    [length r - n].
    Operates in amortized [O(log(size r))] time. *)

(** {6 Iteration and higher-order functions } *)

  val iter : ('a -> unit) -> 'a t -> unit
  (** [iter f r] applies [f] to all the elements in the [r] vect,
    in order. *)

  val iteri : (int -> 'a -> unit) -> 'a t -> unit
  (** Operates like iter, but also passes the index of the character
    to the given function. *)

  val rangeiter : ('a -> unit) -> int -> int -> 'a t -> unit
  (** [rangeiter f m n r] applies [f] to all the elements whose
    indices [k] satisfy [m] <= [k] < [m + n].
    It is thus equivalent to [iter f (sub m n r)], but does not
    create an intermediary vect. [rangeiter] operates in worst-case
    [O(n + log m)] time, which improves on the [O(n log m)] bound
    from an explicit loop using [get].
    Raises Out_of_bounds in the same cases as [sub]. *)

  val fold_left : ('b -> 'a -> 'b ) -> 'b -> 'a t -> 'b
  (** [fold_left f a r] computes [ f (... (f (f a r0) r1)...) rN ]
    where [r0,r1..rN] are the indexed elements of [r]. *)

  val fold: ('b -> 'a -> 'b ) -> 'b -> 'a t -> 'b
    (**as {!fold_left}*)

  val fold_right : ('a -> 'b -> 'b ) -> 'a t -> 'b -> 'b
  (** [fold_right f r a] computes [ f (r0 (f r1 ... (f rN  a) ...)) ]
      where [r0,r1..rN] are the indexed elements of [r]. *)

  val map : ('a -> 'b) -> 'a t -> 'b t
  (** [map f v] returns a vect isomorphic to [v] where each element of index
      [i] equals [f (get v i)]. Therefore, the height of the returned vect
      is the same as that of the original one. Operates in [O(n)] time. *)

  val mapi : (int -> 'a -> 'b) -> 'a t -> 'b t
    (** Same as {!map}, but the
	function is applied to the index of the element as first argument,
	and the element itself as second argument. *)
(* NOT PROVIDED?
  val id_map : ('a -> 'a) -> 'a t -> 'a t
  (** [id_map f v] returns a vect isomorphic to [v] where each element of index
      [i] equals [f (get v i)]. It is very similar to [map], but tries to share
      as much data as possible with the original vect; for example,
      [id_map (fun x -> x) v == v]. This can lead to significative space savings
      if [f] leaves many values unmodified.
      For each element, the new value [f x] and the old one [x] are compared
      with [<>].  Operates in [O(n)] time. *)
*)
  val filter : ('a -> bool) -> 'a t -> 'a t
  (** [filter f v] returns a vect with the elements [x] from [v] such that
      [f x] returns [true]. Operates in [O(n)] time. *)

  val filter_map : ('a -> 'b option) -> 'a t -> 'b t
    (** [filter_map f e] returns a vect consisting in all elements
	[x] such that [f y] returns [Some x] , where [y] is an element
	of [e]. *)

  val find_all : ('a -> bool) -> 'a t -> 'a t
    (** [find_all] is another name for {!Array.filter}. *)

  val partition : ('a -> bool) -> 'a t -> 'a t * 'a t
    (** [partition p a] returns a pair of arrays [(a1, a2)], where
	[a1] is the array of all the elements of [a] that
	satisfy the predicate [p], and [a2] is the array of all the
	elements of [a] that do not satisfy [p].
	The order of the elements in the input array is preserved. *)

  (** {7 Printing}*)
  val print : ?first:string -> ?last:string -> ?sep:string -> ('a BatInnerIO.output -> 'b -> unit) ->  'a BatInnerIO.output -> 'b t -> unit
end

