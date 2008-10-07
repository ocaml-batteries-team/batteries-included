(* Vect: extensible arrays based on ropes as described in

   Boehm, H., Atkinson, R., and Plass, M. 1995. Ropes: an alternative to
   strings.  Softw. Pract. Exper. 25, 12 (Dec. 1995), 1315-1330.

   Motivated by Luca de Alfaro's extensible array implementation Vec.

   Copyright (C) 2007   Mauricio Fernandez <mfp@acm.org>
                        http://eigenclass.org

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Library General Public
   License as published by the Free Software Foundation; either
   version 2 of the License, or (at your option) any later version,
   with the following special exception:

   You may link, statically or dynamically, a "work that uses the
   Library" with a publicly distributed version of the Library to
   produce an executable file containing portions of the Library, and
   distribute that executable file under terms of your choice, without
   any of the additional requirements listed in clause 6 of the GNU
   Library General Public License.  By "a publicly distributed version
   of the Library", we mean either the unmodified Library as
   distributed by the author, or a modified version of the Library that is
   distributed under the conditions defined in clause 2 of the GNU
   Library General Public License.  This exception does not however
   invalidate any other reasons why the executable file might be
   covered by the GNU Library General Public License.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.

   The GNU Library General Public License is available at
   http://www.gnu.org/copyleft/lgpl.html; to obtain it, you can also
   write to the Free Software Foundation, Inc., 59 Temple Place -
   Suite 330, Boston, MA 02111-1307, USA.
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

type 'a t with sexp
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

val get : int -> 'a t -> 'a
  (** [get n r] returns the (n+1)th element from the vect [r]; i.e.
      [get 0 r] returns the first element.
      Operates in worst-case [O(log size)] time.
      Raises Out_of_bounds if a character out of bounds is requested. *)

val at : int -> 'a t -> 'a
  (** as [get] *)

val set : int -> 'a -> 'a t -> 'a t
  (** [set n c r] returns a copy of the [r] vect where the (n+1)th element
      (see also [get]) has been set to [c].
      Operates in worst-case [O(log size)] time. *)

val destructive_set : int -> 'a -> 'a t -> unit
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

val map : ('a -> 'b) -> 'a t -> 'b t
  (** [map f v] returns a vect isomorphic to [v] where each element of index
    [i] equals [f (get v i)]. Therefore, the height of the returned vect
    is the same as that of the original one. Operates in [O(n)] time. *)

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
  (** [get n r] returns the (n+1)th element from the vect [r]; i.e.
    [get 0 r] returns the first element.
    Operates in worst-case [O(log size)] time.
    Raises Out_of_bounds if a character out of bounds is requested. *)

  val set : int -> 'a -> 'a t -> 'a t
  (** [set n c r] returns a copy of the [r] vect where the (n+1)th element
    (see also [get]) has been set to [c].
    Operates in worst-case [O(log size)] time. *)

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

  val fold : ('b -> 'a -> 'b ) -> 'b -> 'a t -> 'b
  (** [fold_left f a r] computes [ f (... (f (f a r0) r1)...) rN-1 ]
    where [rn = Vect.get n r ] and [N = length r]. *)

(* NOT PROVIDED?
  val fold_right : ('a -> 'b -> 'b ) -> 'a t -> 'b -> 'b
  (** [fold_right f r a] computes [ f (r0 ... (f rN-2 (f rN-1 a)) ...)) ]
      where [rn = Vect.get n r ] and [N = length r]. *)
*)

  val map : ('a -> 'b) -> 'a t -> 'b t
  (** [map f v] returns a vect isomorphic to [v] where each element of index
      [i] equals [f (get v i)]. Therefore, the height of the returned vect
      is the same as that of the original one. Operates in [O(n)] time. *)
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
end
