(* $Id: iSet.mli,v 1.1 2003/12/19 17:24:34 yori Exp $ *)
(* Copyright 2003 Yamagata Yoriyuki. distributed with LGPL *)

(** DIET : Discrete Interval Encoding Trees

    Sets of integers represented as ranges

    This data structure is efficient for large sets of integers where
    many adjacent integers are all part of the set.  This will have
    higher overhead for sets with lots of point elements, but will be
    much more efficient for sets containing mostly ranges.
*)

type t = (int * int) BatAvlTree.tree
(** the underlying representation is a balanced tree of ranges *)

type elt = int
(** This kind of set only holds ints *)

val empty : t
(** The empty set *)

val is_empty : t -> bool
(** Test whether a set is empty, returns [true] if the set is empty. *)

val mem : int -> t -> bool
(** test whether a given int is a member of the set *)

val add : int -> t -> t
(** Add the given int to the set, returning a new set *)

val add_range : int -> int -> t -> t
(** [add_range lo hi t] adds the range of integers [lo, hi] (including both endpoints) to
    the given set, returning a new set
    @raise Invalid_argument if [lo] > [hi] *)

val singleton : int -> t
(** Return the singleton set containing only the given element *)

val remove : int -> t -> t
(** Remove an element from the given set, returning a new set *)

val remove_range : int -> int -> t -> t
(** [remove_range lo hi t] removes a range of elements from the given set, returning a new set
    @raise Invalid_argument if [lo] > [hi] *)

val union : t -> t -> t
(** Compute the union of two sets.  This is the set whose elements are
    those elements in either input set. *)

val inter : t -> t -> t
(** Compute the intersection of two sets.  This is the set whose
    elements are those in *both* of the input sets. *)

val diff : t -> t -> t
(** Compute the difference between two sets.  This is the set of
    elements that are in the first but not in the second. Unlike
    [union] and [inter], order matters here.*)

val compl : t -> t
(** Create the complement of the given set - i.e. the set of all
    values not in the input set. *)

val compare : t -> t -> int
(** Compare two sets.  It is not safe to use the polymorphic (<) and
    related functions to compare these sets, as the tree representation
    used can balance in multiple ways. *)

val equal : t -> t -> bool
(** Test whether two sets are equal.  It is not safe to use the
    polymorphic (=) on these sets, as the same set can have multiple
    representations depending on how it was built. *)

val ord : t -> t -> BatOrd.order
(** Same as [compare] but returns [BatOrd.Lt | BatOrd.Eq | BatOrd.Gt]
    instead of an int. *)

val subset : t -> t -> bool
(** [subset t u] returns [true] if [t] is a subset of [u] *)

val from : int -> t -> t
(** [from x t] returns the portion of [t] in the range [x, max_int] *)

val after : int -> t -> t
(** [after x t] returns the portion of [t] in the range [x+1, max_int] *)

val until : int -> t -> t
(** [until x t] returns the portion of [t] in the range [min_int, x] *)

val before : int -> t -> t
(** [before x t] returns the portion of [t] in the range [min_int, x-1] *)

val iter : (int -> unit) -> t -> unit
(** [iter f t] calls [f] once for each element of [t] *)

val iter_range : (int -> int -> unit) -> t -> unit
(** [iter_range f t] calls [f] once for each contiguous range of [t].
    The contiguous ranges of a set are sequences of adjacent integers
    all part of the set. *)

val fold : (int -> 'a -> 'a) -> t -> 'a -> 'a
(** [fold f t x0] returns the final result of merging each element of
    [t] into [x0] using merge function [f] *)

val fold_range : (int -> int -> 'a -> 'a) -> t -> 'a -> 'a
(** As fold, but operates on contiguous ranges *)

val for_all : (int -> bool) -> t -> bool
(** Tests whether a predicate applies to all elements of the set *)

val exists : (int -> bool) -> t -> bool
(** Test whether some element of a set satisfies a predicate *)

val filter : (int -> bool) -> t -> t
(** Builds the subset of those elements that satisfy the predicate *)

val partition : (int -> bool) -> t -> t * t
(** partitions the input set into two sets with elements that satisfy
    the predicate and those that don't *)

val cardinal : t -> int
(** Returns the number of elements in the set *)

val elements : t -> int list
(** Returns a list of all elements in the set *)

val ranges : t -> (int * int) list
(** Returns a list of all contiguous ranges in the set *)

val min_elt : t -> int
(** Returns the minimum element in the set *)

val max_elt : t -> int
(** Returns the maximum element in the set *)

val choose : t -> int
(** Returns some element in the set *)

val enum : t -> (int * int) BatEnum.t
(** Enumerates all contiguous ranges in the set *)

val of_enum : (int*int) BatEnum.t -> t
val of_list : (int*int) list -> t
(** Build a ISet.t out of a list or enum of ranges *)

val print : _ BatIO.output -> t -> unit
