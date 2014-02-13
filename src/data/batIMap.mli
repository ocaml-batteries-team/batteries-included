(* $Id: iMap.mli,v 1.1 2003/12/19 17:24:34 yori Exp $ *)
(* Copyright 2003 Yamagata Yoriyuki. distributed with LGPL *)
(* Modified by Edgar Friendly <thelema314@gmail.com> *)

(** DIET Maps from integers, packed using ranges *)

(* Note: Lost covariance with use of record for keeping eq *)
type 'a t (*= (int * int * 'a) BatAvlTree.tree*)

type key = int

val empty : eq:('a -> 'a -> bool) -> 'a t
(** The empty map.  Needs one parameter: a comparison function for the
    values, to enable merging of ranges with identical values. *)

val singleton : eq:('a -> 'a -> bool) -> int -> 'a -> 'a t

val is_empty : 'a t -> bool
(** Test whether a map is empty (i.e. has no bindings) *)

val add : int -> 'a -> 'a t -> 'a t
(** [add x y t] adds a binding from [x] to [y] in [t], returning a new map. *)

val add_range : int -> int -> 'a -> 'a t -> 'a t
(** [add lo hi y t] adds bindings to [y] for all values in the range
    [lo,hi], returning a new map *)

val find : int -> 'a t -> 'a
(** [find x t] returns the [y] that is bound to [x] in [t].

    @raise Not_found if [x] is unbound *)

val modify : int -> ('a -> 'a) -> 'a t -> 'a t
(** [modify x f t] replaces the [y] that is bound to [x] in [t] by [f y].

    @raise Not_found if [x] is unbound
    @since 2.1 *)

val modify_def : 'a -> int -> ('a -> 'a) -> 'a t -> 'a t
(** [modify_def dft x f t] does the same as [modify x f t] but binds
    [x] to [f dft] if [x] was not bound.

    @since 2.1 *)

val modify_opt : int -> ('a option -> 'a option) -> 'a t -> 'a t
(** [modify_opt x f t] allows to modify the binding for [x] in [t]
    or absence thereof.

    @since 2.1 *)

val remove : int -> 'a t -> 'a t
(** Remove any bindings from the given value. *)

val remove_range : int -> int -> 'a t -> 'a t
(** Remove any bindings within the given range *)

val from : int -> 'a t -> 'a t
(** Return the sub-map of bindings in the range [x,max_int] *)

val after : int -> 'a t -> 'a t
(** Return the sub-map of bindings in the range [x+1,max_int] *)

val until : int -> 'a t -> 'a t
(** Return the sub-map of bindings in the range [min_int, x] *)

val before : int -> 'a t -> 'a t
(** Return the sub-map of bindings in the range [min_int, x-1] *)

val mem : int -> 'a t -> bool
(** Test whether there is a binding from the given int *)

val iter : (int -> 'a -> unit) -> 'a t -> unit
(** [iter f t] calls [f] on every binding *)

val iter_range : (int -> int -> 'a -> unit) -> 'a t -> unit
(** [iter_range f t] calls [f] on every contiguous range.  For maps, contiguous ranges must map to the same [y] *)

val map : ?eq:('b -> 'b -> bool) -> ('a -> 'b) -> 'a t -> 'b t
(** Create a new map by modifying each [y] by the given function.
    This will not create new ranges; the mapping function is only
    applied to each contiguous range once.  It is not applied to the
    ranges in order. [~eq] defaults to (=). *)

val mapi : ?eq:('b -> 'b -> bool) -> (int -> 'a -> 'b) -> 'a t -> 'b t
(** Create a new map by computing new values based on key and value
    of the existing bindings.  This can create new ranges, as adjacent
    bindings can be assigned different values. [~eq] defaults to (=). *)

val map_range : ?eq:('b -> 'b -> bool) -> (int -> int -> 'a -> 'b) -> 'a t -> 'b t
(** Create a new map by modifying each [y] using the given function.
    This will not create new ranges, but will have access to the
    [lo,hi] of the current range.  [~eq] defaults to (=). *)

val fold : (int -> 'b -> 'a -> 'a) -> 'b t -> 'a -> 'a
(** [fold f t x0] folds all the bindings of [t] into [x0] using [f] to
    merge. *)

val fold_range : (int -> int -> 'b -> 'a -> 'a) -> 'b t -> 'a -> 'a
(** [fold_range f t x0] folds all the contiguous ranges of [t] into
    [x0] using [f] to merge. The order of foldings is unspecified.*)

val set_to_map : ?eq:('a -> 'a -> bool) -> BatISet.t -> 'a -> 'a t
(** [set_to_map s x] returns a map where every element of [s] is bound
    to [x]. *)


val domain : 'a t -> BatISet.t
(** [domain t] returns the set of ints that are bound in [t] *)

val map_to_set : ('a -> bool) -> 'a t -> BatISet.t
(** [map_to_set p t] returns the set of keys of [t] where [p]
    evaluates as true *)

val enum : 'a t -> (int * int * 'a) BatEnum.t
(** [enum t] returns an enumeration of the bindings in [t] *)

val of_enum : eq:('a -> 'a -> bool) -> (int * int * 'a) BatEnum.t -> 'a t
(** [of_enum e] returns the set of given ranges *)

val fold2_range : (int -> int -> 'a option -> 'b option -> 'c -> 'c) -> 'a t -> 'b t -> 'c -> 'c

(** [fold2_range f t u x0] folds across each range that's defined in
    either [t] or [u] or both, giving that range and the possible values
    to [f] to merge with [x0].

    Example: let union_first = fold2_range (fun _lo _hi a b = match a,b with Some x,_ -> x | _,Some y -> y)
*)

val union : ('a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t
(** Merge two maps, giving a value *)

val merge :  ?eq:('c -> 'c -> bool) -> (int -> int -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t


val forall2_range : (int -> int -> 'a option -> 'b option -> bool) -> 'a t -> 'b t -> bool


(** Get the equality function used in an IMap.t *)
val get_dec_eq : 'a t -> ('a -> 'a -> bool)

(** Infix operators over a {!BatIMap} *)
module Infix : sig
  val (-->) : 'a t -> int -> 'a
  (** [map-->key] returns the current binding of [key] in [map],
      or @raise Not_found if no such binding exists.
      Equivalent to [find key map]. *)

  val (<--) : 'a t -> int * 'a -> 'a t
    (** [map<--(key, value)] returns a map containing the same bindings as
        [map], plus a binding of [key] to [value]. If [key] was already bound
        in [map], its previous binding disappears. Equivalent to [add key value map]

        {b Important warning}: {!BatIMap.add} takes an optional argument, [eq] that
        is missing in this operator [<--]. As a consequence, using [<--] implies the
        use of {{:http://caml.inria.fr/pub/docs/manual-ocaml/libref/Pervasives.html#VAL(==)}Pervasives.(==)}
        as comparison function.
    *)
end
