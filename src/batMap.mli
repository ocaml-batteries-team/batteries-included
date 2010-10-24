(* 
 * ExtMap - Additional map operations
 * Copyright (C) 1996 Xavier Leroy
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

(** Association tables over ordered types.

   This module implements applicative association tables, also known as
   finite maps or dictionaries, given a total ordering function
   over the keys.
   All operations over maps are purely applicative (no side-effects).
   The implementation uses balanced binary trees, and therefore searching
   and insertion take time logarithmic in the size of the map.

    @author Xavier Leroy (Base module)
    @author David Rajchenbach-Teller
*)

(* module type OrderedType = BatInterfaces.OrderedType *)

module type S =
  sig
    type key
    (** The type of the map keys. *)

    type (+'a) t
    (** The type of maps from type [key] to type ['a]. *)

    val empty: 'a t
    (** The empty map. *)

    val is_empty: 'a t -> bool
    (** Test whether a map is empty or not. *)

    val cardinal: 'a t -> int
    (** Return the number of bindings of a map. *)

    val add: key -> 'a -> 'a t -> 'a t
    (** [add x y m] returns a map containing the same bindings as
       [m], plus a binding of [x] to [y]. If [x] was already bound
       in [m], its previous binding disappears. *)

    val find: key -> 'a t -> 'a
    (** [find x m] returns the current binding of [x] in [m],
       or raises [Not_found] if no such binding exists. *)

    val remove: key -> 'a t -> 'a t
    (** [remove x m] returns a map containing the same bindings as
       [m], except for [x] which is unbound in the returned map. *)

    val modify: key -> ('a -> 'a) -> 'a t -> 'a t
      (** [modify k f m] replaces the previous binding for [k] with [f] applied to
	  that value. If [k] is unbound in [m] or [Not_found] is raised during the
	  search, [Not_found] is raised.

	  @since 1.2.0
	  @raise Not_found if [k] is unbound in [m] (or [f] raises [Not_found])
*)

    val modify_def: 'a -> key -> ('a -> 'a) -> 'a t -> 'a t
    (** [modify_def v0 k f m] replaces the previous binding for [k]
	with [f] applied to that value. If [k] is unbound in [m] or
	[Not_found] is raised during the search, [f v0] is
	inserted (as if the value found were .

	@since 1.3.0
     *)

    val mem: key -> 'a t -> bool
    (** [mem x m] returns [true] if [m] contains a binding for [x],
       and [false] otherwise. *)

    val iter: (key -> 'a -> unit) -> 'a t -> unit
    (** [iter f m] applies [f] to all bindings in map [m].
       [f] receives the key as first argument, and the associated value
       as second argument.  The bindings are passed to [f] in increasing
       order with respect to the ordering over the type of the keys.
       Only current bindings are presented to [f]:
       bindings hidden by more recent bindings are not passed to [f]. *)

    val map: ('a -> 'b) -> 'a t -> 'b t
    (** [map f m] returns a map with same domain as [m], where the
       associated value [a] of all bindings of [m] has been
       replaced by the result of the application of [f] to [a].
       The bindings are passed to [f] in increasing order
       with respect to the ordering over the type of the keys. *)

    val mapi: (key -> 'a -> 'b) -> 'a t -> 'b t
    (** Same as {!Map.S.map}, but the function receives as arguments both the
       key and the associated value for each binding of the map. *)

    val fold: (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    (** [fold f m a] computes [(f kN dN ... (f k1 d1 (f k0 d0 a))...)],
       where [k0,k1..kN] are the keys of all bindings in [m]
       (in increasing order), and [d1 ... dN] are the associated data. *)

    val filter: ('a -> bool) -> 'a t -> 'a t
      (**[filter f m] returns a map where only the values [a] of [m]
	 such that [f a = true] remain. The bindings are passed to [f]
	 in increasing order with respect to the ordering over the
	 type of the keys. *)

    val filteri: (key -> 'a -> bool) -> 'a t -> 'a t
      (**[filter f m] returns a map where only the key, values pairs
	 [key], [a] of [m] such that [f key a = true] remain. The
	 bindings are passed to [f] in increasing order with respect
	 to the ordering over the type of the keys. *)

    val filter_map: (key -> 'a -> 'b option) -> 'a t -> 'b t
      (** [filter_map f m] combines the features of [filteri] and
	  [map].  It calls calls [f key0 a0], [f key1 a1], [f keyn an]
	  where [a0,a1..an] are the elements of [m] and [key0..keyn] the
	  respective corresponding keys. It returns the map of
	  pairs [keyi],[bi] such as [f keyi ai = Some bi] (when [f] returns
	  [None], the corresponding element of [m] is discarded). *)

    val compare: ('a -> 'a -> int) -> 'a t -> 'a t -> int
      (** Total ordering between maps.  The first argument is a total ordering
          used to compare data associated with equal keys in the two maps. *)

    val equal: ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    (** [equal cmp m1 m2] tests whether the maps [m1] and [m2] are
       equal, that is, contain equal keys and associate them with
       equal data.  [cmp] is the equality predicate used to compare
       the data associated with the keys. *)
      
    val keys : _ t -> key BatEnum.t
      (** Return an enumeration of all the keys of a map.*)

    val values: 'a t -> 'a BatEnum.t
      (** Return an enumeration of al the values of a map.*)
    
    val min_binding : 'a t -> (key * 'a)
      (** return the ([key,value]) pair with the smallest key *)

    val max_binding : 'a t -> (key * 'a)
      (** return the [(key,value)] pair with the largest key *)

   (* The following documentations comments are from stdlib's map.mli:
        - choose
        - split
        - singleton
   *)
    val choose : 'a t -> (key * 'a)
    (** Return one binding of the given map, or raise [Not_found] if
       the map is empty. Which binding is chosen is unspecified,
       but equal bindings will be chosen for equal maps.
     *)

    val split : key -> 'a t -> ('a t * 'a option * 'a t)
    (** [split x m] returns a triple [(l, data, r)], where
          [l] is the map with all the bindings of [m] whose key
        is strictly less than [x];
          [r] is the map with all the bindings of [m] whose key
        is strictly greater than [x];
          [data] is [None] if [m] contains no binding for [x],
          or [Some v] if [m] binds [v] to [x].
     *)

    val singleton: key -> 'a -> 'a t
    (** [singleton x y] returns the one-element map that contains a binding [y]
        for [x].
     *)

    val enum  : 'a t -> (key * 'a) BatEnum.t
      (** Return an enumeration of (key, value) pairs of a map.
        The returned enumeration is sorted in increasing order with respect
        to the ordering [Ord.compare], where [Ord] is the argument given to
        {!Map.Make}. *)

    val backwards  : 'a t -> (key * 'a) BatEnum.t
      (** Return an enumeration of (key, value) pairs of a map.
        The returned enumeration is sorted in decreasing order with respect
        to the ordering [Ord.compare], where [Ord] is the argument given to
        {!Map.Make}. *)

    val of_enum: (key * 'a) BatEnum.t -> 'a t
      (** Create a map from a (key, value) enumeration. *)

    val for_all: (key -> 'a -> bool) -> 'a t -> bool
    (** [for_all p m] checks if all the bindings of the map
        satisfy the predicate [p].
    *)

    val exists: (key -> 'a -> bool) -> 'a t -> bool
    (** [exists p m] checks if at least one binding of the map
        satisfy the predicate [p].
    *)


    (** {6 Boilerplate code}*)

    (** {7 Printing}*)

    val print :  ?first:string -> ?last:string -> ?sep:string -> 
      ('a BatInnerIO.output -> key -> unit) -> 
      ('a BatInnerIO.output -> 'c -> unit) -> 
      'a BatInnerIO.output -> 'c t -> unit

    (** Output signature of the functor {!Map.Make}. *)
      
    (** {6 Override modules}*)
      
    (**
       The following modules replace functions defined in {!Map} with functions
       behaving slightly differently but having the same name. This is by design:
       the functions meant to override the corresponding functions of {!Map}.
       
    *)
      
    (** Operations on {!Map} without exceptions.*)
    module Exceptionless : sig
      val find: key -> 'a t -> 'a option
    end

    (** Infix operators over a {!BatMap} *)
    module Infix : sig
      val (-->) : 'a t -> key -> 'a
        (** [map-->key] returns the current binding of [key] in [map],
            or raises [Not_found]. Equivalent to [find key map]. *)

      val (<--) : 'a t -> key * 'a -> 'a t
        (** [map<--(key, value)] returns a map containing the same bindings as
            [map], plus a binding of [key] to [value]. If [key] was already bound
            in [map], its previous binding disappears. Equivalent
            to [add key value map]*)
    end

    (** Operations on {!Map} with labels.
	
	This module overrides a number of functions of {!Map} by
	functions in which some arguments require labels. These labels are
	there to improve readability and safety and to let you change the
	order of arguments to functions. In every case, the behavior of the
	function is identical to that of the corresponding function of {!Map}.
    *)
    module Labels : sig
      val add : key:key -> data:'a -> 'a t -> 'a t
      val iter : f:(key:key -> data:'a -> unit) -> 'a t -> unit
      val map : f:('a -> 'b) -> 'a t -> 'b t
      val mapi : f:(key:key -> data:'a -> 'b) -> 'a t -> 'b t
      val filter: f:('a -> bool) -> 'a t -> 'a t
      val filteri:f:(key -> 'a -> bool) -> 'a t -> 'a t
      val fold :
	f:(key:key -> data:'a -> 'b -> 'b) ->
	'a t -> init:'b -> 'b
      val compare: cmp:('a -> 'a -> int) -> 'a t -> 'a t -> int
      val equal: cmp:('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    end
  end

module StringMap  : S with type key = String.t
(** A map on strings. Comparison of strings takes case into account (i.e. "foo" <> "Foo")*)

module IStringMap : S with type key = String.t
(** A map on strings. Comparison of strings ignores case (i.e. "foo" = "Foo")*)

module NumStringMap : S with type key = String.t
(** A map on strings. Strings are handled as prefix + number (i.e. "abc23" < "abc123", "abc012" = "abc12")*)

(* 

module RopeMap    : S with type key = BatRope.t
(** A map on ropes. Comparison of ropes takes case into account (i.e. r"foo" <> r"Foo")*)

module IRopeMap   : S with type key = BatRope.t
(** A map on ropes. Comparison of ropes ignores case (i.e. r"foo" = r"Foo")*)

 *)

module IntMap     : S with type key = BatInt.t
(** A map on integers.*)

module Make (Ord : BatInterfaces.OrderedType) : S with type key = Ord.t
(** Functor building an implementation of the map structure
   given a totally ordered type. 

    @documents Map.Make
*)

INCLUDE "batPMap.mli"

(**/**)
module type OrderedType = BatInterfaces.OrderedType
(** Input signature of the functor {!Map.Make}. *)
