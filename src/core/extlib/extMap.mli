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

    @documents Map
*)
module Map:
sig

  open ExtString
  open ExtInt

module type OrderedType = Interfaces.OrderedType
(** Input signature of the functor {!Map.Make}. *)

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
    (** [fold f m a] computes [(f kN dN ... (f k1 d1 a)...)],
       where [k1 ... kN] are the keys of all bindings in [m]
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
	  where [a0..an] are the elements of [m] and [key0..keyn] the
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
      
    val keys : _ t -> key Enum.t
      (** Return an enumeration of all the keys of a map.*)

    val values: 'a t -> 'a Enum.t
      (** Return an enumeration of al the values of a map.*)
(*
    val min_key : 'a t -> (key * 'a)
      (** return the ([key,value]) pair with the smallest key *)

    val max_key : 'a t -> (key * 'a)
      (** return the [(key,value)] pair with the largest key *)

    val choose : 'a t -> (key * 'a)
      (** return an implementation defined [(key,value)] pair.  As [Set.choose] *)
*)
(*
    val split : key -> 'a t -> ('a t * 'a option * 'a t)
      (** as [Set.split] *)
*)

    val enum  : 'a t -> (key * 'a) Enum.t
      (** Return an enumeration of (key, value) pairs of a map.*)

    val of_enum: (key * 'a) Enum.t -> 'a t
      (** Create a map from a (key, value) enumeration. *)

    (** {6 Boilerplate code}*)

    (** {7 Printing}*)

    val print :  ?first:string -> ?last:string -> ?sep:string -> 
      ('a InnerIO.output -> key -> unit) -> 
      ('a InnerIO.output -> 'c -> unit) -> 
      'a InnerIO.output -> 'c t -> unit

    (** Output signature of the functor {!Map.Make}. *)
      
    (** {6 Override modules}*)
      
    (**
       The following modules replace functions defined in {!Map} with functions
       behaving slightly differently but having the same name. This is by design:
       the functions meant to override the corresponding functions of {!Map}.
       
       To take advantage of these overrides, you probably want to
       {{:../extensions.html#multiopen}{open several modules in one
       operation}} or {{:../extensions.html#multialias}{alias several
       modules to one name}}. For instance, to open a version of {!Map}
       with exceptionless error management, you may write {v open Map,
       Exceptionless v}. To locally replace module {!Map} with a module of
       the same name but with exceptionless error management, you may
       write {v module Map = Map include Exceptionless v}.
       
    *)
      
    (** Operations on {!Map} without exceptions.*)
    module Exceptionless : sig
      val find: key -> 'a t -> 'a option
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

module RopeMap    : S with type key = Rope.t
(** A map on ropes. Comparison of ropes takes case into account (i.e. r"foo" <> r"Foo")*)

module IRopeMap   : S with type key = Rope.t
(** A map on ropes. Comparison of ropes ignores case (i.e. r"foo" = r"Foo")*)

module IntMap     : S with type key = Int.t
(** A map on integers.*)

module Make (Ord : OrderedType) : S with type key = Ord.t
(** Functor building an implementation of the map structure
   given a totally ordered type. 

    @documents Map.Make
*)
end
