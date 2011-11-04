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

    {b Note} OCaml, Batteries Included, provides two implementations of
    maps: polymorphic maps and functorized maps. Functorized maps (see
    {!S} and {!Make}) are slightly more complex to use but offer
    stronger type-safety. Polymorphic maps make it easier to shoot
    yourself in the foot. In case of doubt, you should use functorized
    maps.

    {4 Functorized maps}
    
    The important part is the {!Make} module which builds association
    maps from a user-provided datatype and comparison function. In the
    {!Make} module (or its output signature {!S}) are documentated all
    functions available on maps.

    Here is a typical example of use:

    {[
    module MyKeyType = struct
      type t = my_type
      let compare = my_compare_function
    end
    module MyMap = Map.Make(MyKeyType)

    let some_map = MyMap.add something MyMap.empty
    ...
    ]}

    You can also use predefined maps such as {!IntMap} for maps with
    integer keys.

    @author Xavier Leroy (Base library)
    @author Nicolas Cannasse
    @author Markus Mottl
    @author David Rajchenbach-Teller
    @author Gabriel Scherer
*)
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

    val extract : key -> 'a t -> 'a * 'a t
    (** [extract k m] removes the current binding of [k] from [m],
        returning the value [k] was bound to and the updated [m].
        
        @since 1.4.0
    *)

    val pop : 'a t -> (key * 'a) * 'a t
    (** [pop m] returns a binding from [m] and [m] without that
        binding.
        
        @since 1.4.0
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
        - partition
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

    val partition: (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
    (** [partition p m] returns a pair of maps [(m1, m2)], where
        [m1] contains all the bindings of [s] that satisfy the
        predicate [p], and [m2] is the map with all the bindings of
        [s] that do not satisfy [p].
        @since 1.4.0
     *)


    val singleton: key -> 'a -> 'a t
    (** [singleton x y] returns the one-element map that contains a binding [y]
        for [x].
     *)

    val bindings : 'a t -> (key * 'a) list
    (** Return the list of all bindings of the given map.
        The returned list is sorted in increasing key order.
    
        Added for compatibility with stdlib 3.12
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

    val merge:
         (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t
    (** [merge f m1 m2] computes a map whose keys is a subset of keys of [m1]
        and of [m2]. The presence of each such binding, and the corresponding
        value, is determined with the function [f].
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
*)


(** {4 Polymorphic maps}

    The functions below present the manipulation of polymorphic maps,
    as were provided by the Extlib PMap module.

    They are similar in functionality to the functorized {!Make}
    module, but the compiler cannot ensure that maps using different
    key ordering have different types: the responsibility of not
    mixing non-sensical comparison functions together is to the
    programmer. If in doubt, you should rather use the {!Make}
    functor for additional safety.
*)

type ('a, 'b) t

val empty : ('a, 'b) t
(** The empty map, using [compare] as key comparison function. *)

val is_empty : ('a, 'b) t -> bool
(** returns true if the map is empty. *)

val create : ('a -> 'a -> int) -> ('a, 'b) t
(** creates a new empty map, using the provided function for key comparison.*)

val singleton : ?cmp:('a -> 'a -> int) -> 'a -> 'b -> ('a, 'b) t
(** creates a new map with a single binding *)

val cardinal: ('a, 'b) t -> int
(** Return the number of bindings of a map. *)

val add : 'a -> 'b -> ('a, 'b) t -> ('a, 'b) t
(** [add x y m] returns a map containing the same bindings as
    [m], plus a binding of [x] to [y]. If [x] was already bound
    in [m], its previous binding disappears. *)

val find : 'a -> ('a, 'b) t -> 'b
(** [find x m] returns the current binding of [x] in [m],
    or raises [Not_found] if no such binding exists. *)

val remove : 'a -> ('a, 'b) t -> ('a, 'b) t
(** [remove x m] returns a map containing the same bindings as
    [m], except for [x] which is unbound in the returned map. *)

val mem : 'a -> ('a, 'b) t -> bool
(** [mem x m] returns [true] if [m] contains a binding for [x],
    and [false] otherwise. *)

val exists : 'a -> ('a, 'b) t -> bool
(** same as [mem].

    @deprecated [mem] should be used instead, as [exists] conflicts with the function checking arbitrary predicates, which is instead named [exists_f].
*)

val iter : ('a -> 'b -> unit) -> ('a, 'b) t -> unit
(** [iter f m] applies [f] to all bindings in map [m].
    [f] receives the key as first argument, and the associated value
    as second argument. The order in which the bindings are passed to
    [f] is unspecified. Only current bindings are presented to [f]:
    bindings hidden by more recent bindings are not passed to [f]. *)

val map : ('b -> 'c) -> ('a, 'b) t -> ('a, 'c) t
(** [map f m] returns a map with same domain as [m], where the
    associated value [a] of all bindings of [m] has been
    replaced by the result of the application of [f] to [a].
    The order in which the associated values are passed to [f]
    is unspecified. *)

val mapi : ('a -> 'b -> 'c) -> ('a, 'b) t -> ('a, 'c) t
(** Same as [map], but the function receives as arguments both the
    key and the associated value for each binding of the map. *)

val fold : ('b -> 'c -> 'c) -> ('a , 'b) t -> 'c -> 'c
(** [fold f m a] computes [(f kN dN ... (f k1 d1 (f k0 d0 a))...)],
    where [k0,k1..kN] are the keys of all bindings in [m],
    and [d0,d1..dN] are the associated data.
    The order in which the bindings are presented to [f] is
    unspecified. *)

val foldi : ('a -> 'b -> 'c -> 'c) -> ('a , 'b) t -> 'c -> 'c
(** Same as [fold], but the function receives as arguments both the
    key and the associated value for each binding of the map. *)

val filter: ('a -> bool) -> ('key, 'a) t -> ('key, 'a) t
(**[filter f m] returns a map where only the values [a] of [m]
   such that [f a = true] remain. The bindings are passed to [f]
   in increasing order with respect to the ordering over the
   type of the keys. *)
  
val filteri: ('key -> 'a -> bool) -> ('key, 'a) t -> ('key, 'a) t
(**[filter f m] returns a map where only the (key, value) pairs
   [key], [a] of [m] such that [f key a = true] remain. The
   bindings are passed to [f] in increasing order with respect
   to the ordering over the type of the keys. *)
  
val filter_map: ('key -> 'a -> 'b option) -> ('key, 'a) t -> ('key, 'b) t
(** [filter_map f m] combines the features of [filteri] and
    [map].  It calls calls [f key0 a0], [f key1 a1], [f keyn an]
    where [a0..an] are the elements of [m] and [key0..keyn] the
    respective corresponding keys. It returns the map of
    pairs [keyi],[bi] such as [f keyi ai = Some bi] (when [f] returns
    [None], the corresponding element of [m] is discarded). *)

(* The following documentations comments are from stdlib's map.mli:
   - choose
   - split
*)
val choose : ('key, 'a) t -> ('key * 'a)
(** Return one binding of the given map, or raise [Not_found] if
    the map is empty. Which binding is chosen is unspecified,
    but equal bindings will be chosen for equal maps.
*)

val split : 'key -> ('key, 'a) t -> (('key, 'a) t * 'a option * ('key, 'a) t)
(** [split x m] returns a triple [(l, data, r)], where
      [l] is the map with all the bindings of [m] whose key
    is strictly less than [x];
      [r] is the map with all the bindings of [m] whose key
    is strictly greater than [x];
      [data] is [None] if [m] contains no binding for [x],
      or [Some v] if [m] binds [v] to [x].
*)

val min_binding : ('key, 'a) t -> ('key * 'a)
(** returns the binding with the smallest key *)

val max_binding : ('key, 'a) t -> ('key * 'a)
(** returns the binding with the largest key *)

val enum : ('a, 'b) t -> ('a * 'b) BatEnum.t
(** creates an enumeration for this map, enumerating key,value pairs with the keys in increasing order. *)

val backwards  : ('a,'b) t -> ('a * 'b) BatEnum.t
(** creates an enumeration for this map, enumerating key,value pairs with the keys in decreasing order. *)

val keys : ('a,'b) t -> 'a BatEnum.t
(** Return an enumeration of all the keys of a map.*)
  
val values: ('a,'b) t -> 'b BatEnum.t
(** Return an enumeration of al the values of a map.*)

val of_enum : ?cmp:('a -> 'a -> int) -> ('a * 'b) BatEnum.t -> ('a, 'b) t
(** creates a map from an enumeration, using the specified function
    for key comparison or [compare] by default. *)

val for_all : ('a -> 'b -> bool) -> ('a, 'b) t -> bool
(** Tests whether all key value pairs satisfy some predicate function *)

val exists_f : ('a -> 'b -> bool) -> ('a, 'b) t -> bool
(** Tests whether some key value pair satisfies some predicate function *)

(* documentation comment from INRIA's stdlib *)
val partition : ('a -> 'b -> bool) -> ('a, 'b) t -> ('a, 'b) t * ('a, 'b) t
(** [partition p m] returns a pair of maps [(m1, m2)], where [m1]
    contains all the bindings of [s] that satisfy the predicate
    [p], and [m2] is the map with all the bindings of [s] that do
    not satisfy [p]. *)

val add_carry : 'a -> 'b -> ('a, 'b) t -> ('a, 'b) t * 'b option
(** [add_carry k v m] adds the binding [(k,v)] to [m], returning the new map and optionally the previous value bound to [k]. *)

val modify : 'a -> ('b -> 'b) -> ('a, 'b) t -> ('a, 'b) t
    (** [modify k f m] replaces the previous binding for [k] with [f]
	applied to that value.  If [k] is unbound in [m] or [Not_found] is
	raised during the search,  [Not_found] is raised.

	@since 1.2.0
	@raise Not_found if [k] is unbound in [m] (or [f] raises [Not_found]) *)

val modify_def: 'b -> 'a -> ('b -> 'b) -> ('a,'b) t -> ('a,'b) t
(** [modify_def v0 k f m] replaces the previous binding for [k]
    with [f] applied to that value. If [k] is unbound in [m] or
    [Not_found] is raised during the search, [f v0] is
    inserted (as if the value found were [v0]).

    @since 1.3.0
 *)


val extract : 'a -> ('a, 'b) t -> 'b * ('a, 'b) t
(** [extract k m] removes the current binding of [k] from [m],
    returning the value [k] was bound to and the updated [m]. *)

val pop : ('a, 'b) t -> ('a * 'b) * ('a, 'b) t
(** [pop m] returns a binding from [m] and [m] without that
    binding. *)

val union : ('a, 'b) t -> ('a, 'b) t -> ('a, 'b) t
(** [union m1 m2] merges two maps, using the comparison function of
    [m1]. In case of conflicted bindings, [m2]'s bindings override
    [m1]'s. Equivalent to [foldi add m2 m1].
    The resulting map uses the comparison function of [m1]. *)

val diff :  ('a, 'b) t -> ('a, 'b) t -> ('a, 'b) t
(** [diff m1 m2] removes all bindings of keys found in [m2] from [m1],
    using the comparison function of [m1]. Equivalent to
      [foldi (fun k _v m -> remove k m) m2 m1]
    The resulting map uses the comparison function of [m1].*)

val intersect : ('b -> 'c -> 'd) -> ('a, 'b) t -> ('a, 'c) t -> ('a, 'd) t
(** [intersect merge_f m1 m2] returns a map with bindings only for
    keys bound in both [m1] and [m2], and with [k] bound to [merge_f
    v1 v2], where [v1] and [v2] are [k]'s bindings in [m1] and [m2].
    The resulting map uses the comparison function of [m1]. *)

val merge:
  ('key -> 'a option -> 'b option -> 'c option)
  -> ('key, 'a) t -> ('key, 'b) t -> ('key, 'c) t
(** [merge f m1 m2] computes a map whose keys is a subset of keys of [m1]
    and of [m2]. The presence of each such binding, and the corresponding
    value, is determined with the function [f].    
    The resulting map uses the comparison function of [m1].
*)

val merge_unsafe:
  ('key -> 'a option -> 'b option -> 'c option)
  -> ('key, 'a) t -> ('key, 'b) t -> ('key, 'c) t
(** Same as merge, but assumes the comparison function of both maps
    are equal. If it's not the case, the result is a map using the
    comparison function of its first parameter, but which ['b option]
    elements are passed to the function is unspecified.
*)

(** Exceptionless versions of functions *)
module Exceptionless : sig
  val find: 'a -> ('a,'b) t -> 'b option
end



(** Infix operators over a {!BatPMap} *)
module Infix : sig
  val (-->) : ('a, 'b) t -> 'a -> 'b
    (** [map-->key] returns the current binding of [key] in [map],
        or raises [Not_found]. Equivalent to [find key map]. *)

  val (<--) : ('a, 'b) t -> 'a * 'b -> ('a, 'b) t
    (** [map<--(key, value)] returns a map containing the same bindings as
        [map], plus a binding of [key] to [value]. If [key] was already bound
        in [map], its previous binding disappears. Equivalent
        to [add key value map]*)
end

val bindings : ('key, 'a) t -> ('key * 'a) list
(** Return the list of all bindings of the given map.
    The returned list is sorted in increasing key order.

    Added for compatibility with stdlib 3.12
*)


(** {6 Boilerplate code}*)

(** {7 Printing}*)
  
val print :  ?first:string -> ?last:string -> ?sep:string -> 
  ('a BatInnerIO.output -> 'b -> unit) -> 
  ('a BatInnerIO.output -> 'c -> unit) -> 
  'a BatInnerIO.output -> ('b, 'c) t -> unit




(**/**)
module type OrderedType = BatInterfaces.OrderedType
(** Input signature of the functor {!Map.Make}. *)
(**/**)
