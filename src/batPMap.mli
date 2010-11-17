(*
 * PMap - Polymorphic maps
 * Copyright (C) 1996-2003 Xavier Leroy, Nicolas Cannasse, Markus Mottl
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

(** Polymorphic Map.

    This is a polymorphic map, similar to standard library [Map] module
    but in a defunctorized style.

    @author Xavier Leroy (Base library)
    @author Nicolas Cannasse
    @author Markus Mottl
    @author David Rajchenbach-Teller
*)

type ('a, 'b) t

val empty : ('a, 'b) t
(** The empty map, using [compare] as key comparison function. *)

val is_empty : ('a, 'b) t -> bool
(** returns true if the map is empty. *)

val create : ('a -> 'a -> int) -> ('a, 'b) t
(** creates a new empty map, using the provided function for key comparison.*)

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
(** same as [mem]. *)

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
    and [d0,d1.dN] are the associated data.
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

val choose : ('key, 'a) t -> ('key * 'a)
  (** returns one pair of the given map, deterministically *)

val min_binding : ('key, 'a) t -> ('key * 'a)
  (** returns the binding with the smallest key *)

val max_binding : ('key, 'a) t -> ('key * 'a)
  (** returns the binding with the largest key *)

val enum : ('a, 'b) t -> ('a * 'b) BatEnum.t
(** creates an enumeration for this map. *)

val of_enum : ?cmp:('a -> 'a -> int) -> ('a * 'b) BatEnum.t -> ('a, 'b) t
(** creates a map from an enumeration, using the specified function
  for key comparison or [compare] by default. *)

val for_all : ('a -> 'b -> bool) -> ('a, 'b) t -> bool
(** Tests whether all key value pairs satisfy some predicate function *)

val exists_f : ('a -> 'b -> bool) -> ('a, 'b) t -> bool
(** Tests whether some key value pair satisfies some predicate function *)

val partition : ('a -> 'b -> bool) -> ('a, 'b) t -> ('a, 'b) t * ('a, 'b) t
(** Divides a map into two maps based on a test function *)

val add_carry : 'a -> 'b -> ('a, 'b) t -> ('a, 'b) t * 'b option
(** [add_carry k v m] adds the binding [(k,v)] to [m], returning the new map and optionally the previous value bound to [k]. *)

val modify : 'a -> ('b -> 'b) -> ('a, 'b) t -> ('a, 'b) t
  (** [modify k f m] replaces the previous binding for [k] with [f]
      applied to that value.  If [k] is unbound in [m] or [Not_found] is
      raised during the search, [m] is returned unchanged. *)

val extract : 'a -> ('a, 'b) t -> 'b * ('a, 'b) t
  (** [extract k m] removes the current binding of [k] from [m],
      returning the value [k] was bound to and the updated [m]. *)

val pop : ('a, 'b) t -> ('a * 'b) * ('a, 'b) t
  (** [pop m] returns a binding from [m] and [m] without that
      binding. *)

val union : ('a, 'b) t -> ('a, 'b) t -> ('a, 'b) t
  (** [union m1 m2] merges two maps, using the comparison function of
      the second map and containing all bindings of the two maps.  In
      case of conflicted bindings, the first map's bindings override the
      second map's. Equivalent to [foldi add m1 m2]*)

val diff :  ('a, 'b) t -> ('a, 'b) t -> ('a, 'b) t
  (** [diff m1 m2] removes all bindings of keys found in [m2] from [m1].  Equivalent to [fold remove m2 m1] *)

val split : 'a -> ('a, 'b) t -> (('a, 'b) t * 'b option * ('a, 'b) t)

(** Infix operators over a {!BatPMap} *)
module Infix : sig
  val (-->) : ('a, 'b) t -> 'a -> 'b
    (** [map-->key] returns the current binding of [key] in [map],
        or raises [Not_found] if no such binding exists.
        Equivalent to [find key map]. *)

  val (<--) : ('a, 'b) t -> 'a * 'b -> ('a, 'b) t
    (** [map<--(key, value)] returns a map containing the same bindings as
        [map], plus a binding of [key] to [value]. If [key] was already bound
        in [map], its previous binding disappears. Equivalent to [add key value map] *)
end

(** {6 Boilerplate code}*)

(** {7 Printing}*)
  
val print :  ?first:string -> ?last:string -> ?sep:string -> 
  ('a BatInnerIO.output -> 'b -> unit) -> 
  ('a BatInnerIO.output -> 'c -> unit) -> 
  'a BatInnerIO.output -> ('b, 'c) t -> unit
