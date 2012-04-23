(*
 * PSet - Polymorphic sets
 * Copyright (C) 1996-2003 Xavier Leroy, Nicolas Cannasse, Markus Mottl
 * Copyright (C)      2009 David Rajchenbach-Teller, LIFO, Universite d'Orleans
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


(**
   Polymorphic sets

   The functions below are now integrated as part of the {!BatSet}
   module. Please go there to find detailed documentation.
*)

type 'a t

include BatEnum.Enumerable with type 'a enumerable = 'a t
include BatInterfaces.Mappable with type 'a mappable = 'a t

val empty: 'a t

val create : ('a -> 'a -> int) -> 'a t

val singleton : ?cmp:('a -> 'a -> int) -> 'a -> 'a t

val is_empty: 'a t -> bool

val mem: 'a -> 'a t -> bool

val add: 'a -> 'a t -> 'a t

val remove: 'a -> 'a t -> 'a t

val iter: ('a -> unit) -> 'a t -> unit

val map: ('a -> 'a) -> 'a t -> 'a t

val filter: ('a -> bool) -> 'a t -> 'a t

val filter_map: ('a -> 'b option) -> 'a t -> 'b t

val fold: ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b

val exists: ('a -> bool) -> 'a t -> bool

val cardinal: 'a t -> int

val choose : 'a t -> 'a

val min_elt : 'a t -> 'a

val max_elt : 'a t -> 'a

val enum: 'a t -> 'a BatEnum.t

val of_enum: 'a BatEnum.t -> 'a t

val of_enum_cmp: cmp:('a -> 'a -> int) -> 'a BatEnum.t -> 'a t

val of_list: 'a list -> 'a t

val for_all : ('a -> bool) -> 'a t -> bool

val partition : ('a -> bool) -> 'a t -> 'a t * 'a t

val split : 'a -> 'a t -> 'a t * bool * 'a t

val filter : ('a -> bool) -> 'a t -> 'a t

val pop : 'a t -> 'a * 'a t

val union: 'a t -> 'a t -> 'a t

val diff: 'a t -> 'a t -> 'a t

val intersect: 'a t -> 'a t -> 'a t

val subset: 'a t -> 'a t -> bool

val print :  ?first:string -> ?last:string -> ?sep:string ->
  ('a BatInnerIO.output -> 'c -> unit) ->
  'a BatInnerIO.output -> 'c t -> unit

