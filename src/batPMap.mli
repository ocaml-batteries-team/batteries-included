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

    The functions below are now integrated as part of the {!BatMap}
    module. Please go there to find detailed documentation.
*)

type ('a, 'b) t

val empty : ('a, 'b) t

val is_empty : ('a, 'b) t -> bool

val create : ('a -> 'a -> int) -> ('a, 'b) t

val singleton : ?cmp:('a -> 'a -> int) -> 'a -> 'b -> ('a, 'b) t

val cardinal: ('a, 'b) t -> int

val add : 'a -> 'b -> ('a, 'b) t -> ('a, 'b) t

val find : 'a -> ('a, 'b) t -> 'b

val remove : 'a -> ('a, 'b) t -> ('a, 'b) t

val mem : 'a -> ('a, 'b) t -> bool

val exists : 'a -> ('a, 'b) t -> bool

val iter : ('a -> 'b -> unit) -> ('a, 'b) t -> unit

val map : ('b -> 'c) -> ('a, 'b) t -> ('a, 'c) t

val mapi : ('a -> 'b -> 'c) -> ('a, 'b) t -> ('a, 'c) t

val fold : ('b -> 'c -> 'c) -> ('a , 'b) t -> 'c -> 'c

val foldi : ('a -> 'b -> 'c -> 'c) -> ('a , 'b) t -> 'c -> 'c

val filter: ('a -> bool) -> ('key, 'a) t -> ('key, 'a) t
  
val filteri: ('key -> 'a -> bool) -> ('key, 'a) t -> ('key, 'a) t
  
val filter_map: ('key -> 'a -> 'b option) -> ('key, 'a) t -> ('key, 'b) t

val choose : ('key, 'a) t -> ('key * 'a)

val split : 'key -> ('key, 'a) t -> (('key, 'a) t * 'a option * ('key, 'a) t)

val min_binding : ('key, 'a) t -> ('key * 'a)

val max_binding : ('key, 'a) t -> ('key * 'a)

val enum : ('a, 'b) t -> ('a * 'b) BatEnum.t

val backwards  : ('a,'b) t -> ('a * 'b) BatEnum.t

val keys : ('a,'b) t -> 'a BatEnum.t
  
val values: ('a,'b) t -> 'b BatEnum.t

val of_enum : ?cmp:('a -> 'a -> int) -> ('a * 'b) BatEnum.t -> ('a, 'b) t

val for_all : ('a -> 'b -> bool) -> ('a, 'b) t -> bool

val exists_f : ('a -> 'b -> bool) -> ('a, 'b) t -> bool

val partition : ('a -> 'b -> bool) -> ('a, 'b) t -> ('a, 'b) t * ('a, 'b) t

val add_carry : 'a -> 'b -> ('a, 'b) t -> ('a, 'b) t * 'b option

val modify : 'a -> ('b -> 'b) -> ('a, 'b) t -> ('a, 'b) t

val modify_def: 'b -> 'a -> ('b -> 'b) -> ('a,'b) t -> ('a,'b) t

val extract : 'a -> ('a, 'b) t -> 'b * ('a, 'b) t

val pop : ('a, 'b) t -> ('a * 'b) * ('a, 'b) t

val union : ('a, 'b) t -> ('a, 'b) t -> ('a, 'b) t

val diff :  ('a, 'b) t -> ('a, 'b) t -> ('a, 'b) t

val intersect : ('b -> 'c -> 'd) -> ('a, 'b) t -> ('a, 'c) t -> ('a, 'd) t

val split : 'a -> ('a, 'b) t -> (('a, 'b) t * 'b option * ('a, 'b) t)

val merge:
  ('key -> 'a option -> 'b option -> 'c option)
  -> ('key, 'a) t -> ('key, 'b) t -> ('key, 'c) t

val merge_unsafe:
  ('key -> 'a option -> 'b option -> 'c option)
  -> ('key, 'a) t -> ('key, 'b) t -> ('key, 'c) t

module Exceptionless : sig
  val find: 'a -> ('a,'b) t -> 'b option
end

module Infix : sig
  val (-->) : ('a, 'b) t -> 'a -> 'b

  val (<--) : ('a, 'b) t -> 'a * 'b -> ('a, 'b) t
end

val bindings : ('key, 'a) t -> ('key * 'a) list

val print :  ?first:string -> ?last:string -> ?sep:string -> 
  ('a BatInnerIO.output -> 'b -> unit) -> 
  ('a BatInnerIO.output -> 'c -> unit) -> 
  'a BatInnerIO.output -> ('b, 'c) t -> unit
