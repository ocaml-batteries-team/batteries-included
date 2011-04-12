(*
 * Cache - Simple (and maybe complex) caching structures
 * Copyright (C) 2011 Batteries Included Team
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


type ('a,'b) manual_cache = {
  get : 'a -> 'b; 
  del : 'a -> unit; 
  enum: unit -> ('a * 'b) BatEnum.t
}

val make_ht : gen:('a -> 'b) -> int -> ('a,'b) manual_cache

val make_map : gen:('a -> 'b) -> ('a,'b) manual_cache
(** These functions build a cache with either a hashtbl or a map.  The
    [cache.get] function gets a value from the cache, generating it
    with the generator function [gen] and adding it to the cache if
    needed.  The [cache.del] function removes a value from the
    cache. *)

type ('a, 'b) auto_cache = 'a -> 'b

val lru_cache : gen:('a -> 'b) -> int -> ('a, 'b) auto_cache



(* TODO
val rec_cache : gen:(('a -> 'b) -> 'a -> 'b) -> ('a, 'b) manual_cache

 *)
