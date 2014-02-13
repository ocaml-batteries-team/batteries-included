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


(** The data structure for a manual cache with keys ['a] and values ['b].

    This cache gives access to some internals of a memoized function
    [f], called the generating function.  This function is usually
    pure; always returning the same output for a given input.  This
    module allows the results of complex functions to be cached so
    that the function does not need to be called again to get that
    result.

    When [c.get k] is called, an internal structure is first consulted to
    see if [f k] has been stored in that structure.  If it has, then
    that previous result is returned, otherwise, [f k] is evaluated,
    the result stored in the caching structure and also returned to
    the user.

    The user is allowed to remove a value from the caching structure
    with [c.del k].  This allows the user to prevent unbounded growth
    of the cache by removing unneeded entries.  If the user prefers an
    automatically managed cache, this module provides [!auto_cache].

    Last, [c.enum ()] will enumerate all the currently memorized
    bindings as pairs.  This allows inspection of what is currently
    cached.
*)
type ('a,'b) manual_cache = {
  get : 'a -> 'b;
  del : 'a -> unit;
  enum: unit -> ('a * 'b) BatEnum.t
}


val make_ht : gen:('a -> 'b) -> init_size:int -> ('a,'b) manual_cache
(** Make a manual cache backed by a hashtable.  The generating
    function is passed with [~gen] and the initial size of the
    hashtable is [~init_size]. The hashtable uses the polymorphic
    [hash] and [(=)].*)


val make_map : gen:('a -> 'b) -> ('a,'b) manual_cache
(** Make a manual cache for function [~gen] backed by a Set.t.  This
    set uses the polymorphic [(<)] for comparison, so ['a] should be
    properly comparable by it. *)

(** Automatically managed caches

    This type of cache is more transparent than the [!manual_cache]
    above.  It does not provide inspection of the caching structure,
    but guarantees bounded memory usage through some policy of
    discarding some entries.

    Each auto-cache can have a different policy to decide which entry
    to discard.

*)
type ('a, 'b) auto_cache = 'a -> 'b


val lru_cache : gen:('a -> 'b) -> cap:int -> ('a, 'b) auto_cache
  (* Make a simple LRU (least-recently-used) automatic cache for
     function [~gen] and with capacity [~cap].  When a new entry is
     added to the cache, if its capacity was [cap], then the least
     recently used entry in the cache will be removed to make room for
     it. *)


  (* TODO
     val rec_cache : gen:(('a -> 'b) -> 'a -> 'b) -> ('a, 'b) manual_cache
     val other_fancy_caching_strategy : (as lru_cache, probably)
  *)
