(*
 * Splay -- splay trees
 * Copyright (C) 2011  Batteries Included Development Team
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

(** Maps over ordered types based on splay trees.

    Splay trees are ordered binary trees that have the 
    most recently used element as the root of the tree.
    If another element is accessed (even read-only), 
    the tree will be rearranged internally.
    
    Not threadsafe; even read-only functions will rearrange
    the tree, even though its contents will remain unchanged.
 *)

module Map (Ord : BatInterfaces.OrderedType)
  : sig
    include BatMap.S with type key = Ord.t
    val print_as_list:
      ('a BatInnerIO.output -> key -> unit) ->
      ('a BatInnerIO.output -> 'c -> unit) ->
      'a BatInnerIO.output -> 'c t -> unit
    val of_list : (Ord.t * 'a) list -> 'a t
    val to_list : 'a t -> (Ord.t * 'a) list
  end
