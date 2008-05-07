(* 
 * LazyListLabels - enumeration over abstract collection of elements.
 * Copyright (C) 2008 David Teller
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
exception No_more_elements    = LazyList.No_more_elements
exception Empty_list          = LazyList.Empty_list
exception Invalid_index       = LazyList.Invalid_index
exception Different_list_size = LazyList.Different_list_size
type 'a t                     = 'a LazyList.t
type 'a node_t                = 'a LazyList.node_t

let iter ~f x         = LazyList.iter f x
let iter2 ~f x        = LazyList.iter2 f x
let iteri ~f x        = LazyList.iteri f x
let map   ~f x        = LazyList.map   f x
let map2  ~f x        = LazyList.map2  f x
let mapi   ~f x       = LazyList.mapi  f x
let filter ~f         = LazyList.filter f
let exists ~f         = LazyList.exists f
let exists2 ~f        = LazyList.exists2 f
let for_all ~f        = LazyList.for_all f
let for_all2 ~f       = LazyList.for_all2 f
let filter_map ~f     = LazyList.filter_map f
let find ~f           = LazyList.find f
let findi ~f          = LazyList.findi f
let rfind ~f          = LazyList.rfind f
let rfindi ~f         = LazyList.rfindi f
let find_exn ~f       = LazyList.find_exn f
let rfind_exn ~f      = LazyList.rfind_exn f
let remove_if ~f      = LazyList.remove_if f
let remove_all_such ~f= LazyList.remove_all_such f
let take_while      ~f= LazyList.take_while f
let drop_while      ~f= LazyList.drop_while f
let fold_left ~f ~init  = LazyList.fold_left f init
let fold_right ~f ~init = LazyList.fold_right f init
let fold_left2 ~f ~init = LazyList.fold_left2 f init
let fold_right2 ~f l1 l2 ~init = LazyList.fold_right2 f l1 l2 init

let combine = LazyList.combine
let stable_sort = LazyList.stable_sort
let sort = LazyList.sort
let eternity = LazyList.eternity
let of_array = LazyList.of_array
let eager_of_list = LazyList.eager_of_list
let of_enum = LazyList.of_enum
let of_stream = LazyList.of_stream
let of_list = LazyList.of_list
let enum = LazyList.enum
let to_array = LazyList.to_array
let to_stream = LazyList.to_stream
let to_list = LazyList.to_list
let drop = LazyList.drop
let take = LazyList.take
let remove_all = LazyList.remove_all
let remove = LazyList.remove
let unique = LazyList.unique
let split_nth = LazyList.split_nth
let split_at = LazyList.split_at
let flatten = LazyList.flatten
let concat = LazyList.concat
let ( ^@^ ) = LazyList.( ^@^ )
let append = LazyList.append
let rev_append = LazyList.rev_append
let eager_append = LazyList.eager_append
let rev = LazyList.rev
let mem_assq = LazyList.mem_assq
let mem_assoc = LazyList.mem_assoc
let assq = LazyList.assq
let assoc = LazyList.assoc
let nth = LazyList.nth
let at = LazyList.at
let last = LazyList.last
let first = LazyList.first
let tl = LazyList.tl
let hd = LazyList.hd
let would_at_fail = LazyList.would_at_fail
let is_empty = LazyList.is_empty
let length = LazyList.length
let next = LazyList.next
let rindex_ofq = LazyList.rindex_ofq
let rindex_of = LazyList.rindex_of
let index_ofq = LazyList.index_ofq
let index_of = LazyList.index_of
let memq = LazyList.memq
let mem = LazyList.mem
let range = LazyList.range
let make = LazyList.make
let init = LazyList.init
let seq_hide = LazyList.seq_hide
let seq = LazyList.seq
let from_loop = LazyList.from_loop
let from_while = LazyList.from_while
let from = LazyList.from
let cons = LazyList.cons
let nil = LazyList.nil

module ExceptionLess = struct
  let find   ~f = LazyList.ExceptionLess.find f
  let rfind  ~f = LazyList.ExceptionLess.rfind f
  let findi  ~f = LazyList.ExceptionLess.findi f
  let rfindi ~f = LazyList.ExceptionLess.rfindi f

  let assq      = LazyList.ExceptionLess.assq
  let assoc     = LazyList.ExceptionLess.assoc
  let at        = LazyList.ExceptionLess.at
  let split_at  = LazyList.ExceptionLess.split_at

end
