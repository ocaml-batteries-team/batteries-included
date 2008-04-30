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

exception Empty_list = LazyList.Empty_list
exception Invalid_index = LazyList.Invalid_index
exception Different_list_size = LazyList.Different_list_size


type 'a node_t = 'a LazyList.node_t
and 'a t       = 'a LazyList.t

let nil               = LazyList.nil
let length            = LazyList.length
let hd                = LazyList.hd
let tl                = LazyList.tl
let nth               = LazyList.nth
let at_exn            = LazyList.at_exn
let at                = LazyList.at
let rev               = LazyList.rev
let eager_append      = LazyList.eager_append
let rev_append        = LazyList.rev_append
let append            = LazyList.append
let (^@^)             = LazyList.append
let concat            = LazyList.concat
let flatten           = LazyList.flatten
let to_list           = LazyList.to_list
let to_array          = LazyList.to_array
let enum              = LazyList.enum
let of_list           = LazyList.of_list
let of_enum           = LazyList.of_enum
let eager_of_list     = LazyList.eager_of_list
let of_array          = LazyList.of_array
let range             = LazyList.range
let mem               = LazyList.mem
let eternity          = LazyList.eternity

let iter ~f x         = LazyList.iter f x
let iteri ~f x        = LazyList.iteri f x
let map   ~f x        = LazyList.map   f x
let fold_left ~f ~init= LazyList.fold_left f init
let fold_right ~f ~init=LazyList.fold_right f init
let filter ~f         = LazyList.filter f
let exists ~f         = LazyList.exists f
let for_all ~f        = LazyList.for_all f
let map_filter ~f     = LazyList.map_filter f
let from              = LazyList.from
