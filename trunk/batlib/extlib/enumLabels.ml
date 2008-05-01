(*
 * EnumLabels - enumeration over abstract collection of elements.
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


(**
   For more information on this module, see Enum
*)

let iter ~f x     = Enum.iter f x

let iter2 ~f x y  = Enum.iter2 f x y

let iteri ~f x    = Enum.iteri f x

let iter2i ~f x y = Enum.iter2i f x y


let fold ~f ~init x    = Enum.fold f init x

let fold2 ~f ~init x y = Enum.fold2 f init x y

let foldi ~f ~init x   = Enum.foldi f init x

let fold2i ~f ~init x y= Enum.fold2i f init x y

let find ~f x    = Enum.find f x

let map ~f x     = Enum.map f x

let mapi ~f x    = Enum.mapi f x

let filter ~f x  = Enum.filter f x

let filter_map ~f x= Enum.filter_map f x

let init x ~f    = Enum.init x f

let switch ~f    = Enum.switch f

let switchn x ~f   = Enum.switchn x f

let take_while ~f  = Enum.take_while f

let drop_while ~f  = Enum.drop_while f

let from ~f        = Enum.from f

let from_while ~f  = Enum.from_while f

let from_loop ~init ~f = Enum.from_loop init f

let from_loop_while ~init ~f = Enum.from_loop_while init f



module ExceptionLess = struct
  let find ~f = Enum.ExceptionLess.find f
end

(*Everything else is identical*)

let slazy = Enum.slazy
let lsing = Enum.lsing
let lcons = Enum.lcons
let lapp = Enum.lapp
let ising = Enum.ising
let icons = Enum.icons
let iapp = Enum.iapp
let seq = Enum.seq
let ( -- ) = Enum.( -- )
let range = Enum.range
let fast_count = Enum.fast_count
let count = Enum.count
let cycle = Enum.cycle
let repeat = Enum.repeat
let singleton = Enum.singleton
let make = Enum.make
let empty = Enum.empty
exception Infinite_enum = Enum.Infinite_enum
exception No_more_elements = Enum.No_more_elements
let concat = Enum.concat
let append = Enum.append
let force = Enum.force
let clone = Enum.clone
let close = Enum.close
let drop = Enum.drop
let junk = Enum.junk
let push = Enum.push
let get = Enum.get
let peek = Enum.peek
let is_empty = Enum.is_empty
type 'a t = 'a Enum.t
