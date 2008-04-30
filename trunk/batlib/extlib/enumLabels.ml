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

include Enum


let iter ~f x     = Enum.iter f x

let iter2 ~f x y  = Enum.iter2 f x y

let iteri ~f x    = Enum.iteri f x

let iter2i ~f x y = Enum.iter2i f x y


let fold ~f ~init x    = Enum.fold f init x

let fold2 ~f ~init x y = Enum.fold2 f init x y

let foldi ~f ~init x   = Enum.foldi f init x

let fold2i ~f ~init x y= Enum.fold2i f init x y

let find ~f x    = Enum.find f x

let find_exn ~f x= Enum.find_exn f x

let map ~f x     = Enum.map f x

let mapi ~f x    = Enum.mapi f x

let filter ~f x  = Enum.filter f x

let filter_map ~f x= Enum.filter_map f x

let init x ~f    = Enum.init x f

let switch ~f    = Enum.switch f

let switchn x ~f   = Enum.switchn x f
(*Everything else is identical*)
