(*
 * ExtUnit - Operations on Unit
 * Copyright (C) 2008 David Teller, LIFO, Universite d'Orleans
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
 * Foundation, Inc.
 *)


let unit_string = "()"

type t = unit
let as_string   = unit_string
let string_of _ = as_string
let of_string   = function
  | "()" -> ()
  | _    -> raise (Invalid_argument "unit_of_string")
let compare _ _ = 0
let print out t = BatInnerIO.nwrite out unit_string

