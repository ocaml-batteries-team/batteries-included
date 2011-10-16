(*
 * BatCounter
 * Copyright (C) 2011 Edgar Friendly <thelema314@gmail.com>
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

(* Inspired by Coda Hale's Metrics *)

type t = int ref

let make () = ref 0

let atomic_add x n = x := !x + n
let get_and_set x n = let r = !x in x := n; r

let inc x = incr x
let add x n = atomic_add x n
let dec x = decr x
let sub x n = atomic_add x (0-n)
let count x = !x
let clear x = x := 0
let get_and_clear x = get_and_set x 0
