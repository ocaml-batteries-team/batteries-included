(*
 * MultiMap - Polymorphic maps with multiple associations
 * Copyright (C) 1996-2003 Xavier Leroy, Nicolas Cannasse, Markus Mottl
 * Copyright (C) 2008      David Teller, LIFO, Universite d'Orleans
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


type ('a, 'b) t = ('a, 'b BatSet.t) BatMap.t

let empty = BatMap.empty

let is_empty = BatMap.is_empty

let find k t = try BatMap.find k t with Not_found -> BatSet.empty

let add k d t = BatMap.modify_def BatSet.empty k (BatSet.add d) t

let remove_all k t = BatMap.remove k t

let remove k d t =
  try
    let set = BatSet.remove d (BatMap.find k t) in
    if   BatSet.is_empty set then BatMap.remove k t
    else BatMap.add k set t;
  with Not_found -> t

let mem = BatMap.mem
(* let exists = mem *)
let iter = BatMap.iter
let map = BatMap.map
let mapi = BatMap.mapi
let fold = BatMap.fold
let foldi = BatMap.foldi
let modify = BatMap.modify
let modify_def = BatMap.modify_def
let modify_opt = BatMap.modify_opt

let (|>) x f = f x
let enum t = BatMap.enum t |> BatEnum.map (fun (k,s) -> BatSet.enum s |> BatEnum.map (fun x -> (k,x))) |> BatEnum.concat

let of_enum e = BatEnum.fold (fun acc (k,d) -> add k d acc) empty e

let print ?(first="{\n") ?(last="\n}") ?(sep=",\n") ?(kvsep=": ") print_k print_v out t =
  let print_one out (k,v) =
    BatPrintf.fprintf out "%a%s%a" print_k k kvsep print_v v
  in
  BatEnum.print ~first ~last ~sep print_one out (enum t)

module Infix =
struct
  let (-->) map key = find key map
  let (<--) map (key, value) = add key value map
end
