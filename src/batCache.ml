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

let (|>) x f = f x
let tap f x = f x; x

type ('a,'b) manual_cache = {
  get : 'a -> 'b; 
  del : 'a -> unit; 
  enum: unit -> ('a * 'b) BatEnum.t
}

let make_ht ~gen init_size =
  let ht = BatHashtbl.create init_size in
  {get = (fun k -> 
    try BatHashtbl.find ht k
    with Not_found -> gen k |> tap (BatHashtbl.add ht k));
   del = (fun k -> BatHashtbl.remove ht k);
   enum = (fun () -> BatHashtbl.enum ht) }

let make_map ~gen =
  let m = ref BatMap.empty in
  {get = (fun k -> 
    try BatMap.find k !m
    with Not_found -> gen k |> tap (fun v -> m := BatMap.add k v !m));
   del = (fun k -> m := BatMap.remove k !m);
   enum = (fun () -> BatMap.enum !m) }

type ('a, 'b) auto_cache = 'a -> 'b

let lru_cache ~gen size =
  let entries = ref None in
  let get k = 
    match !entries with 
      | Some dll ->
	let n = 
	  try BatDllist.find (fun (k1,v1) -> k1 = k) dll |> tap BatDllist.remove
	  with Not_found -> BatDllist.create (k, gen k)
	in
	BatDllist.splice n dll;
	BatDllist.get n |> snd
      | None -> let v = gen k in entries := Some (BatDllist.create (k, v)); v
  in
  get
