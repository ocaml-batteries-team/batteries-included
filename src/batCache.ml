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

open BatInnerPervasives

type ('a,'b) manual_cache = {
  get : 'a -> 'b;
  del : 'a -> unit;
  enum: unit -> ('a * 'b) BatEnum.t
}

let make_ht ~gen ~init_size =
  let ht = BatHashtbl.create init_size in
  {get = (fun k ->
     try BatHashtbl.find ht k
     with Not_found -> gen k |> tap (BatHashtbl.add ht k));
   del = (fun k -> BatHashtbl.remove ht k);
   enum = (fun () -> BatHashtbl.enum ht) }

(* For tests, use side effects to count number of times the function
   is run *)
(*$T make_ht
  let runs = ref 0 in let c = make_ht ~gen:(fun x -> incr runs; x) ~init_size:5 in let s = c.get 3 + c.get 4 + c.get 3 in s = 10 && !runs = 2
*)

let make_map ~gen =
  let m = ref BatMap.empty in
  {get = (fun k ->
     try BatMap.find k !m
     with Not_found -> gen k |> tap (fun v -> m := BatMap.add k v !m));
   del = (fun k -> m := BatMap.remove k !m);
   enum = (fun () -> BatMap.enum !m) }

(*$T make_map
  let runs = ref 0 in let c = make_map ~gen:(fun x -> incr runs; x) in let s = c.get 3 + c.get 4 + c.get 3 in s = 10 && !runs = 2
*)

type ('a, 'b) auto_cache = 'a -> 'b

let lru_cache ~gen ~cap =
  let entries = ref None in
  let len = ref 0 in
  let get k =
    match !entries with (* remove match by replacing get after first run? *)
    | Some dll -> (* not at head of list *)
      let (k0,v) = BatDllist.get dll in
      if k = k0 then v (* special case head of list *)
      else
        let n =
          try BatDllist.find (fun (k1,_v1) -> k1 = k) dll |> tap BatDllist.remove
          with Not_found -> incr len; BatDllist.create (k, gen k)
        in
        (* Put n at the head of the list *)
        BatDllist.splice n dll; entries := Some n;
        (* Remove the tail if over capacity *)
        if !len > cap then (BatDllist.remove (BatDllist.prev n); decr len);
        (*          BatDllist.print (BatTuple.Tuple2.print BatPervasives.print_any BatPervasives.print_any) BatIO.stdout n; *)
        (* return the value *)
        BatDllist.get n |> snd
    | None -> (* no list - generate it *)
      let v = gen k in entries := Some (BatDllist.create (k, v)); incr len; v
  in
  get

  (* WARNING: s is evaluated right to left *)
  (*$T lru_cache
    let runs = ref 0 in let id = lru_cache ~gen:(fun x -> incr runs; x) ~cap:3 in \
    let s = id 1 + id 1 + id 3 + id 3 + id 2 + id 1 in\
    s = 11 && !runs = 3

    let runs = ref 0 in let id = lru_cache ~gen:(fun x -> incr runs; x) ~cap:3 in \
    let s = id 1 + id 1 + id 4 + id 3 + id 2 + id 1 in \
    s = 12 && !runs = 5

  *)
