(***********************************************************************)
(*                                                                     *)
(*                            Weaktbl                                  *)
(*                                                                     *)
(*             (C) 2007 by Zheng Li (li@pps.jussieu.fr)                *)
(*                                                                     *)
(*  This program is free software; you can redistribute it and/or      *)
(*  modify it under the terms of the GNU Lesser General Public         *)
(*  License version 2.1 as published by the Free Software Foundation,  *)
(*  with the special exception on linking described in file LICENSE.   *)
(*                                                                     *)
(*  This program is distributed in the hope that it will be useful,    *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of     *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the      *)
(*  GNU Library General Public License for more details.               *)
(*                                                                     *)
(***********************************************************************)


(* weak stack, for ordering purpose *)
module Stack = struct
  type 'a t = {mutable data:'a Weak.t; mutable length:int; mutable cursor:int}
  let create n = 
    let len = min n (Sys.max_array_length - 1) in
    {data = Weak.create len; length = len; cursor = 0}
  let iter f s = 
    for i = s.cursor -1 downto 0 do
      match Weak.get s.data i with Some x -> f x | _ -> ()
    done
  let length s = (* resize by the way, since it's invoked by push *)
    let flag = ref false and pt = ref 0 in
    for i = 0 to s.cursor -1 do
      match Weak.get s.data i with
      | Some x as d -> if !flag then Weak.set s.data !pt d; incr pt
      | None -> flag := true
    done;
    s.cursor <- !pt; s.cursor
  let copy s = 
    let s' = create s.length in 
    Weak.blit s.data 0 s'.data 0 s.cursor; s'.cursor <- s.cursor; s'
  let rec push x s = 
    if s.cursor < s.length then
      (Weak.set s.data s.cursor (Some x); s.cursor <- s.cursor + 1)
    else
      let len = length s in
      if len >= s.length / 3 && len < s.length * 2 / 3 then push x s else
        let len' = min (len * 3 / 2 + 2) (Sys.max_array_length -1) in
        if len' = len then failwith "Weaktbl.Stack.push: stack cannnot grow"
        else
          let data' = Weak.create len' in
          Weak.blit s.data 0 data' 0 s.cursor; 
          s.data <- data'; s.length <- len'; push x s
  let rec pop s =
    if s.cursor <= 0 then raise Not_found;
    s.cursor <- s.cursor -1;
    match Weak.get s.data s.cursor with Some x -> x | None -> pop s
  let rec top s =
    if s.cursor <= 0 then raise Not_found;
    match Weak.get s.data (s.cursor -1) with 
    | Some x -> x | None -> s.cursor <- s.cursor -1; top s
  let is_empty s = (* stop as earlier as we can *)
    try iter (fun _ -> raise Not_found) s; true with Not_found -> false
end

open Obj (* Recover polymorphism from standard monomorphic (Weak)Hashtbl *)
module Make (H: Hashtbl.HashedType) : Hashtbl.S with type key = H.t = struct
  type box = H.t Weak.t
  let enbox k = let w = Weak.create 1 in Weak.set w 0 (Some k); w
  let unbox bk = Weak.get bk 0 
  type bind = box * t
  let bind_new k v = enbox k, repr v
  type cls = bind Stack.t
  let cls_new bd = let cls = Stack.create 1 in Stack.push bd cls; cls
  let dummy k = cls_new (bind_new k ())
  let rec top_bind cls = 
    let (bk,v) as bind = Stack.top cls in 
    match unbox bk with 
    | Some k -> k, (obj v) | _ -> assert (bind == Stack.pop cls); top_bind cls
  let top_key cls = fst (top_bind cls) and top_value cls = snd (top_bind cls)
  let all_bind cls =
    let l = ref [] in
    let f (bk,v) = match unbox bk with 
      | Some k -> l := (k, obj v) :: !l | _ -> () in
    Stack.iter f cls; List.rev !l
  let all_key cls = List.map fst (all_bind cls) 
  and all_value cls = List.map snd (all_bind cls) 
  module HX = struct
    type t = cls
    let hash x = try H.hash (top_key x) with Not_found -> 0
    let equal x y = try H.equal (top_key x) (top_key y) with Not_found -> false
  end
  module W = Weak.Make(HX)
  type key = H.t and 'a t = W.t
  let create = W.create and clear = W.clear
  let find_all tbl key = 
    try all_value (W.find tbl (dummy key)) with Not_found-> []
  let find tbl key = top_value (W.find tbl (dummy key))
  let add tbl key data =
    let bd = bind_new key data in
    let cls = 
      try let c = W.find tbl (dummy key) in Stack.push bd c; c
      with Not_found -> let c = cls_new bd in W.add tbl c; c in
    let final _ = ignore bd; ignore cls in
    try Gc.finalise final key 
    with Invalid_argument _ -> Gc.finalise final bd; Gc.finalise final cls
  let remove tbl key = 
    try ignore (Stack.pop (W.find tbl (dummy key))) with Not_found -> ()
  let replace tbl key data = remove tbl key; add tbl key data
  let mem tbl key = try ignore (find tbl key); true with Not_found -> false
  let iter f tbl = 
    let f' (bk,v) = match unbox bk with Some k -> f k (obj v) | None -> () in
    W.iter (Stack.iter f') tbl
  let fold f tbl accu = 
    let r = ref accu in
    let f' k v = r := f k v !r in
    iter f' tbl; !r
  let length tbl = W.fold (fun cls -> (+) (Stack.length cls)) tbl 0
  let copy tbl = 
    let tbl'= W.create (W.count tbl * 3 / 2 + 2) in 
    W.iter (fun cls -> W.add tbl' (Stack.copy cls)) tbl; tbl'
  let stats _ = assert false
  let reset _ = assert false
end

module StdHash = Make
  (struct 
     type t = Obj.t let equal x y = (compare x y) = 0 let hash = Hashtbl.hash
  end)
open StdHash
type ('a,'b) t = 'b StdHash.t
let create = create and clear = clear and copy = copy and length = length
let add tbl k = add tbl (repr k)
let remove tbl k = remove tbl (repr k)
let find tbl k = find tbl (repr k)
let find_all tbl k = find_all tbl (repr k)
let replace tbl k = replace tbl (repr k)
let mem tbl k = mem tbl (repr k)
let iter f = iter (fun k d -> f (obj k) d)
let fold f = fold (fun k d a -> f (obj k) d a)
