(*
 * Dllist- a mutable, circular, doubly linked list library
 * Copyright (C) 2004 Brian Hurt, Jesse Guardiani
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


type 'a node_t = {
  mutable data : 'a;
  mutable next : 'a node_t;
  mutable prev : 'a node_t
}

type 'a enum_t = {
  mutable curr : 'a node_t;
  mutable valid : bool
}

type 'a t = 'a node_t

type 'a mappable = 'a t
type 'a enumerable = 'a t

exception Empty

let invariants t =
  assert (t.next.prev == t && t.prev.next == t);
  let current = ref t.next in
  while !current != t do
    let t = !current in
    assert (t.next.prev == t && t.prev.next == t);
    current := t.next
  done

let create x = let rec nn = { data = x; next = nn; prev = nn} in nn

let length node =
  let rec loop cnt n =
    if n == node then
      cnt
    else
      loop (cnt + 1) n.next
  in
  loop 1 node.next

let add node elem =
  let nn = { data = elem; next = node.next; prev = node } in
  node.next.prev <- nn;
  node.next <- nn
(*$T add
  let t = of_list [1;2;3] in add t 12; invariants t; to_list t = [1;12;2;3]
  let t = of_list [1] in add t 2; invariants t; to_list t = [1;2]
*)

let append node elem =
  let nn = { data = elem; next = node.next; prev = node } in
  node.next.prev <- nn;
  node.next <- nn;
  nn

let prepend node elem =
  let nn = { data = elem; next = node; prev = node.prev } in
  node.prev.next <- nn;
  node.prev <- nn;
  nn

let promote node =
  let next = node.next in
  let prev = node.prev in
  if next != prev then begin
    next.next.prev <- node;
    node.next <- next.next;
    node.prev <- next;
    next.next <- node;
    next.prev <- prev;
    prev.next <- next
  end
(*$T promote
  let t = of_list [1;2;3;4] in promote t; invariants t; to_list t = [1;3;4;2]
  let t = of_list [1] in promote t; invariants t; to_list t = [1]
*)

let demote node =
  let next = node.next in
  let prev = node.prev in
  if next != prev then begin
    prev.prev.next <- node;
    node.prev <- prev.prev;
    node.next <- prev;
    prev.prev <- node;
    prev.next <- next;
    next.prev <- prev
  end
(*$T demote
  let t = of_list [1;2;3;4] in demote t; invariants t; to_list t = [1;4;2;3]
  let t = of_list [1] in demote t; invariants t; to_list t = [1]
*)

let remove node =
  let next = node.next in
  if next == node then raise Empty; (* singleton list points to itself for next *)
  let prev = node.prev in
  (* Remove node from list by linking prev and next together *)
  prev.next <- next;
  next.prev <- prev;
  (* Make node a singleton list by setting its next and prev to itself *)
  node.next <- node;
  node.prev <- node
(*$T remove
  let t = of_list [1;2;3;4] in let u = next t in remove t; invariants u; to_list u = [2;3;4]
  let t = of_list [1;2] in let u = next t in remove t; invariants u; to_list u = [2]
  let t = of_list [1;2] in let u = next t in remove t; try remove u; false with Empty -> true
  let t = of_list [1] in try remove t; false with Empty -> true
*)

let drop node =
  let next = node.next in
  if next == node then raise Empty; (* singleton list points to itself for next *)
  let prev = node.prev in
  prev.next <- next;
  next.prev <- prev;
  node.next <- node;
  node.prev <- node;
  next
(*$T drop
  let t = of_list [1;2;3;4] in let t = drop t in invariants t; to_list t = [2;3;4]
  let t = of_list [1] in try ignore (drop t); false with Empty -> true
*)

let rev_drop node =
  let next = node.next in
  if next == node then raise Empty;  (* singleton list points to itself for next *)
  let prev = node.prev in
  prev.next <- next;
  next.prev <- prev;
  node.next <- node;
  node.prev <- node;
  prev
(*$T rev_drop
  let t = of_list [1;2;3;4] in let t = rev_drop t in invariants t; to_list t = [4;2;3]
  let t = of_list [1] in try ignore (rev_drop t); false with Empty -> true
*)

let splice node1 node2 =
  let next = node1.next in
  let prev = node2.prev in
  node1.next <- node2;
  node2.prev <- node1;
  next.prev <- prev;
  prev.next <- next

let set node data = node.data <- data (*BISECT-VISIT*)

let get node = node.data

let next node = node.next

let prev node = node.prev

let skip node idx =
  let m = if idx > 0 then -1 else 1 in
  let rec loop idx n =
    if idx == 0 then
      n
    else
      loop (idx + m) n.next
  in
  loop idx node

let rev node =
  let rec loop next n =
    begin
      let prev = n.prev in
      n.next <- prev;
      n.prev <- next;

      if n != node then
        loop n prev
    end
  in
  loop node node.prev
(*$T rev
  let t = of_list [1] in rev t; invariants t; to_list t = [1]
  let t = of_list [1;2;3;4] in rev t; invariants t; to_list t = [1;4;3;2]
*)

let iter f node =
  let () = f node.data in
  let rec loop n =
    if n != node then
      let () = f n.data in
      loop n.next
  in
  loop node.next

let for_all p node =
  let rec loop n =
    if n == node then true
    else p n.data && loop n.next
  in
  p node.data && loop node.next

let find p node =
  let rec loop n =
    if n == node then raise Not_found
    else if p n.data then n else loop n.next
  in
  if p node.data then node else loop node.next

(*$T find
   find (fun x -> x mod 2 = 0) (of_list [1;3;4;5;7;6]) |> get = 4
   find (fun x -> x = 1) (of_list [1;3;4;5;7;6]) |> get = 1
   find (fun x -> x > 3) (of_list [-1;3;9;1;1;1]) |> get = 9
   try find (fun x -> x land 3 = 2) (of_list [1;4;3])|>ignore; false with Not_found -> true
*)
(*qtest TODO: migrate try into an exception test *)

let exists p node =
  let rec loop n =
    if n == node then false
    else p n.data || loop n.next
  in
  p node.data || loop node.next


let fold_left f init node =
  let rec loop accu n =
    if n == node then
      accu
    else
      loop (f accu n.data) n.next
  in
  loop (f init node.data) node.next

let fold_right f node init =
  let rec loop accu n =
    if n == node then
      f n.data accu
    else
      loop (f n.data accu) n.prev
  in
  loop init node.prev

let map f node =
  let first = create (f node.data) in
  let rec loop last n =
    if n == node then
      begin
        first.prev <- last;
        first
      end
    else
      begin
        let nn = { data = f n.data; next = first; prev = last } in
        last.next <- nn;
        loop nn n.next
      end
  in
  loop first node.next

let copy node = map (fun x -> x) node

let to_list node = fold_right (fun d l -> d::l) node []

let of_list lst =
  match lst with
  | [] -> raise Empty
  | h :: t ->
    let first = create h in
    let rec loop last = function
      | [] ->
        last.next <- first;
        first.prev <- last;
        first
      | h :: t ->
        let nn = { data = h; next = first; prev = last } in
        last.next <- nn;
        loop nn t
    in
    loop first t
(*$T
  try ignore (of_list []); false with Empty -> true
*)

let enum node =
  let next e () =
    if not e.valid then
      raise BatEnum.No_more_elements
    else
      begin
        let rval = e.curr.data in
        e.curr <- e.curr.next;

        if (e.curr == node) then
          e.valid <- false;
        rval
      end
  and count e () =
    if not e.valid then
      0
    else
      let rec loop cnt n =
        if n == node then
          cnt
        else
          loop (cnt + 1) (n.next)
      in
      loop 1 (e.curr.next)
  in
  let rec clone e () =
    let e' = { curr = e.curr; valid = e.valid } in
    BatEnum.make ~next:(next e') ~count:(count e') ~clone:(clone e')
  in
  let e = { curr = node; valid = true } in
  BatEnum.make ~next:(next e) ~count:(count e) ~clone:(clone e)

let rev_enum node =
  let prev e () =
    if not e.valid then
      raise BatEnum.No_more_elements
    else
      begin
        let rval = e.curr.data in
        e.curr <- e.curr.prev;

        if (e.curr == node) then
          e.valid <- false;
        rval
      end
  and count e () =
    if not e.valid then
      0
    else
      let rec loop cnt n =
        if n == node then
          cnt
        else
          loop (cnt + 1) (n.prev)
      in
      loop 1 (e.curr.prev)
  in
  let rec clone e () =
    let e' = { curr = e.curr; valid = e.valid } in
    BatEnum.make ~next:(prev e') ~count:(count e') ~clone:(clone e')
  in
  let e = { curr = node; valid = true } in
  BatEnum.make ~next:(prev e) ~count:(count e) ~clone:(clone e)

let backwards t =
  rev_enum (prev t)

let of_enum enm =
  match BatEnum.get enm with
  | None -> raise Empty
  | Some(d) ->
    let first = create d in
    let f n d = append n d in
    ignore(BatEnum.fold f first enm);
    first

let print ?(first="[") ?(last="]") ?(sep="; ") print_a out t =
  BatEnum.print ~first ~last ~sep print_a out (enum t)

let filter f node = (*TODO : make faster*)
  of_enum (BatEnum.filter f (enum node))

let filter_map f node = (*TODO : make faster*)
  of_enum (BatEnum.filter_map f (enum node))
