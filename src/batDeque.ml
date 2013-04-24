(*
 * Deque -- functional double-ended queues
 * Copyright (C) 2011  Batteries Included Development Team
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

type 'a dq = { front : 'a list ; flen : int ;
               rear : 'a list  ; rlen : int }

let invariants t =
  assert (List.length t.front = t.flen);
  assert (List.length t.rear = t.rlen)

type 'a t = 'a dq
type 'a enumerable = 'a t
type 'a mappable = 'a t

let empty = { front = [ ] ; flen = 0 ;
              rear  = [ ] ; rlen = 0 }

let size q =
  q.flen + q.rlen

let cons x q =
  { q with front = x :: q.front ; flen = q.flen + 1 }

(*$T cons
   size (cons 1 empty) = 1
   to_list(cons 1 empty) <> to_list(cons 2 empty)
*)

(*$Q cons
  (Q.list Q.pos_int) ~count:10 \
    (fun l -> List.fold_left (fun q x -> cons x q) empty l |> to_list = List.rev l)
*)

let snoc q x =
  { q with rear = x :: q.rear ; rlen = q.rlen + 1 }

(*$T cons; snoc
   to_list(cons 1 empty) = to_list(snoc empty 1)
   to_list(cons 1 (cons 2 empty)) = (to_list (snoc (snoc empty 2) 1) |> List.rev)
*)

(*$Q snoc
   (Q.list Q.int) (fun l -> List.fold_left snoc empty l |> to_list = l)
*)

let front q =
  match q with
  | {front = h :: front; flen = flen; _} ->
    Some (h, { q with front = front ; flen = flen - 1 })
  | {rear = []; _} ->
    None
  | {rear = rear; rlen = rlen; _} ->
    (* beware: when rlen = 1, we must put the only element of
     * the deque at the front (ie new_flen = 1, new_rlen = 0) *)
    let new_flen = (rlen + 1) / 2 in
    let new_rlen = rlen / 2 in
    (* we split the non empty list in half because if we transfer
     * everything to the front, then a call to rear would also
     * transfer everything to the rear etc. -> no amortization
     * (but we could transfer 3/4 instead of 1/2 of the list for instance) *)
    let rear, rev_front = BatList.split_at new_rlen rear in
    let front = List.rev rev_front in
    Some (List.hd front, { front = List.tl front ;
                           flen = new_flen - 1 ;
                           rear = rear ;
                           rlen = new_rlen })

(*$T front
   front(cons 1 empty) = Some(1,empty)
   front(snoc empty 1) = Some(1,empty)
*)

let rear q =
  match q with
  | {rear = t :: rear; rlen = rlen; _} ->
    Some ({ q with rear = rear ; rlen = rlen - 1 }, t)
  | {front = []; _} ->
    None
  | {front = front; flen = flen; _} ->
    let new_rlen = (flen + 1) / 2 in
    let new_flen = flen / 2 in
    let front, rev_rear = BatList.split_at new_flen front in
    let rear = List.rev rev_rear in
    Some ({ front = front ; flen = new_flen ;
            rear = List.tl rear ; rlen = new_rlen - 1 },
      List.hd rear)

(*$T rear
   match rear(empty |> cons 1 |> cons 2) with | Some(_, 1) -> true | _ -> false
*)

let rev q = { front = q.rear ; flen = q.rlen ;
              rear = q.front ; rlen = q.flen }

(*$Q rev
   (Q.list Q.pos_int) (fun l -> let q = of_list l in rev q |> to_list = List.rev l)
*)

let of_list l = { front = l ; flen = List.length l ;
                  rear = [] ; rlen = 0 }

let is_empty q = size q = 0

let append q r =
  if size q > size r then
    { q with
      rlen = q.rlen + size r ;
      rear = BatList.append r.rear (List.rev_append r.front q.rear) }
  else
    { r with
      flen = r.flen + size q ;
      front = BatList.append q.front (List.rev_append q.rear r.front) }

let append_list q l =
  let n = List.length l in
  { q with
    rear = List.rev_append l q.rear;
    rlen = q.rlen + n }

let prepend_list l q =
  let n = List.length l in
  { q with
    front = BatList.append l q.front ;
    flen = q.flen + n }

let at ?(backwards=false) q n =
  let size_front = q.flen in
  let size_rear = q.rlen in
  if n < 0 || n >= size_rear + size_front then
    None
  else
    Some (
      if backwards then
        if n < size_rear then BatList.at q.rear n
        else BatList.at q.front (size_front - 1 - (n - size_rear))
      else
      if n < size_front then BatList.at q.front n
      else BatList.at q.rear (size_rear - 1 - (n - size_front))
    )

let map f q =
  let rec go q r = match front q with
    | None -> r
    | Some (x, q) ->
      go q (snoc r (f x))
  in
  go q empty

let mapi f q =
  let rec go n q r = match front q with
    | None -> r
    | Some (x, q) ->
      go (n + 1) q (snoc r (f n x))
  in
  go 0 q empty

let iter f q =
  let rec go q = match front q with
    | None -> ()
    | Some (x, q) ->
      f x ; go q
  in
  go q

let iteri f q =
  let rec go n q = match front q with
    | None -> ()
    | Some (x, q) ->
      f n x ;
      go (n + 1) q
  in
  go 0 q

let rec fold_left fn acc q = match front q with
  | None -> acc
  | Some (f, q) -> fold_left fn (fn acc f) q

let rec fold_right fn q acc = match rear q with
  | None -> acc
  | Some (q, r) -> fold_right fn q (fn r acc)

let to_list q =
  BatList.append q.front (BatList.rev q.rear)

let find ?(backwards=false) test q =
  let rec spin k f r = match f with
    | [] -> begin match r with
        | [] -> None
        | _ -> spin k (List.rev r) []
      end
    | x :: f ->
      if test x then Some (k, x)
      else spin (k + 1) f r
  in
  if backwards then
    spin 0 q.rear q.front
  else
    spin 0 q.front q.rear

let rec enum q =
  let cur = ref q in
  let next () = match front !cur with
    | None -> raise BatEnum.No_more_elements
    | Some (x, q) ->
      cur := q ; x
  in
  let count () = size !cur in
  let clone () = enum !cur in
  BatEnum.make ~next ~count ~clone

let of_enum e =
  BatEnum.fold snoc empty e

(*$Q enum
   (Q.list Q.int) (fun l -> List.of_enum (enum (List.fold_left snoc empty l)) = l)
*)(*$Q of_enum
    (Q.list Q.int) (fun l -> to_list (of_enum (List.enum l)) = l)
  *)

let print ?(first="[") ?(last="]") ?(sep="; ") elepr out dq =
  let rec spin dq = match front dq with
    | None -> ()
    | Some (a, dq) when size dq = 0 ->
      elepr out a
    | Some (a, dq) ->
      elepr out a ;
      BatInnerIO.nwrite out sep ;
      spin dq
  in
  BatInnerIO.nwrite out first ;
  spin dq ;
  BatInnerIO.nwrite out last

  (*$Q print
    (Q.list Q.int) (fun l -> \
      BatIO.to_string (print ~first:"<" ~last:">" ~sep:"," Int.print) (of_list l) \
      = BatIO.to_string (List.print ~first:"<" ~last:">" ~sep:"," Int.print) l)
  *)
