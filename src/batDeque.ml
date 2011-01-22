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

type 'a t = 'a dq

let empty = { front = [ ] ; flen = 0 ;
              rear  = [ ] ; rlen = 0 }

let size q =
  q.flen + q.rlen

let cons x q =
  { q with front = x :: q.front ; flen = q.flen + 1 }

let snoc q x =
  { q with rear = x :: q.rear ; rlen = q.rlen + 1 }

let front q =
  match q.front with
    | h :: front -> Some (h, { q with front = front ; flen = q.flen - 1 })
    | _ ->
        match q.rear with
          | [] -> None
          | _ ->
              let front = List.rev q.rear in
              Some (List.hd front, { front = List.tl front ;
                                     flen = q.rlen - 1 ;
                                     rear = [] ;
                                     rlen = 0 })

let rear q =
  match q.rear with
    | t :: rear -> Some ({ q with rear = rear ; rlen = q.rlen - 1 }, t)
    | _ ->
        match q.front with
          | [] -> None
          | _ ->
              let rear = List.rev q.front in
              Some ({ front = [] ; flen = 0 ;
                      rear = List.tl rear ; rlen = q.flen - 1 },
                    List.hd rear)

let rev q = { front = q.rear ; flen = q.rlen ;
              rear = q.front ; rlen = q.flen }

let of_list l = { front = l ; flen = List.length l ;
                  rear = [] ; rlen = 0 }

let is_empty q = size q = 0

let append_rl q r =
  let rec spin rear = function
    | [] -> r.rear @ rear
    | x :: rfront ->
        spin (x :: rear) rfront
  in { q with
         rlen = q.rlen + size r ;
         rear = spin q.rear r.front }

let append_lr q r =
  let rec spin front = function
    | [] -> q.front @ front
    | x :: qrear ->
        spin (x :: front) qrear
  in { r with
         flen = r.flen + size q ;
         front = spin r.front q.rear }

let append q r =
  if size q > size r then
    append_rl q r
  else
    append_lr q r

let append_list q l =
  let n = List.length l in
  let rec spin rear = function
    | [] -> rear
    | x :: l -> spin (x :: rear) l
  in { q with
         rear = spin q.rear l ;
         rlen = q.rlen + n }

let prepend_list l q =
  let n = List.length l in
  { q with
      front = l @ q.front ;
      flen = q.flen + n }

let rec nth ?(backwards=false) q n =
  if backwards then nth (rev q) n
  else if n >= size q then None
  else
    let rec git q n =
      match front q with
        | Some (x, q) ->
            if n = 0 then Some x else git q (n - 1)
        | None ->
            failwith "Queue.nth: internal error"
    in
    git q n

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
  let rec go l q = match rear q with
    | None -> l
    | Some (q, x) ->
        go (x :: l) q
  in
  go [] q

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

