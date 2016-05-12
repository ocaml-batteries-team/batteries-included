(*
 * BatQueue - Extended operations on queues
 * Copyright (C) 1996 Xavier Leroy
 *               2008 David Teller, LIFO, Universite d'Orleans
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

include Queue

type 'a enumerable = 'a t

let map f q =
  let r = create () in
  iter (fun x -> add (f x) r) q;
  r
(*$T map
  create () |> map (fun x -> x) |> is_empty

  create () |> tap (add 1) |> map (fun x -> x+1) \
  |> enum |> BatList.of_enum |> (=) [2]

  create () |> tap (add 1) |> tap (add 2) |> map (fun x -> x+1) \
  |> enum |> BatList.of_enum |> (=) [2;3]

  let q = Queue.create () in \
  for i = 1 to 5 do Queue.push i q; done; \
  let q = map ((+) 10) q in \
  BatList.of_enum (enum q) = [11;12;13;14;15]
*)

let filter f q =
  let r = create () in
  iter (fun x -> if f x then add x r) q;
  r
(*$T filter
  create () |> filter (fun n -> n>3) |> is_empty
  create () |> tap (add 1) |> filter (fun n -> n>3) |> is_empty
  create () |> tap (add 1) |> tap (add 2) |> filter (fun n -> n>3) |> is_empty
  create () |> tap (add 1) |> tap (add 2) |> filter (fun n -> n>1) |> enum |> BatList.of_enum |> (=) [2]
  create () |> tap (add 1) |> tap (add 2) |> filter (fun n -> n>0) |> enum |> BatList.of_enum |> (=) [1;2]
 *)

let filter_map f q =
  let r = create () in
  iter (fun x ->
        match f x with
          | None -> ()
          | Some v -> add v r) q;
  r
(*$T filter_map
  create () |> filter_map (fun n -> None) |> is_empty

  create () |> tap (add 1) \
  |> filter_map (fun n -> if n>3 then Some (n+1) else None) |> is_empty

  create () |> tap (add 1) |> tap (add 2) \
  |> filter_map (fun n -> if n>3 then Some (n+1) else None) |> is_empty

  create () |> tap (add 1) |> tap (add 2) \
  |> filter_map (fun n -> if n>1 then Some (n+1) else None) |> enum \
  |> BatList.of_enum |> (=) [3]

  create () |> tap (add 1) |> tap (add 2) \
  |> filter_map (fun n -> if n>0 then Some (n+1) else None) |> enum \
  |> BatList.of_enum |> (=) [2;3]
 *)

let filter_inplace f q =
  BatConcreteQueue.(filter_inplace f (of_abstr q))
(*$T filter_inplace
  let q1 = Queue.create () in \
  for i = 1 to 5 do Queue.push i q1; done; \
  let q2,q3 = Queue.copy q1, Queue.copy q1 in \
  filter_inplace (fun a -> List.mem a [2;4]) q1; \
  filter_inplace (fun a -> List.mem a [3]) q2; \
  filter_inplace (fun a -> List.mem a []) q3; \
  length q1 = 2 && \
  length q2 = 1 && \
  length q3 = 0 && \
  BatList.of_enum (enum q1) = [2;4] && \
  BatList.of_enum (enum q2) = [3] && \
  BatList.of_enum (enum q3) = []
*)

let of_enum e =
  let q = create () in
  BatEnum.iter (fun x -> push x q) e;
  q
(*$Q of_enum
  (Q.list Q.int) (fun l -> \
    let e = BatList.enum l in \
    BatEnum.equal (=) (enum (of_enum (BatEnum.clone e))) e \
  )
*)

let enum q = BatEnum.from (fun () -> try pop q with Empty -> raise BatEnum.No_more_elements)
(*$T enum
  let q = Queue.create () in \
  for i = 0 to 10 do Queue.push i q; done; \
  let e = enum q in \
  let i = ref (-1) in \
  BatEnum.count e = 11 && BatEnum.for_all (fun elt -> incr i; !i = elt) e
*)

let print ?(first="") ?(last="") ?(sep="") print_a out t =
  BatEnum.print ~first ~last ~sep print_a out (enum (copy t))
(*$T print
  BatIO.to_string (print ~sep:"," ~first:"[" ~last:"]" BatInt.print) (of_enum (BatArray.enum [|2;4;66|])) = "[2,4,66]"
*)

let compare cmp a b = BatEnum.compare cmp (enum a) (enum b)
let equal eq a b = BatEnum.equal eq (enum a) (enum b)

module Exceptionless = struct
  let peek q = try Some (peek q) with Empty -> None
  let take q = try Some (take q) with Empty -> None
  (*$T
    Exceptionless.peek (Queue.create ()) = None
    Exceptionless.take (Queue.create ()) = None
  *)
end
