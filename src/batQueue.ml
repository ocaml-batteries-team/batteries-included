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


module Concrete = struct
(* Explanation from OCaml 4.02 source:

   A queue is a reference to either nothing or some cell of a cyclic
   list. By convention, that cell is to be viewed as the last cell in
   the queue. The first cell in the queue is then found in constant
   time: it is the next cell in the cyclic list. The queue's length is
   also recorded, so as to make [length] a constant-time operation.

   The [tail] field should really be of type ['a cell option], but
   then it would be [None] when [length] is 0 and [Some] otherwise,
   leading to redundant memory allocation and accesses. We avoid this
   overhead by filling [tail] with a dummy value when [length] is 0.
   Of course, this requires bending the type system's arm slightly,
   because it does not have dependent sums.
   The dummy value used by the stdlib is (Obj.magic None). *)

  type 'a cell = {
    content: 'a;
    mutable next: 'a cell
  }
  and 'a t = {
    mutable length: int;
    mutable tail: 'a cell
  }

  external of_abstr : 'a Queue.t -> 'a t = "%identity"
  external to_abstr : 'a t -> 'a Queue.t = "%identity"

  let map f {tail;length} =
    let q = Queue.create () in
    let rec loop ({ content; next } as current) =
      Queue.add (f content) q;
      if current != tail then loop next
    in
    if length > 0 then loop tail.next;
    q

  let filter f {tail;length} =
    let q = Queue.create () in
    let rec loop ({ content; next } as current) =
      if f content then Queue.add content q;
      if current != tail then loop next
    in
    if length > 0 then loop tail.next;
    q

  let filter_map f {tail;length} =
    let q = Queue.create () in
    let rec loop ({ content; next } as current) =
      begin match f content with
      | None -> ()
      | Some elem ->
	 Queue.add elem q;
      end;
      if current != tail then loop next
    in
    if length > 0 then loop tail.next;
    q

  let filter_inplace f ({tail} as queue) =
    if not (Queue.is_empty (to_abstr queue)) then
      let rec filter'
          ({ next = { content; next} as current } as prev)
        =
        if f content
        then
          (* Keep cell. Recursion to next cell unless we reached the tail *)
          (if current != tail then filter' current)
        else begin
          (* Remove cell. *)
          if current != tail
          then begin
            (* Easy case. We are not removing the tail cell. *)
            prev.next <- next;
            queue.length <- queue.length - 1;
            (* Recursion with the same cell,
             * because it is now pointing beyond current. *)
            filter' prev
          end
          else begin
            (* Removing the tail cell *)
            if prev == current
            (* Tail cell is the last cell. Just clear the queue. *)
            then begin
              Queue.clear (to_abstr queue)
            end
            else begin
              (* Tail cell is not the last cell.
               * prev is the new tail. *)
              prev.next <- next;
              queue.length <- queue.length - 1;
              queue.tail <- prev;
            end
          end
        end
      in
      filter' tail

end

include Queue

type 'a enumerable = 'a t

let map f q = Concrete.(map f (of_abstr q))
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

let filter f q = Concrete.(filter f (of_abstr q))
(*$T filter
  create () |> filter (fun n -> n>3) |> is_empty
  create () |> tap (add 1) |> filter (fun n -> n>3) |> is_empty
  create () |> tap (add 1) |> tap (add 2) |> filter (fun n -> n>3) |> is_empty
  create () |> tap (add 1) |> tap (add 2) |> filter (fun n -> n>1) |> enum |> BatList.of_enum |> (=) [2]
  create () |> tap (add 1) |> tap (add 2) |> filter (fun n -> n>0) |> enum |> BatList.of_enum |> (=) [1;2]
 *)

let filter_map f q = Concrete.(filter_map f (of_abstr q))
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

let filter_inplace f q = Concrete.(filter_inplace f (of_abstr q))
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
