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
