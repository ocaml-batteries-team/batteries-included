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


include Stack

type 'a enumerable = 'a Stack.t

let of_gen e =
  let s = create () in
  BatGen.iter (fun x -> push x s) e;
  s

(*$T of_gen
  let s = create () in push 3 s; push 5 s; [3;5] |> List.gen |> of_gen = s
  let s = create () in of_gen (BatGen.empty ()) = s
*)

(* Consumes input stack *)
let gen_destruct s =
  fun () ->
    try Some (pop s)
    with Stack.Empty -> None

(*$T gen_destruct
  let s = of_gen (List.gen [2;4;6;8]) in \
   gen_destruct s |> List.of_gen = [8;6;4;2] && is_empty s
*)

(* consumes a copy *)
let gen s = gen_destruct (copy s)

let print ?(first="") ?(last="") ?(sep="") print_a out t =
  BatGen.print ~first ~last ~sep print_a out (gen t)

(*$T print
  IO.to_string (print Int.print) (of_gen (List.gen [2;4;6;8])) = "8642"
*)

let compare cmp a b = BatGen.compare ~cmp (gen a) (gen b)
let equal eq a b = BatGen.eq ~eq (gen a) (gen b)

(*$T equal
  not (equal Int.equal (create()) (of_gen (List.gen [2])))
  equal Int.equal (create()) (create())
  equal Int.equal (of_gen (List.gen [2])) (of_gen (List.gen [2]))
*)

(*$T compare
  0 <> (compare Int.compare (create()) (of_gen (List.gen [2])))
*)

module Exceptionless = struct
  let top s = try Some (top s) with Empty -> None
  let pop s = try Some (pop s) with Empty -> None
end
