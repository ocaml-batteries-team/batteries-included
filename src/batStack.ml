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

let of_enum e =
  let s = create () in
  BatEnum.iter (fun x -> push x s) e;
  s

(*$T of_enum
  let s = create () in push 3 s; push 5 s; [3;5] |> List.enum |> of_enum = s
  let s = create () in of_enum (BatEnum.empty ()) = s
*)

(* Consumes input stack *)
let enum_destruct s =
  let get () = try pop s with Stack.Empty -> raise BatEnum.No_more_elements in
  BatEnum.from get

(*$T enum_destruct
  let s = of_enum (List.enum [2;4;6;8]) in \
   enum_destruct s |> List.of_enum = [8;6;4;2] && is_empty s
*)

(* consumes a copy *)
let enum s = enum_destruct (copy s)

let print ?(first="") ?(last="") ?(sep="") print_a out t =
  BatEnum.print ~first ~last ~sep print_a out (enum t)

(*$T print
  IO.to_string (print Int.print) (of_enum (List.enum [2;4;6;8])) = "8642"
*)

let compare cmp a b = BatEnum.compare cmp (enum a) (enum b)
let equal eq a b = BatEnum.equal eq (enum a) (enum b)

(*$T equal
  not (equal Int.equal (create()) (of_enum (List.enum [2])))
  equal Int.equal (create()) (create())
  equal Int.equal (of_enum (List.enum [2])) (of_enum (List.enum [2]))
*)

(*$T compare
  0 <> (compare Int.compare (create()) (of_enum (List.enum [2])))
*)

module Exceptionless = struct
  let top s = try Some (top s) with Empty -> None
  let pop s = try Some (pop s) with Empty -> None
end
