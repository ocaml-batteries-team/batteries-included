(*
 * Return -- fast return in OCaml
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


type 'a t = 'a -> exn

let return label value =
  raise (label value)

let label (type u) (f : u t -> u) : u =
  let module M = struct exception Return of u end in
  try f (fun x -> M.Return x)
  with M.Return u -> u
let with_label = label

(* testing nesting with_labels *)
(*$T with_label
  with_label (fun label1 -> \
    with_label (fun _label2 -> ignore (return label1 1)); 2 \
  ) = 1
*)
