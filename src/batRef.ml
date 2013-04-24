(*
 * Ref - Operations on references
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


type 'a t = 'a ref

let post r f =
  let old = !r in
  r := f old;
  old

let pre r f =
  r := f !r;
  !r

let swap a b =
  let buf = !a in
  a := !b;
  b := buf

(*$T swap
  let a = ref 1 and b = ref 2 in swap a b; !a = 2 && !b = 1
*)

let pre_incr  r = pre  r ( ( + ) 1 )
let pre_decr  r = pre  r ( ( + ) (-1) )
let post_incr r = post r ( ( + ) 1 )
let post_decr r = post r ( ( + ) (-1) )

(*$T pre_incr
  let r = ref 0 in pre_incr r = 1 && !r = 1
*)
(*$T post_incr
  let r = ref 0 in post_incr r = 0 && !r = 1
*)

let copy r = ref (!r)

(*$T copy
  let r = ref 0 in let s = copy r in r := 1; !s == 0 && !r == 1
*)

let protect r v body =
  let old = !r in
  try
    r := v;
    let res = body() in
    r := old;
    res
  with x ->
    r := old;
    raise x

(*$T protect
  let r = ref 0 in let b () = incr r; !r in protect r 2 b = 3 && !r = 0
  let r = ref 0 in let b () = incr r; if !r=3 then raise Not_found in (try protect r 2 b; false with Not_found -> true) && !r = 0
*)


external ref : 'a -> 'a ref = "%makemutable"
(** Return a fresh reference containing the given value. *)

external ( ! ) : 'a ref -> 'a = "%field0"
(** [!r] returns the current contents of reference [r].
    Equivalent to [fun r -> r.contents]. *)

external ( := ) : 'a ref -> 'a -> unit = "%setfield0"
(** [r := a] stores the value of [a] in reference [r].
    Equivalent to [fun r v -> r.contents <- v]. *)

external set : 'a ref -> 'a -> unit = "%setfield0"
(** As [ := ] *)

external get : 'a ref -> 'a = "%field0"
(** As [ ! ]*)

let print print_a out r = print_a out !r

let toggle r = r := not !r

(*$T toggle
  let r = ref true in toggle r; !r = false;
  let r = ref false in toggle r; !r = true;
*)

let oset r x = r := Some x

let oget_exn r = match !r with None -> raise Not_found | Some x -> x

(*  FAIL $T oset, oget_exn
    let r = ref None in oset r 3; oget_exn r = 3
*)

let compare c x y = c !x !y
(*$T compare
  let a = ref 1 and b = ref 2 in compare Int.compare a b < 0
*)

let ord o x y = o !x !y
(*$T ord
  let a = ref 1 and b = ref 2 in ord Int.ord a b = BatOrd.Lt
*)

let eq e x y = e !x !y
  (*$T eq
    let a = ref 1 and b = ref 2 in eq Int.equal a b = false
    let a = ref 1 and b = ref 1 in eq Int.equal a b = true
  *)
