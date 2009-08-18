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

let pre_incr  r = pre  r ( ( + ) 1 )
let pre_decr  r = pre  r ( ( + ) (-1) )
let post_incr r = post r ( ( + ) 1 )
let post_decr r = post r ( ( + ) (-1) )

let copy r = ref (!r)


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

let print print_a out x = print_a out !x
