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


type 'a t = 'a option ref

exception Return

let return label value =
  label := Some value;
  raise Return (*(Obj.repr label)*)

let label f =
  let r = ref None in
    try   f r
    with  Return when !r <> None -> (*[!r = None] may happen if the user has let the exception escape its scope *)
      match !r with                (*in that case, we wish the exception to fall-through for debugging purposes*)
	| None   -> assert false (*Should be impossible*)
	| Some x -> 
	    r := None;             (*Reset the trap for sanity checks should another exception escape scope    *)
	    x                      (*(not that this should be possible in that case -- let's just be careful)  *)
let with_label = label
