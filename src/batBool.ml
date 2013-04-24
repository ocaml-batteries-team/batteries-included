(*
 * BatBool - Extended booleans
 * Copyright (C) 2007 Bluestorm <bluestorm dot dylc on-the-server gmail dot com>
 *               2008 David Teller
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


open BatNumber

module BaseBool = struct
  type t = bool
  external not : bool -> bool = "%boolnot"
  (** The boolean negation. *)

  external ( && ) : bool -> bool -> bool = "%sequand"
  (** The boolean ``and''. Evaluation is sequential, left-to-right:
	  in [e1 && e2], [e1] is evaluated first, and if it returns [false],
	  [e2] is not evaluated at all. *)

  external ( || ) : bool -> bool -> bool = "%sequor"
  (** The boolean ``or''. Evaluation is sequential, left-to-right:
	  in [e1 || e2], [e1] is evaluated first, and if it returns [true],
	  [e2] is not evaluated at all. *)
  let zero, one = false, true
  let neg = not

  let succ _ = true
  let pred _ = false
  let abs  x = x

  let add    = ( || )
  let mul    = ( && )
  let sub _  = not (*Weird extrapolation*)
  (*BISECT-IGNORE-BEGIN*)
  let div _ _=
    raise (Invalid_argument "Bool.div")

  let modulo _ _ =
    raise (Invalid_argument "Bool.modulo")

  let pow _ _ =
    raise (Invalid_argument "Bool.pow")
  (*BISECT-IGNORE-END*)

  let compare = compare

  let equal = (=)

  let ord = BatOrd.ord compare

  let of_int = function
    | 0 -> false
    | _ -> true

  let to_int = function
    | false -> 0
    | true  -> 1

  let of_float x = of_int (int_of_float x)
  let to_float x = float_of_int (to_int x)
  let of_string = function
    | "true" | "tt" | "1" -> true
    | "false"| "ff" | "0" -> false
    | _                   -> raise (Invalid_argument "Bool.of_string")

  let to_string = string_of_bool
end

include BatNumber.MakeNumeric(BaseBool)

(*$T succ
  succ true = true
  succ false = true
*)
(*$T pred
  pred true = false
  pred false = false
*)
(*$T abs
  abs true = true
  abs false = false
*)
(*$T sub
  sub true  true  = false
  sub true  false = true
  sub false true  = false
  sub false false = true
*)
(*$Q of_int
  (Q.int) (fun i -> (of_int i) = (Int.(<>) i 0))
*)
(*$T of_int
  of_int 0 = false
*)
(*$T
  of_float (-1.) = true
  of_float 0. = false
  of_float (1. /. 0.) = false
  of_float (-1. /. 0.) = false
  of_float nan = false
  to_float true = 1.
  to_float false = 0.
  of_string "true" = true
  of_string "false" = false
  try ignore (of_string "smurf"); false with Invalid_argument _ -> true
*)


external not : bool -> bool = "%boolnot"
external ( && ) : bool -> bool -> bool = "%sequand"
external ( || ) : bool -> bool -> bool = "%sequor"

type bounded = t
let min_num, max_num = false, true

let print out t = BatInnerIO.nwrite out (to_string t)
  (*$T
    BatIO.to_string print true = "true"
    BatIO.to_string print false = "false"
  *)
