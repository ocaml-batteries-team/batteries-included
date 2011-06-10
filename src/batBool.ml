(* 
 * ExtBool - Extended booleans
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
  let div _ _=
    raise (Invalid_argument "Bool.div")

  let modulo _ _ = 
    raise (Invalid_argument "Bool.modulo")

  let pow _ _ = 
    raise (Invalid_argument "Bool.pow")

  let compare = compare
    
  let of_int = function
    | 0 -> false
    | _ -> true

  let to_int = function
    | false -> 0
    | true  -> 1

  open BatStd
  let of_float = of_int -| int_of_float
  let to_float = float_of_int -| to_int
  let of_string = function
    | "true" | "tt" | "1" -> true
    | "false"| "ff" | "0" -> false
    | _                   -> raise (Invalid_argument "Bool.of_string")

  let to_string = string_of_bool
end

include BatNumber.MakeNumeric(BaseBool)
module Infix = BatNumber.MakeInfix(BaseBool)
module Compare = BatNumber.MakeCompare(BaseBool)
  
external not : bool -> bool = "%boolnot"
external ( && ) : bool -> bool -> bool = "%sequand"
external ( || ) : bool -> bool -> bool = "%sequor"

type bounded = t
let min_num, max_num = false, true
  
let print out t = BatInnerIO.nwrite out (to_string t)
let t_printer paren out t = print out t
  
