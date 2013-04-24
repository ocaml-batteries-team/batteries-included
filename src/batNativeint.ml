(*
 * BatNativeInt - Extended native ints
 * Copyright (C) 2005 Damien Doligez
 *               2007 Bluestorm <bluestorm dot dylc on-the-server gmail dot com>
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


module BaseNativeint = struct
  include Nativeint

  let modulo = rem
  let pow = BatNumber.generic_pow ~zero ~one ~div_two:(fun n -> shift_right n 1) ~mod_two:(logand one) ~mul
end

include BatNumber.MakeNumeric(BaseNativeint)

let min_int = Nativeint.min_int
let max_int = Nativeint.max_int
let minus_one = Nativeint.minus_one
let lognot = Nativeint.lognot
let size = Nativeint.size
external neg : nativeint -> nativeint = "%nativeint_neg"
external add : nativeint -> nativeint -> nativeint = "%nativeint_add"
external sub : nativeint -> nativeint -> nativeint = "%nativeint_sub"
external mul : nativeint -> nativeint -> nativeint = "%nativeint_mul"
external div : nativeint -> nativeint -> nativeint = "%nativeint_div"
external rem : nativeint -> nativeint -> nativeint = "%nativeint_mod"
external logand : nativeint -> nativeint -> nativeint = "%nativeint_and"
external logor : nativeint -> nativeint -> nativeint = "%nativeint_or"
external logxor : nativeint -> nativeint -> nativeint = "%nativeint_xor"
external shift_left : nativeint -> int -> nativeint = "%nativeint_lsl"
external shift_right : nativeint -> int -> nativeint = "%nativeint_asr"
external shift_right_logical :
  nativeint -> int -> nativeint = "%nativeint_lsr"
external of_int : int -> nativeint = "%nativeint_of_int"
external to_int : nativeint -> int = "%nativeint_to_int"
external of_float : float -> nativeint = "caml_nativeint_of_float"
external to_float : nativeint -> float = "caml_nativeint_to_float"
external of_int32 : int32 -> nativeint = "%nativeint_of_int32"
external to_int32 : nativeint -> int32 = "%nativeint_to_int32"
external of_int64 : int64 -> nativeint = "%int64_to_nativeint"
external to_int64 : nativeint -> int64 = "%int64_of_nativeint"

(*$T of_int32
   (of_int32 8l) = 8n
*)
(*$T to_int32
   (to_int32 8n) = 8l
*)
(*$T of_int64
   (of_int64 9L) = 9n
*)
(*$T to_int64
   (to_int64 9n) = 9L
*)

external of_string : string -> nativeint = "caml_nativeint_of_string"
external format : string -> nativeint -> string = "caml_nativeint_format"


type bounded = t
let min_num, max_num = min_int, max_int

let print out t = BatPrintf.fprintf out "%nx" t
let t_printer _paren out t = print out t
