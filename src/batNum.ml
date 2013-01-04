(*
 * ExtNum - Operations on arbitrary-precision numbers
 * Copyright (C) 2008 Gabriel Scherer
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



module BaseNum = struct
  include Num
  type t = num
  
  let zero       = Int 0
  let one        = Int 1
  let neg        = minus_num
  let abs        = abs_num
  let add        = add_num
  let sub        = sub_num
  let mul        = mult_num
  let div        = div_num
  let modulo     = mod_num
  let pow        = power_num
  let compare    = compare_num
  let of_int     = num_of_int
  let to_int     = int_of_num
  let to_float   = float_of_num
  let to_string  = string_of_num
  let of_string  = num_of_string
  let pred       = pred_num
  let succ       = succ_num

  let of_float f =
    match classify_float f with
    | FP_normal
    | FP_subnormal ->
        let x,e = frexp f in
        let n,e =
          Big_int.big_int_of_int64 (Int64.of_float (ldexp x 52)),
          (e-52)
        in
        if e >= 0 then
          Big_int (Big_int.shift_left_big_int n e)
        else
          div
            (Big_int n)
            (Big_int Big_int.(shift_left_big_int unit_big_int ~-e))
    | FP_zero -> zero
    | FP_nan -> div zero zero
    | FP_infinite ->
        if f >= 0. then div one zero else div (neg one) zero
end
  
include BatNumber.MakeNumeric(BaseNum)
  
include Num
let round = round_num
let floor = floor_num
let ceil  = ceiling_num
let square= square_num
let is_integer = is_integer_num
let approx= integer_num
let quo   = quo_num
let sign  = sign_num
  
let print out t = BatInnerIO.nwrite out (to_string t)

module Infix = struct
  include BatNumber.MakeInfix (BaseNum)
  let (=/), (</), (>/), (<=/), (>=/), (<>/) = (=/), (</), (>/), (<=/), (>=/), (<>/)
  let (+/), (-/), ( */ ), (//), ( **/ ) = (+/), (-/), ( */ ), (//), ( **/ )
end 

module Compare = BatNumber.MakeCompare (BaseNum)

