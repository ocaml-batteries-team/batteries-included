(*
 * BatNum - Operations on arbitrary-precision numbers
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
  let order      = BatOrd.ord compare
  let equal      = BatOrd.eq_comp compare
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

module TaggedInfix = struct
  let (=/), (</), (>/), (<=/), (>=/), (<>/) = Num.((=/), (</), (>/), (<=/), (>=/), (<>/))
  let (+/), (-/), ( */ ), (//), ( **/ ) = Num.((+/), (-/), ( */ ), (//), ( **/ ))
end

module Infix = struct
  (* infix operators without / suffix: + - * / *)
  include BatNumber.MakeInfix (BaseNum)
  include TaggedInfix
end

include (BatNumber.MakeNumeric(BaseNum): BatNumber.Numeric with type t = Num.num and module Infix := Infix)

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

let of_float_string a =
  try
    let ipart_s,fpart_s = BatString.split a ~by:"." in
    if fpart_s = ""
    then of_string ipart_s
    else
      let frac = pow (of_int 10) (of_int (String.length fpart_s)) in
      div (of_string (ipart_s ^ fpart_s)) frac
  with Not_found -> of_string a

(*$T
   equal (of_float_string "2.5") (of_string "5/2")
   equal (of_float_string "-2.5") (of_string "-5/2")
   equal (of_float_string "-2.1") (of_string "-21/10")
   equal (of_float_string "2.") (of_string "2")
   equal (of_float_string ".5") (of_string "1/2")
   equal (of_float_string "-0.5") (of_string "-1/2")
   equal (of_float_string "-.5") (of_string "-1/2")
*)
