(*
 * BatFloat - Extended floating-point numbers
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

module BaseFloat = struct
  type t = float
  let zero, one = 0., 1.
  let neg = (~-.)

  let succ x = x +. 1.
  let pred x = x -. 1.
  let abs = abs_float

  let add, sub, mul, div = (+.), (-.), ( *.), (/.)
  let modulo = mod_float
  let pow = ( ** )

  let compare = compare

  let of_int = float_of_int
  let to_int = int_of_float

  let of_string = float_of_string
  let to_string = string_of_float

  external of_float : float -> float = "%identity"
  external to_float : float -> float = "%identity"

end

let approx_equal ?(epsilon = 1e-5) f1 f2 = abs_float (f1 -. f2) < epsilon

(*$T approx_equal
   approx_equal 0. 1e-15
   approx_equal 0.3333333333 (1. /. 3.)
   not (approx_equal 1. 2.)
   not (approx_equal 1.5 1.45)
*)

external exp : float -> float = "caml_exp_float" "exp" "float"
external log : float -> float = "caml_log_float" "log" "float"
external log10 : float -> float = "caml_log10_float" "log10" "float"
external cos : float -> float = "caml_cos_float" "cos" "float"
external sin : float -> float = "caml_sin_float" "sin" "float"
external tan : float -> float = "caml_tan_float" "tan" "float"
external acos : float -> float = "caml_acos_float" "acos" "float"
external asin : float -> float = "caml_asin_float" "asin" "float"
external atan : float -> float = "caml_atan_float" "atan" "float"
external atan2 : float -> float -> float = "caml_atan2_float" "atan2" "float"
external cosh : float -> float = "caml_cosh_float" "cosh" "float"
external sinh : float -> float = "caml_sinh_float" "sinh" "float"
external tanh : float -> float = "caml_tanh_float" "tanh" "float"
external ceil : float -> float = "caml_ceil_float" "ceil" "float"
external floor : float -> float = "caml_floor_float" "floor" "float"
external frexp : float -> float * int = "caml_frexp_float"
external ldexp : float -> int -> float = "caml_ldexp_float"
external modf : float -> float * float = "caml_modf_float"

let root m n =
  if n < 0 then invalid_arg "Float.root: Negative root"
  else if m < 0. && n land 1 <> 1 then
    invalid_arg "Float.root: Imaginary result"
  else if m < 0. then
    -. exp (log (abs_float m) /. (float_of_int n))
  else
    exp (log m /. (float_of_int n))

(* sign bit is top bit, shift all other 63 bits away and test if = one

   Negative numbers have this bit set, positive unset.
*)
let signbit x = Int64.shift_right_logical (Int64.bits_of_float x) 63 = Int64.one

(*$T signbit
  signbit (-256.)
  not (signbit 1e50)
*)

let copysign x s =
  if signbit s then
    -. (abs_float x)
  else
    abs_float x

(*$T copysign
   copysign 1. 1. = 1.
   copysign 1. (-1.) = (-1.)
*)

let round x =
  (* 'halve' is the biggest representable double that is smaller than 0.5;
     (halve +. 0.5) rounds to 1., which makes for incorrect rounding of 'halve',
     while (halve +. halve) is strictly smaller than 1. as expected. *)
  let halve = 0.499999999999999944 in    
  (* we test x >= 0. rather than x > 0. because otherwise
     round_to_string 0. returns "-0." (ceil of -0.5 is 'negative
     zero') which is confusing. *)
  if x >= 0.0 then floor (x +. halve) else ceil (x -. halve)

(* the tests below look ugly with those Pervasives.(...); this is
   a temporary fix made necessary by BatFloat overriding the (=)
   operator. Hugh. *)
(*$T round
   Pervasives.(=) (List.map round [1.1; 2.4; 3.3; 3.5; 4.99]) [1.; 2.; 3.; 4.; 5.]
   Pervasives.(=) (List.map round [-1.1; -2.4; -3.3; -3.5; -4.99]) [-1.; -2.; -3.; -4.; -5.]
   round 0.499999999999999944 = 0.
   round (-0.499999999999999944) = 0.
*)

let round_to_int x =
  int_of_float (round x)

(*$T round_to_int
   Pervasives.(=) (List.map round_to_int [1.1; 2.4; 3.3; 3.5; 4.99]) [1; 2; 3; 4; 5]
*)

module Infix = struct
  include BatNumber.MakeInfix(BaseFloat)
  let (=~) = approx_equal
end

include (BatNumber.MakeNumeric(BaseFloat) : BatNumber.Numeric with type t = float and module Infix := Infix)

let min (x:float) y = if x < y then x else y
let max (x:float) y = if x < y then y else x

(* Fix definitions for performance *)
external of_float : float -> float = "%identity"
external to_float : float -> float = "%identity"
external sqrt : float -> float = "caml_sqrt_float" "sqrt" "float"
external neg : float -> float = "%negfloat"
external abs : float -> float = "%absfloat"
external modulo : float -> float -> float = "caml_fmod_float" "fmod" "float"
external pow : float -> float -> float = "caml_power_float" "pow" "float"
external of_int : int -> float = "%floatofint"
external to_int : float -> int = "%intoffloat"
external of_float : float -> float = "%identity"
external to_float : float -> float = "%identity"
external ( + ) : t -> t -> t = "%addfloat"
external ( - ) : t -> t -> t = "%subfloat"
external ( * ) : t -> t -> t = "%mulfloat"
external ( / ) : t -> t -> t = "%divfloat"
external ( ** ) : t -> t -> t = "caml_power_float" "pow" "float"


type bounded = t
let min_num, max_num = neg_infinity, infinity

type fpkind = Pervasives.fpclass =
  | FP_normal
  | FP_subnormal
  | FP_zero
  | FP_infinite
  | FP_nan
external classify : float -> fpkind = "caml_classify_float"

let is_nan f = match classify f with
  | FP_nan -> true
  | _      -> false

let is_special f =
  match classify f with
  | FP_nan
  | FP_infinite -> true
  | FP_normal
  | FP_subnormal
  | FP_zero -> false

let is_finite f =
  match classify f with
  | FP_nan
  | FP_infinite -> false
  | FP_normal
  | FP_subnormal
  | FP_zero -> true

let infinity     = Pervasives.infinity
let neg_infinity = Pervasives.neg_infinity
let nan          = Pervasives.nan
let epsilon      = Pervasives.epsilon_float
let e = 2.7182818284590452354
let log2e = 1.4426950408889634074
let log10e = 0.43429448190325182765
let ln2 = 0.69314718055994530942
let ln10 = 2.30258509299404568402
let pi = 3.14159265358979323846
let pi2 = 1.57079632679489661923
let pi4 = 0.78539816339744830962
let invpi = 0.31830988618379067154
let invpi2 = 0.63661977236758134308
let sqrtpi2 = 1.12837916709551257390
let sqrt2 = 1.41421356237309504880
let invsqrt2 = 0.70710678118654752440

let print out t = BatInnerIO.nwrite out (to_string t)


let round_to_string ?(digits=0) x =
  if Pervasives.(<) digits 0 then invalid_arg "Float.round_to_string";
  match classify x with
  | FP_normal
  | FP_subnormal
  | FP_zero ->
    BatPrintf.sprintf "%.*f" digits x
  (* we don't call sprintf in the 'special' cases as it seems to
     behave weirdly in some cases (eg. on Windows, bug #191) *)
  | FP_infinite ->
    if x = neg_infinity then "-inf" else "inf"
  | FP_nan -> "nan"

(*$T round_to_string
   List.mem (round_to_string 3.) ["3."; "3"]
   Pervasives.(=) (round_to_string ~digits:0 3.) (round_to_string 3.)
   Pervasives.(=) (round_to_string ~digits:1 3.) "3.0"
   Pervasives.(=) (round_to_string ~digits:1 0.) "0.0"
   Pervasives.(=) (round_to_string ~digits:1 epsilon_float) "0.0"
   Pervasives.(=) (round_to_string ~digits:3 1.23456) "1.235"
   Pervasives.(=) (round_to_string ~digits:3 (- 1.23456)) "-1.235"
   Pervasives.(=) (round_to_string ~digits:3 1.98765) "1.988"
   Pervasives.(=) (round_to_string ~digits:3 (- 1.98765)) "-1.988"
   Result.(catch (round_to_string ~digits:(-1)) 3. |> is_exn (Invalid_argument "Float.round_to_string"))
   List.mem (round_to_string 0.5) ["0"; "0."; "1"; "1."]
   List.mem (round_to_string (-0.5)) ["-1"; "-1."; "0"; "0."; "-0"; "-0."]
   List.mem (round_to_string ~digits:2 0.215) ["0.21"; "0.22"]
   List.mem (round_to_string ~digits:2 (-0.215)) ["-0.22"; "-0.21"]
   Pervasives.(=) (round_to_string ~digits:32 epsilon_float) "0.00000000000000022204460492503131"
   List.mem (round_to_string ~digits:42 infinity) ["inf"; "infinity"]
   List.mem (round_to_string ~digits:0 neg_infinity) ["-inf"; "-infinity"]
   List.for_all (fun digits -> Pervasives.(=) "nan" (String.sub (round_to_string ~digits nan) 0 3)) [0; 42]
*)


module Base_safe_float = struct
  include BaseFloat

  let if_safe x = match classify x with
    | FP_infinite -> raise Overflow
    | FP_nan      -> raise NaN
    | _           -> ()
  let check x = if_safe x; x

  let safe1 f x   = check (f x)
  let safe2 f x y = check (f x y)

  let add     = safe2 add
  let sub     = safe2 sub
  let div     = safe2 div
  let mul     = safe2 mul
  let modulo  = safe2 modulo
  let pred    = safe1 pred
  let succ    = safe1 succ
  let pow     = safe2 pow
end

module Safe_float = struct
  include BatNumber.MakeNumeric(Base_safe_float)

  let safe1 = Base_safe_float.safe1
  let safe2 = Base_safe_float.safe2
  let if_safe = Base_safe_float.if_safe
  let exp = safe1 exp
  let log = safe1 log
  let log10 = safe1 log10
  let cos = safe1 cos
  let sin = safe1 sin
  let tan = safe1 tan
  let acos = safe1 acos
  let asin = safe1 asin
  let atan = safe1 atan
  let atan2 = safe2 atan2
  let cosh = safe1 cosh
  let sinh = safe1 sinh
  let tanh = safe1 tanh
  let ceil = safe1 ceil
  let floor = safe1 floor
  let modf x = let (_, z) as result = modf x in if_safe z; result
  let frexp x = let (f, _) as result = frexp x in if_safe f; result (*BISECT-VISIT*)
  let ldexp = safe2 ldexp

  type bounded = t
  let min_num, max_num = neg_infinity, infinity

  type fpkind = Pervasives.fpclass =
    | FP_normal
    | FP_subnormal
    | FP_zero
    | FP_infinite
    | FP_nan
  external classify : float -> fpkind = "caml_classify_float"

  let is_nan = is_nan
  let infinity     = Pervasives.infinity
  let neg_infinity = Pervasives.neg_infinity
  let nan          = Pervasives.nan
  let epsilon      = Pervasives.epsilon_float
  let pi           = 4. *. atan 1.

  external of_float : float -> float = "%identity"
  external to_float : float -> float = "%identity"

  let print = print
end

(*$T succ
  is_nan (succ nan)
  succ infinity = infinity
  succ neg_infinity = neg_infinity
  succ (-3.) = -2.
*)

(*$T pred
  is_nan (pred nan)
  pred infinity = infinity
  pred neg_infinity = neg_infinity
  pred (-3.) = -4.
*)

(*$T root
   approx_equal (root 9. 2) 3.
   approx_equal (root 8. 3) 2.
   approx_equal (root 1. 20) 1.
   approx_equal (root (-8.) 3) (-2.)
   approx_equal (root 0. 6) 0.
   approx_equal (root (-0.) 6) 0.
   is_nan (root nan 4)
   root infinity 4 = infinity
   root neg_infinity 3 = neg_infinity
   try ignore (root (-8.) 4); false with Invalid_argument _ -> true
   try ignore (root neg_infinity 4); false with Invalid_argument _ -> true
   try ignore (root (9.) (-2)); false with Invalid_argument _ -> true
*)

(*$T is_nan
  not (is_nan infinity)
  not (is_nan neg_infinity)
  not (is_nan (-0.))
  not (is_nan 12.)
  is_nan nan
*)

(*$T is_special
  is_special infinity
  is_special neg_infinity
  not (is_special (-0.))
  not (is_special 12.)
  is_special nan
*)

(*$T is_finite
   List.for_all is_finite [1.0; 1e200; 1e-200; 0.0; -0.0; -1.0; -1e200; -1e-200]
   not (is_finite nan)
   not (is_finite infinity)
   not (is_finite neg_infinity)
*)

(*$T
  try ignore (Safe_float.add 0. infinity); false with BatNumber.Overflow -> true
  try ignore (Safe_float.add 0. neg_infinity); false with BatNumber.Overflow -> true
  try ignore (Safe_float.add 0. nan); false with BatNumber.NaN -> true
  ignore (Safe_float.add 0. (-0.)); true
  ignore (Safe_float.add 0. (12.)); true
*)

(*$T
  try ignore (Safe_float.modf nan); false with Number.NaN -> true
  try ignore (Safe_float.modf infinity); false with Number.Overflow -> true
  try ignore (Safe_float.modf neg_infinity); false with Number.Overflow -> true
  let (frac, int) = Safe_float.modf 3.234 in approx_equal frac 0.234 && approx_equal int 3.
*)
