(* 
 * ExtFloat - Extended floating-point numbers
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

include BatNumber.MakeNumeric(BaseFloat)
module Infix = BatNumber.MakeInfix(BaseFloat)
module Compare = BatNumber.MakeCompare(BaseFloat)

external of_float : float -> float = "%identity"
external to_float : float -> float = "%identity"
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

let infinity     = Pervasives.infinity
let neg_infinity = Pervasives.neg_infinity
let nan          = Pervasives.nan
let epsilon      = Pervasives.epsilon_float
let pi           = 4. *. atan 1.
  
let print out t = BatInnerIO.nwrite out (to_string t)
let t_printer paren out t = print out t

module Base_safe_float = struct
  include BaseFloat

  let if_safe x = match classify x with
    | FP_infinite -> raise Overflow
    | FP_nan      -> raise NaN
    | _           -> ()
  let check x = let _ = if_safe x in x

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
  let modf x = let (y,z) as result = modf x in if_safe y; if_safe z; result
  let frexp x = let (f,_) as result = frexp x in if_safe f; result
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
      
  let is_nan f = match classify f with
    | FP_nan -> true
    | _      -> false
      
  let infinity     = Pervasives.infinity
  let neg_infinity = Pervasives.neg_infinity
  let nan          = Pervasives.nan
  let epsilon      = Pervasives.epsilon_float
  let pi           = 4. *. atan 1.
    
  external of_float : float -> float = "%identity"
  external to_float : float -> float = "%identity"

  let print = print
  let t_printer paren out t = print out t
end
