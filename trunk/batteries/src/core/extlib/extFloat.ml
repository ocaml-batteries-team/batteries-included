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

open Sexplib
TYPE_CONV_PATH "Batteries.Data.Numeric" (*For Sexplib, Bin-prot...*)

open Number

module BaseFloat = struct
  type t = float with sexp
  let zero, one = 0., 1.
  let neg = (~-.)

  let succ x = x +. 1.
  let pred x = x -. 1.
  let abs = abs_float

  let add, sub, mul, div = (+.), (-.), ( *.), (/.)
  let modulo = mod_float
  let pow = ( ** )

  let min_num, max_num = neg_infinity, infinity
  let compare = compare
    
  let of_int = float_of_int
  let to_int = int_of_float

  let of_string = float_of_string
  let to_string = string_of_float

  external of_float : float -> float = "%identity"
  external to_float : float -> float = "%identity"

  let is_nan f = match classify_float f with
    | FP_nan -> true
    | _      -> false

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
  external classify : float -> fpclass = "caml_classify_float"
    
  type fpclass = Pervasives.fpclass = 
      FP_normal           
      | FP_subnormal        
      | FP_zero             
      | FP_infinite         
      | FP_nan              

  let infinity     = Pervasives.infinity
  let neg_infinity = Pervasives.neg_infinity
  let nan          = Pervasives.nan
  let epsilon      = Pervasives.epsilon_float
  let pi           = 4. *. atan 1.
end

module Float = struct
  include Number.MakeNumeric(BaseFloat)
  include BaseFloat
  let print out t = InnerIO.nwrite out (to_string t)
end
