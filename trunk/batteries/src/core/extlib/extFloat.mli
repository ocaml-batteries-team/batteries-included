(* 
 * ExtFloat - Extended floats
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



module Float :
  sig
    (**Operations on floating-point numbers

       @author Gabriel Scherer
       @author David Teller
       @author Edgar Friendly
    *)

    type t = float

    (**
       {6 Usual operations}
    *)

    val zero : float
    val one : float
    val neg : float -> float
    val succ : float -> float
    val pred : float -> float
    val abs : float -> float
    val add : float -> float -> float
    val sub : float -> float -> float
    val mul : float -> float -> float
    val div : float -> float -> float
    val modulo : float -> float -> float
    val pow : float -> float -> float
    val min_num : float
    val max_num : float
    val compare : 'a -> 'a -> int
    val of_int : int -> float
    val to_int : float -> int
    external of_float : float -> float = "%identity"
    external to_float : float -> float = "%identity"
    val of_string : string -> float
    val to_string : float -> string
    val ( + ) : t -> t -> t
    val ( - ) : t -> t -> t
    val ( * ) : t -> t -> t
    val ( / ) : t -> t -> t
    val ( ** ) : t -> t -> t
    val ( <> ) : t -> t -> bool
    val ( >= ) : t -> t -> bool
    val ( <= ) : t -> t -> bool
    val ( > ) : t -> t -> bool
    val ( < ) : t -> t -> bool
    val ( = ) : t -> t -> bool
    val operations : t Number.numeric

    (**
       {6 Operations specific to floating-point numbers}
    *)

    external exp : float -> float = "caml_exp_float" "exp" "float"
      (** Exponential. *)
      
    external log : float -> float = "caml_log_float" "log" "float"
      (** Natural logarithm. *)
      
    external log10 : float -> float = "caml_log10_float" "log10" "float"
      (** Base 10 logarithm. *)
      
    external cos : float -> float = "caml_cos_float" "cos" "float"
      (** See {!atan2}. *)
      
    external sin : float -> float = "caml_sin_float" "sin" "float"
      (** See {!atan2}. *)
      
    external tan : float -> float = "caml_tan_float" "tan" "float"
      (** See {!atan2}. *)
      
    external acos : float -> float = "caml_acos_float" "acos" "float"
      (** See {!atan2}. *)
      
    external asin : float -> float = "caml_asin_float" "asin" "float"
      (** See {!atan2}. *)
      
    external atan : float -> float = "caml_atan_float" "atan" "float"
      (** See {!atan2}. *)
      
    external atan2 : float -> float -> float = "caml_atan2_float" "atan2" "float"
      (** The usual trigonometric functions. *)
      
    external cosh : float -> float = "caml_cosh_float" "cosh" "float"
      (** See {!tanh}. *)

    external sinh : float -> float = "caml_sinh_float" "sinh" "float"
      (** See {!tanh}. *)
	
    external tanh : float -> float = "caml_tanh_float" "tanh" "float"
      (** The usual hyperbolic trigonometric functions. *)
      
    external ceil : float -> float = "caml_ceil_float" "ceil" "float"
      (** See {!floor}. *)
      
    external floor : float -> float = "caml_floor_float" "floor" "float"
      (** Round the given float to an integer value.
	  [floor f] returns the greatest integer value less than or
	  equal to [f].
	  [ceil f] returns the least integer value greater than or
	  equal to [f]. *)
      
    val infinity : float
      (** Positive infinity. *)
      
    val neg_infinity : float
      (** Negative infinity. *)
      
    val nan : float
      (** A special floating-point value denoting the result of an
	  undefined operation such as [0.0 /. 0.0].  Stands for
	  ``not a number''.  Any floating-point operation with [nan] as
	  argument returns [nan] as result.  As for floating-point comparisons,
	  [=], [<], [<=], [>] and [>=] return [false] and [<>] returns [true]
	  if one or both of their arguments is [nan]. *)
      
    val is_nan : float -> bool
      (** [is_nan f] returns [true] if [f] is [nan], [false] otherwise.*)
      
    val epsilon : float
      (** The smallest positive float [x] such that [1.0 +. x <> 1.0]. *)
      
    val pi : float
      (** The constant pi (3.14159...) *)
      
    (** {6 Operations on the internal representation of floating-point numbers}*)

    external frexp : float -> float * int = "caml_frexp_float"
      (** [frexp f] returns the pair of the significant
	  and the exponent of [f].  When [f] is zero, the
	  significant [x] and the exponent [n] of [f] are equal to
	  zero.  When [f] is non-zero, they are defined by
	  [f = x *. 2 ** n] and [0.5 <= x < 1.0]. *)
      
    external ldexp : float -> int -> float = "caml_ldexp_float"
      (** [ldexp x n] returns [x *. 2 ** n]. *)
      
    external modf : float -> float * float = "caml_modf_float"
      (** [modf f] returns the pair of the fractional and integral
	  part of [f]. *)

      (** Classes of floating point numbers*)
    type fpclass = Pervasives.fpclass = 
	FP_normal           (** Normal number, none of the below *)
      | FP_subnormal        (** Number very close to 0.0, has reduced precision *)
      | FP_zero             (** Number is 0.0 or -0.0 *)
      | FP_infinite         (** Number is positive or negative infinity *)
      | FP_nan              (** Not a number: result of an undefined operation *)
	  (** The five classes of floating-point numbers, as determined by
	      the {!classify} function. *)
	  
    external classify : float -> fpclass = "caml_classify_float"
	(** Return the class of the given floating-point number:
	    normal, subnormal, zero, infinite, or not a number. *)


    (** {6 Boilerplate code}*)
    (** {7 S-Expressions}*)

    val t_of_sexp : Sexplib.Sexp.t -> t
    val sexp_of_t : t -> Sexplib.Sexp.t
end
