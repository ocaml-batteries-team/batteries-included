(* 
 * ExtFloat - Extended floats
 * Copyright (C) 2007 Bluestorm <bluestorm dot dylc on-the-server gmail dot com>
 *               2009 David Rajchenbach-Teller, LIFO, Universite d'Orleans
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



(**Operations on floating-point numbers.

   Caml's floating-point numbers follow the IEEE 754 standard, using
   double precision (64 bits) numbers. Floating-point operations never
   raise an exception on overflow, underflow, division by zero,
   etc. Instead, special IEEE numbers are returned as appropriate,
   such as [infinity] for [1.0 /. 0.0], [neg_infinity] for [-1.0 /. 0.0], and
   [nan] (``not a number'') for [0.0 /. 0.0]. These special numbers then
   propagate through floating-point computations as expected: for
   instance, [1.0 /. infinity] is [0.0], and any operation with [nan] as
   argument returns [nan] as result.

   For more precision, see
   {{:http://en.wikipedia.org/wiki/IEEE_754}The Wikipedia entry on
   standard IEEE 754}.
   
   @author Gabriel Scherer
   @author David Teller
   @author Edgar Friendly
   
   @documents Float
*)

    type t = float
	(**The type of floating-point numbers.

	   Floating-point numbers are the default representation of
	   real numbers by OCaml. *)

    (**
       {6 Usual operations}
    *)

    val zero : float
      (** Floating number zero. This is the same thing as [0.]*)

    val one : float
      (** Floating number one. This is the same thing as [1.]*)

    val neg : float -> float

    val succ : float -> float
      (** Add [1.] to a floating number. Note that, as per IEEE 754,
	  if [x] is a large enough float number, [succ x] might be
	  equal to [x], due to rounding.*)

    val pred : float -> float
      (** Substract [1.] from a floating number. Note that, as per
	  IEEE 754, if [x] is a large enough float number, [pred x]
	  might be equal to [x], due to rounding.*)

    val abs : float -> float
      (** The absolute value of a floating point number.*)

    val add : float -> float -> float
    val sub : float -> float -> float
    val mul : float -> float -> float
    val div : float -> float -> float
    val modulo : float -> float -> float
    val pow : float -> float -> float
    val min_num : float
    val max_num : float
    val compare : float -> float -> int
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
    val ( -- ): t -> t -> t BatEnum.t
    val ( --- ): t -> t -> t BatEnum.t
    val operations : t BatNumber.numeric

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
    type fpkind = Pervasives.fpclass = 
	FP_normal           (** Normal number, none of the below *)
      | FP_subnormal        (** Number very close to 0.0, has reduced precision *)
      | FP_zero             (** Number is 0.0 or -0.0 *)
      | FP_infinite         (** Number is positive or negative infinity *)
      | FP_nan              (** Not a number: result of an undefined operation *)
	  (** The five classes of floating-point numbers, as determined by
	      the {!classify} function. *)
	  
    external classify : float -> fpkind = "caml_classify_float"
	(** Return the class of the given floating-point number:
	    normal, subnormal, zero, infinite, or not a number. *)

    (** {6 Submodules grouping all infix operators} *)

    module Infix : BatNumber.Infix with type bat__infix_t = t
    module Compare : BatNumber.Compare with type bat__compare_t = t

    (** {6 Boilerplate code}*)

    (** {7 Printing}*)
    val print: 'a BatInnerIO.output -> t -> unit
    val t_printer : t BatValue_printer.t



(**Operations on floating-point numbers, with exceptions raised in
   case of error. 

   The operations implemented in this module are the same as the operations
   implemented in module {!Float}, with the exception that no operation returns
   [nan], [infinity] or [neg_infinity]. In case of overflow, instead of returning
   [infinity] or [neg_infinity], operations raise exception {!Number.Overflow}.
   In case of [nan], operations raise exception {!Number.NaN}.

   Caml's floating-point numbers follow the IEEE 754 standard, using
   double precision (64 bits) numbers. Floating-point operations never
   raise an exception on overflow, underflow, division by zero,
   etc. Instead, special IEEE numbers are returned as appropriate,
   such as [infinity] for [1.0 /. 0.0], [neg_infinity] for [-1.0 /. 0.0], and
   [nan] (``not a number'') for [0.0 /. 0.0]. These special numbers then
   propagate through floating-point computations as expected: for
   instance, [1.0 /. infinity] is [0.0], and any operation with [nan] as
   argument returns [nan] as result.

   For more precision, see
   {{:http://en.wikipedia.org/wiki/IEEE_754}The Wikipedia entry on
   standard IEEE 754}.
   
   @author David Teller
   
   @documents Safe_float
*)
module Safe_float :
  sig


    type t = float
	(**The type of floating-point numbers.

	   Floating-point numbers are the default representation of
	   real numbers by OCaml. *)

    (**
       {6 Usual operations}
    *)

    val zero : float
      (** Floating number zero. This is the same thing as [0.]*)

    val one : float
      (** Floating number one. This is the same thing as [1.]*)

    val neg : float -> float

    val succ : float -> float
      (** Add [1.] to a floating number. Note that, as per IEEE 754,
	  if [x] is a large enough float number, [succ x] might be
	  equal to [x], due to rounding.*)

    val pred : float -> float
      (** Substract [1.] from a floating number. Note that, as per
	  IEEE 754, if [x] is a large enough float number, [pred x]
	  might be equal to [x], due to rounding.*)

    val abs : float -> float
      (** The absolute value of a floating point number.*)

    val add : float -> float -> float
    val sub : float -> float -> float
    val mul : float -> float -> float
    val div : float -> float -> float
    val modulo : float -> float -> float
    val pow : float -> float -> float
    val min_num : float
    val max_num : float
    val compare : float -> float -> int
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
    val operations : t BatNumber.numeric

    (**
       {6 Operations specific to floating-point numbers}
    *)

    val exp : float -> float
      (** Exponential. *)
      
    val log : float -> float
      (** Natural logarithm. *)
      
    val log10 : float -> float
      (** Base 10 logarithm. *)
      
    val cos : float -> float
      (** See {!atan2}. *)
      
    val sin : float -> float
      (** See {!atan2}. *)
      
    val tan : float -> float
      (** See {!atan2}. *)
      
    val acos : float -> float
      (** See {!atan2}. *)
      
    val asin : float -> float
      (** See {!atan2}. *)
      
    val atan : float -> float
      (** See {!atan2}. *)
      
    val atan2 : float -> float -> float
      (** The usual trigonometric functions. *)
      
    val cosh : float -> float
      (** See {!tanh}. *)

    val sinh : float -> float
      (** See {!tanh}. *)
	
    val tanh : float -> float
      (** The usual hyperbolic trigonometric functions. *)
      
    val ceil : float -> float
      (** See {!floor}. *)
      
    val floor : float -> float
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

    val frexp : float -> float * int
      (** [frexp f] returns the pair of the significant
	  and the exponent of [f].  When [f] is zero, the
	  significant [x] and the exponent [n] of [f] are equal to
	  zero.  When [f] is non-zero, they are defined by
	  [f = x *. 2 ** n] and [0.5 <= x < 1.0]. *)
      
    val ldexp : float -> int -> float
      (** [ldexp x n] returns [x *. 2 ** n]. *)
      
    val modf : float -> float * float
      (** [modf f] returns the pair of the fractional and integral
	  part of [f]. *)

      (** Classes of floating point numbers*)
    type fpkind = Pervasives.fpclass = 
	FP_normal           (** Normal number, none of the below *)
      | FP_subnormal        (** Number very close to 0.0, has reduced precision *)
      | FP_zero             (** Number is 0.0 or -0.0 *)
      | FP_infinite         (** Number is positive or negative infinity *)
      | FP_nan              (** Not a number: result of an undefined operation *)
	  (** The five classes of floating-point numbers, as determined by
	      the {!classify} function. *)
	  
    external classify : float -> fpkind = "caml_classify_float"
	(** Return the class of the given floating-point number:
	    normal, subnormal, zero, infinite, or not a number. *)


    (** {6 Boilerplate code}*)

    (** {7 Printing}*)
    val print: 'a BatInnerIO.output -> t -> unit
    val t_printer : t BatValue_printer.t
end
