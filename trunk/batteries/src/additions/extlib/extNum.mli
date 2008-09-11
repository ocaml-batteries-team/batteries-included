(*
 * ExtNum - Operations on arbitrary-precision numbers
 * Copyright (C) 1996 Valerie Menissier-Morain
 *               2008 Gabriel Scherer
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


module Num :
  sig
(** Operation on arbitrary-precision numbers.

   Numbers (type [num]) are arbitrary-precision rational numbers,
   plus the special elements [1/0] (infinity) and [0/0] (undefined). 
*)

    open Nat
    open Big_int
    open Ratio

    (** The type of numbers. *)
    type num = Num.num =
	Int of int
      | Big_int of big_int
      | Ratio of ratio
    type t = num

    (** {6 Usual operations}*)
    val zero    : num
    val one     : num
    val neg : num -> num
    val abs : num -> num
    val add : num -> num -> num
    val sub : num -> num -> num
    val mul : num -> num -> num
    val div : num -> num -> num
    val modulo : num -> num -> num
    val pow : num -> num -> num
    val compare : num -> num -> int
    val of_int : int -> num
    val to_int : num -> int
    val of_float: float -> num
    val to_float: num     -> float
    val of_string : string -> num
    val to_string : num -> string
    val ( + ) : num -> num -> num
    val ( - ) : num -> num -> num
    val ( * ) : num -> num -> num
    val ( / ) : num -> num -> num
    val ( ** ) : num -> num -> num
    val ( <> ) : num -> num -> bool
    val ( >= ) : num -> num -> bool
    val ( <= ) : num -> num -> bool
    val ( > ) : num -> num -> bool
    val ( < ) : num -> num -> bool
    val ( = ) : num -> num -> bool
    
    val operations : num numeric

      
    (** {6 Special operations} *)
	  
	  
    val ( +/ ) : num -> num -> num
      (** Same as {!Num.add_num}.*)
      
    val add_num : num -> num -> num
      (** Addition *)

    val minus_num : num -> num
      (** Unary negation. *)
      
    val ( -/ ) : num -> num -> num
      (** Same as {!Num.sub_num}.*)
      
    val sub_num : num -> num -> num
      (** Subtraction *)
      
    val ( */ ) : num -> num -> num
      (** Same as {!Num.mult_num}.*)
      
    val mult_num : num -> num -> num
      (** Multiplication *)
      
    val square_num : num -> num
      (** Squaring *)
      
    val ( // ) : num -> num -> num
      (** Same as {!Num.div_num}.*)
      
    val div_num : num -> num -> num
      (** Division *)
      
    val quo_num : num -> num -> num
      (** Euclidean division: quotient. *)
      
    val mod_num : num -> num -> num
      (** Euclidean division: remainder. *)
      
    val ( **/ ) : num -> num -> num
      (** Same as {!Num.power_num}. *)
      
    val power_num : num -> num -> num
      (** Exponentiation *)
      
    val abs_num : num -> num
      (** Absolute value. *)
      
    val succ_num : num -> num
      (** [succ n] is [n+1] *)
      
    val pred_num : num -> num
      (** [pred n] is [n-1] *)
      
    val incr_num : num ref -> unit
      (** [incr r] is [r:=!r+1], where [r] is a reference to a number. *)
      
    val decr_num : num ref -> unit
      (** [decr r] is [r:=!r-1], where [r] is a reference to a number. *)
      
    val is_integer_num : num -> bool
      (** Test if a number is an integer *)
      
    (** The four following functions approximate a number by an integer : *)
      
    val integer_num : num -> num
      (** [integer_num n] returns the integer closest to [n]. In case of ties, 
	  rounds towards zero. *)
      
    val floor_num : num -> num
      (** [floor_num n] returns the largest integer smaller or equal to [n]. *)
      
    val round_num : num -> num
      (** [round_num n] returns the integer closest to [n]. In case of ties,
	  rounds off zero. *)
      
    val ceiling_num : num -> num
      (** [ceiling_num n] returns the smallest integer bigger or equal to [n]. *)
      
      
    val sign_num : num -> int
      (** Return [-1], [0] or [1] according to the sign of the argument. *)
      
    (** {7 Comparisons between numbers} *)
      
    val ( =/ ) : num -> num -> bool
    val ( </ ) : num -> num -> bool
    val ( >/ ) : num -> num -> bool
    val ( <=/ ) : num -> num -> bool
    val ( >=/ ) : num -> num -> bool
    val ( <>/ ) : num -> num -> bool
    val eq_num : num -> num -> bool
    val lt_num : num -> num -> bool
    val le_num : num -> num -> bool
    val gt_num : num -> num -> bool
    val ge_num : num -> num -> bool
      
    val compare_num : num -> num -> int
      (** Return [-1], [0] or [1] if the first argument is less than,
	  equal to, or greater than the second argument. *)
      
    val max_num : num -> num -> num
      (** Return the greater of the two arguments. *)
      
    val min_num : num -> num -> num
      (** Return the smaller of the two arguments. *)
      
      
    (** {6 Coercions with strings} *)
      
    val string_of_num : num -> string
      (** Convert a number to a string, using fractional notation. *)
      
    val approx_num_fix : int -> num -> string
      (** See {!Num.approx_num_exp}.*)
      
    val approx_num_exp : int -> num -> string
      (** Approximate a number by a decimal. The first argument is the
	  required precision. The second argument is the number to
	  approximate. {!Num.approx_num_fix} uses decimal notation; the first
	  argument is the number of digits after the decimal point.
	  [approx_num_exp] uses scientific (exponential) notation; the
	  first argument is the number of digits in the mantissa. *)
      
    val num_of_string : string -> num
      (** Convert a string to a number. *)
      
    (** {6 Coercions between numerical types} *)
      
    val int_of_num : num -> int
    val num_of_int : int -> num
    val nat_of_num : num -> nat
    val num_of_nat : nat -> num
    val num_of_big_int : big_int -> num
    val big_int_of_num : num -> big_int
    val ratio_of_num : num -> ratio
    val num_of_ratio : ratio -> num
    val float_of_num : num -> float
      
  end
