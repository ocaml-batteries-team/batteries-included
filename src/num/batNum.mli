(*
 * BatNum - Operations on arbitrary-precision numbers
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


(** Operation on arbitrary-precision numbers.

    Numbers (type {!num}) are arbitrary-precision rational numbers,
    plus the special elements [1/0] (infinity) and [0/0] (undefined).

    @author Valerie Menissier-Morain (base module)
    @author Gabriel Scherer
    @author David Teller

    @documents Num
*)

open Nat
open Big_int
open Ratio

(** The type of numbers. *)
type num = Num.num =
  | Int of int
  | Big_int of big_int
  | Ratio of ratio

type t = num

(** {6 Usual operations}*)
val zero   : num
val one    : num
val neg    : num -> num
val abs    : num -> num
val add    : num -> num -> num
val sub    : num -> num -> num
val mul    : num -> num -> num
val div    : num -> num -> num
val modulo : num -> num -> num
val pow    : num -> num -> num

val compare   : num -> num -> int
val ord       : num -> num -> BatOrd.order
val equal     : num -> num -> bool
val of_int    : int -> num
val to_int    : num -> int
val of_float  : float -> num
val to_float  : num     -> float

val of_string : string -> num
val to_string : num -> string
(** Convert a number to a string, using fractional notation. Two
    formats are recognized: simple integer literals and a pair of integer
    literals separated by a '/', to indicate a rational number.*)

val of_float_string: string -> num
(** Convert a simple floating point literal to a num.  Plain integer
    literals are also accepted; numbers written with a trailing exponent
    are not currently accepted. *)

val ( + ) : num -> num -> num
val ( - ) : num -> num -> num
val ( * ) : num -> num -> num
val ( / ) : num -> num -> num
val ( ** ) : num -> num -> num
(* Available only in `Compare` submodule
   val ( <> ) : num -> num -> bool
   val ( >= ) : num -> num -> bool
   val ( <= ) : num -> num -> bool
   val ( > ) : num -> num -> bool
   val ( < ) : num -> num -> bool
   val ( = ) : num -> num -> bool
*)

val max_num : num -> num -> num
val min_num : num -> num -> num

(** {6 Additional operations}*)
val quo : num -> num -> num
(**Euclidian divisiom*)

val square: num -> num
val succ  : num -> num
(** @raise Invalid_argument ["Num.succ"] for [Num.Ratio _] argument *)
val pred  : num -> num
(** @raise Invalid_argument ["Num.pred"] for [Num.Ratio _] argument *)

val is_integer : num -> bool
(** [is_integer x] returns [true] if [x] represents an integer value,
    [false] otherwise *)

val round : num -> num
val floor : num -> num
val ceil  : num -> num

val approx: num -> num
(**[approx n] return the integer closest to [n]*)

val sign  : num -> int
(** Return [-1], [0] or [1] according to the sign of the argument. *)

val operations : num BatNumber.numeric



(** {6 Comparisons between numbers} *)

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





(** {6 Coercions with strings} *)




val approx_num_fix : int -> num -> string
(** See {!Num.approx_num_exp}.*)

val approx_num_exp : int -> num -> string
(** Approximate a number by a decimal. The first argument is the
    required precision. The second argument is the number to
    approximate. {!Num.approx_num_fix} uses decimal notation; the first
    argument is the number of digits after the decimal point.
    [approx_num_exp] uses scientific (exponential) notation; the
    first argument is the number of digits in the mantissa. *)



(** {6 Coercions between numerical types} *)
val nat_of_num : num -> nat
val num_of_nat : nat -> num
val num_of_big_int : big_int -> num
val big_int_of_num : num -> big_int
val ratio_of_num : num -> ratio
val num_of_ratio : ratio -> num
val float_of_num : num -> float

(** {6 Boilerplate code}*)

(** {7 Printing}*)
val print: 'a BatInnerIO.output -> t -> unit

(** {6 Submodules grouping all infix operators} *)

module TaggedInfix : sig
  val ( =/ ) : num -> num -> bool
  val ( </ ) : num -> num -> bool
  val ( >/ ) : num -> num -> bool
  val ( <=/ ) : num -> num -> bool
  val ( >=/ ) : num -> num -> bool
  val ( <>/ ) : num -> num -> bool

  val ( +/ ) : num -> num -> num
  val ( -/ ) : num -> num -> num
  val ( */ ) : num -> num -> num
  val ( // ) : num -> num -> num
  val ( **/ ) : num -> num -> num
end

module Infix : sig
  include BatNumber.Infix with type bat__infix_t = t
  val ( =/ ) : num -> num -> bool
  val ( </ ) : num -> num -> bool
  val ( >/ ) : num -> num -> bool
  val ( <=/ ) : num -> num -> bool
  val ( >=/ ) : num -> num -> bool
  val ( <>/ ) : num -> num -> bool

  val ( +/ ) : num -> num -> num
  val ( -/ ) : num -> num -> num
  val ( */ ) : num -> num -> num
  val ( // ) : num -> num -> num
  val ( **/ ) : num -> num -> num
end

module Compare : BatNumber.Compare with type bat__compare_t = t

(** {6 Deprecated} *)

val ( +/ ) : num -> num -> num
val add_num : num -> num -> num
val minus_num : num -> num
val ( -/ ) : num -> num -> num
val sub_num : num -> num -> num
val ( */ ) : num -> num -> num
val mult_num : num -> num -> num
val square_num : num -> num
val ( // ) : num -> num -> num
val div_num : num -> num -> num
val quo_num : num -> num -> num
val mod_num : num -> num -> num
val ( **/ ) : num -> num -> num
val power_num : num -> num -> num
val abs_num : num -> num
val succ_num : num -> num
val pred_num : num -> num
val incr_num : num ref -> unit
val decr_num : num ref -> unit
val is_integer_num : num -> bool

val integer_num : num -> num
val floor_num : num -> num
val round_num : num -> num
val ceiling_num : num -> num
val sign_num : num -> int


val string_of_num : num -> string
val num_of_string : string -> num

val int_of_num : num -> int
val num_of_int : int -> num
val compare_num : num -> num -> int
