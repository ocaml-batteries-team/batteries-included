(*
 * BatBig_int - Extended operations on big integers
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


(** Operations on arbitrary-precision integers.

    Big integers (type {!big_int} or equivalently {!Big_int.t}) are
    signed integers of arbitrary size. This module lets you compute
    with huge numbers, whose size is limited only by the amount of
    memory given to OCaml. The downside is speed, as big integers
    are much slower than any other type of integer known to OCaml.

    This module replaces Stdlib's
    {{:http://caml.inria.fr/pub/docs/manual-ocaml/libref/Big_int.html}Big_int}
    module.

    @author Valerie Menissier-Morain (base module)
    @author Gabriel Scherer
    @author David Teller
*)

type big_int = Big_int.big_int
(** The type of big integers. *)

val zero : big_int
val zero_big_int : big_int
(** The big integer [0]. *)

val one : big_int
val unit_big_int : big_int
(** The big integer [1]. *)

(** {6 Arithmetic operations} *)

val neg : big_int -> big_int
val succ : big_int -> big_int
val pred : big_int -> big_int
val abs : big_int -> big_int
val add : big_int -> big_int -> big_int
val sub : big_int -> big_int -> big_int
val mul : big_int -> big_int -> big_int
val div : big_int -> big_int -> big_int
val modulo : big_int -> big_int -> big_int
val pow : big_int -> big_int -> big_int

type t = big_int
val ( + ) : t -> t -> t
val ( - ) : t -> t -> t
val ( * ) : t -> t -> t
val ( / ) : t -> t -> t
val ( ** ) : t -> t -> t

val minus_big_int : big_int -> big_int
(** Unary negation. *)
val abs_big_int : big_int -> big_int
(** Absolute value. *)
val add_big_int : big_int -> big_int -> big_int
(** Addition. *)
val succ_big_int : big_int -> big_int
(** Successor (add 1). *)
val add_int_big_int : int -> big_int -> big_int
(** Addition of a small integer to a big integer. *)
val sub_big_int : big_int -> big_int -> big_int
(** Subtraction. *)
val pred_big_int : big_int -> big_int
(** Predecessor (subtract 1). *)
val mult_big_int : big_int -> big_int -> big_int
(** Multiplication of two big integers. *)
val mult_int_big_int : int -> big_int -> big_int
(** Multiplication of a big integer by a small integer *)
val square_big_int: big_int -> big_int
(** Return the square of the given big integer *)
val sqrt_big_int: big_int -> big_int
(** [sqrt_big_int a] returns the integer square root of [a],
    that is, the largest big integer [r] such that [r * r <= a].
    @raise Invalid_argument if [a] is negative. *)
val quomod_big_int : big_int -> big_int -> big_int * big_int
(** Euclidean division of two big integers.
    The first part of the result is the quotient,
    the second part is the remainder.
    Writing [(q,r) = quomod_big_int a b], we have
    [a = q * b + r] and [0 <= r < |b|].
    @raise Division_by_zero if the divisor is zero. *)
val div_big_int : big_int -> big_int -> big_int
(** Euclidean quotient of two big integers.
    This is the first result [q] of [quomod_big_int] (see above). *)
val mod_big_int : big_int -> big_int -> big_int
(** Euclidean modulus of two big integers.
    This is the second result [r] of [quomod_big_int] (see above). *)
val gcd_big_int : big_int -> big_int -> big_int
(** Greatest common divisor of two big integers. *)
val power_int_positive_int: int -> int -> big_int
val power_big_int_positive_int: big_int -> int -> big_int
val power_int_positive_big_int: int -> big_int -> big_int
val power_big_int_positive_big_int: big_int -> big_int -> big_int
(** Exponentiation functions.  Return the big integer
    representing the first argument [a] raised to the power [b]
    (the second argument).  Depending
    on the function, [a] and [b] can be either small integers
    or big integers.  @raise Invalid_argument if [b] is negative. *)

val operations : t BatNumber.numeric

(** {6 Generators} *)

val ( -- ) : big_int -> big_int -> big_int BatEnum.t
val ( --- ): big_int -> big_int -> big_int BatEnum.t

(** {6 Comparisons and tests} *)

val compare : big_int -> big_int -> int
val ord : big_int -> big_int -> BatOrd.order
val equal : big_int -> big_int -> bool

(* Available only in `Compare` submodule
    val ( <> ) : t -> t -> bool
    val ( >= ) : t -> t -> bool
    val ( <= ) : t -> t -> bool
    val ( > ) : t -> t -> bool
    val ( < ) : t -> t -> bool
    val ( = ) : t -> t -> bool
*)

val sign_big_int : big_int -> int
(** Return [0] if the given big integer is zero,
    [1] if it is positive, and [-1] if it is negative. *)
val compare_big_int : big_int -> big_int -> int
(** [compare_big_int a b] returns [0] if [a] and [b] are equal,
    [1] if [a] is greater than [b], and [-1] if [a] is smaller
    than [b]. *)
val eq_big_int : big_int -> big_int -> bool
val le_big_int : big_int -> big_int -> bool
val ge_big_int : big_int -> big_int -> bool
val lt_big_int : big_int -> big_int -> bool
val gt_big_int : big_int -> big_int -> bool
(** Usual boolean comparisons between two big integers. *)
val max_big_int : big_int -> big_int -> big_int
(** Return the greater of its two arguments. *)
val min_big_int : big_int -> big_int -> big_int
(** Return the smaller of its two arguments. *)
val num_digits_big_int : big_int -> int
(** Return the number of machine words used to store the
    given big integer.  *)

(** {6 Conversions to and from strings} *)

val to_string : big_int -> string
val string_of_big_int : big_int -> string
(** Return the string representation of the given big integer,
    in decimal (base 10). *)

val of_string : string -> big_int
val big_int_of_string : string -> big_int
(** Convert a string to a big integer, in decimal.
    The string consists of an optional [-] or [+] sign,
    followed by one or several decimal digits. *)

val to_string_in_binary : big_int -> string
(** as [string_of_big_int], but in base 2 *)
val to_string_in_octal  : big_int -> string
(** as [string_of_big_int], but in base 8 *)
val to_string_in_hexa   : big_int -> string
(** as [string_of_big_int], but in base 16 *)

val to_string_in_base : int -> big_int -> string
(** [to_string_in_base b n] returns the string representation in base [b] of
    the given big integer [n]. Should you have advanced needs (arbitrarily large
    bases, or custom digits instead of the usual [0,1,...9,a,b,...,z]), use
    [to_string_in_custom_base] instead. @raise Invalid_argument if b is not in
    [2 .. 36]. *)

val to_string_in_custom_base : string -> int -> big_int -> string
(** First argument, called [symbols], is the vector of the symbols used to
    represent the digits in base [b]. [to_string_in_base] is almost equivalent to
    [to_string_in_custom_base big_int_base_default_symbols], the difference being
    that [to_string_in_custom_base] allows the base to be arbitrarily large,
    provided that [symbols] can accommodate it. Concretely, the base [b] must be at
    least [2], and [symbols] must be of size at least [b]. The default value of
    [big_int_base_default_symbols] contains 62 symbols, as it uses lowercase and
    uppercase letters both. See below for more information. @raise Invalid_argument
    if [b] is incorrect. *)


val big_int_base_default_symbols : string
(** Default vector of symbols used by [to_string_in_base] and its fixed-base
    derivatives [to_string_in_binary], [to_string_in_octal] and [to_string_in_hexa]
    to represent digits. The symbol at position [p] encodes the value [p]. The
    original value of this vector is, schematically, [['0'..'9' 'a' 'b'..'z' 'A'
    'B'..'Z']], which is sufficient for bases up to and including 62. The basic
    [to_string_in_base] function is capped to base 36 to avoid unexpected
    behaviours do to the case-sensitivity of the output in bases 37 to 62. You
    technically {i can} mutate the vector, for instance if you prefer to exchange
    lower- and upper-case symbols program-wide. As usual where mutability is
    concerned, discretion is advised. Most of the time, it is better to build
    custom functions using [to_string_in_custom_base].
*)


(** {6 Conversions to and from other numerical types} *)


val of_int : int -> big_int
val big_int_of_int : int -> big_int
(** Convert a small integer to a big integer. *)
val is_int_big_int : big_int -> bool
(** Test whether the given big integer is small enough to
    be representable as a small integer (type [int])
    without loss of precision.  On a 32-bit platform,
    [is_int_big_int a] returns [true] if and only if
    [a] is between -2{^30} and 2{^30}-1.  On a 64-bit platform,
    [is_int_big_int a] returns [true] if and only if
    [a] is between -2{^62} and 2{^62}-1. *)

val to_int : big_int -> int
val int_of_big_int : big_int -> int
(** Convert a big integer to a small integer (type [int]).
    @raise Failure if the big integer
    is not representable as a small integer. *)

val big_int_of_int32 : int32 -> big_int
(** Convert a 32-bit integer to a big integer. *)
val big_int_of_nativeint : nativeint -> big_int
(** Convert a native integer to a big integer. *)
val big_int_of_int64 : int64 -> big_int
(** Convert a 64-bit integer to a big integer. *)
val int32_of_big_int : big_int -> int32
(** Convert a big integer to a 32-bit integer.
    @raise Failure if the big integer is outside the
    range [[-2{^31}, 2{^31}-1]]. *)
val nativeint_of_big_int : big_int -> nativeint
(** Convert a big integer to a native integer.
    @raise Failure if the big integer is outside the
    range [[Nativeint.min_int, Nativeint.max_int]]. *)
val int64_of_big_int : big_int -> int64
(** Convert a big integer to a 64-bit integer.
    @raise Failure if the big integer is outside the
    range [[-2{^63}, 2{^63}-1]]. *)

val float_of_big_int : big_int -> float
(** Returns a floating-point number approximating the
    given big integer. *)

val of_float: float -> big_int
(** rounds to the nearest integer
    @raise Invalid_argument when given NaN or +/-infinity *)

val to_float: big_int -> float


(** {6 Bit-oriented operations} *)

val and_big_int : big_int -> big_int -> big_int
(** Bitwise logical ``and''.
    The arguments must be positive or zero. *)
val or_big_int : big_int -> big_int -> big_int
(** Bitwise logical ``or''.
    The arguments must be positive or zero. *)
val xor_big_int : big_int -> big_int -> big_int
(** Bitwise logical ``exclusive or''.
    The arguments must be positive or zero. *)
val shift_left_big_int : big_int -> int -> big_int
(** [shift_left_big_int b n] returns [b] shifted left by [n] bits.
    Equivalent to multiplication by [2^n]. *)
val shift_right_big_int : big_int -> int -> big_int
(** [shift_right_big_int b n] returns [b] shifted right by [n] bits.
    Equivalent to division by [2^n] with the result being
    rounded towards minus infinity. *)
val shift_right_towards_zero_big_int : big_int -> int -> big_int
(** [shift_right_towards_zero_big_int b n] returns [b] shifted
    right by [n] bits.  The shift is performed on the absolute
    value of [b], and the result has the same sign as [b].
    Equivalent to division by [2^n] with the result being
    rounded towards zero. *)
val extract_big_int : big_int -> int -> int -> big_int
(** [extract_big_int bi ofs n] returns a nonnegative number
    corresponding to bits [ofs] to [ofs + n - 1] of the
    binary representation of [bi].  If [bi] is negative,
    a two's complement representation is used. *)


(** {6 Submodules grouping all infix operators}  *)

module Infix : BatNumber.Infix with type bat__infix_t = t
module Compare : BatNumber.Compare with type bat__compare_t = t

(**/**)

(** {6 For internal use} *)
val nat_of_big_int : big_int -> Nat.nat
val big_int_of_nat : Nat.nat -> big_int
val base_power_big_int: int -> int -> big_int -> big_int
val sys_big_int_of_string: string -> int -> int -> big_int
val round_futur_last_digit : string -> int -> int -> bool
val approx_big_int: int -> big_int -> string

(** {6 Obsolete}*)
val zero_big_int : big_int
(** The big integer [0]. *)
val unit_big_int : big_int
(** The big integer [1]. *)
val minus_big_int : big_int -> big_int
(** Unary negation. *)
val abs_big_int : big_int -> big_int
(** Absolute value. *)
val add_big_int : big_int -> big_int -> big_int
val succ_big_int : big_int -> big_int
(** Successor (add 1). *)
val sub_big_int : big_int -> big_int -> big_int
(** Subtraction. *)
val pred_big_int : big_int -> big_int
(** Predecessor (subtract 1). *)
val mult_big_int : big_int -> big_int -> big_int
(** Multiplication of two big integers. *)
val mult_int_big_int : int -> big_int -> big_int
(** Multiplication of a big integer by a small integer *)
val div_big_int : big_int -> big_int -> big_int
(** Euclidean quotient of two big integers.
    This is the first result [q] of [quomod_big_int] (see above). *)
val mod_big_int : big_int -> big_int -> big_int
(** Euclidean modulus of two big integers.
    This is the second result [r] of [quomod_big_int] (see above). *)
val gcd_big_int : big_int -> big_int -> big_int
(** Greatest common divisor of two big integers. *)
(**/**)

(** {6 Boilerplate code} *)

(** {7 Printing} *)

val print : 'a BatIO.output -> t -> unit
