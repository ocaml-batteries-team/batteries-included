(*
 * BatInt64 - Extended 64-bit integers
 * Copyright (C) 2005 Damien Doligez
 *               2007 Bluestorm <bluestorm dot dylc on-the-server gmail dot com>
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


(** 64-bit integers.

    This module provides operations on the type [int64]
    of signed 64-bit integers.  Unlike the built-in [int] type,
    the type [int64] is guaranteed to be exactly 64-bit wide on all
    platforms.  All arithmetic operations over [int64] are taken
    modulo 2{^64}.

    Performance notice: values of type [int64] occupy more memory
    space than values of type [int], and arithmetic operations on
    [int64] are generally slower than those on [int].  Use [int64]
    only when the application requires exact 64-bit arithmetic.

    Any integer literal followed by [L] is taken to be an [int64].
    For instance, [1L] is {!Int64.one}.


    This module extends Stdlib's
    {{:http://caml.inria.fr/pub/docs/manual-ocaml/libref/Int64.html}Int64}
    module, go there for documentation on the rest of the functions
    and types.

    @author Xavier Leroy (base module)
    @author Gabriel Scherer
    @author David Teller
*)

type t = int64

val zero : int64
(** The 64-bit integer 0. *)

val one : int64
(** The 64-bit integer 1. *)

val minus_one : int64
(** The 64-bit integer -1. *)

external neg : int64 -> int64 = "%int64_neg"
(** Unary negation. *)

external add : int64 -> int64 -> int64 = "%int64_add"
(** Addition. *)

external sub : int64 -> int64 -> int64 = "%int64_sub"
(** Subtraction. *)

external mul : int64 -> int64 -> int64 = "%int64_mul"
(** Multiplication. *)

external div : int64 -> int64 -> int64 = "%int64_div"
(** Integer division.
    This division rounds the real quotient of
    its arguments towards zero, as specified for {!Pervasives.(/)}.
    @raise Division_by_zero if the second argument is zero. *)

external rem : int64 -> int64 -> int64 = "%int64_mod"
(** Integer remainder.  If [y] is not zero, the result
    of [Int64.rem x y] satisfies the following property:
    [x = Int64.add (Int64.mul (Int64.div x y) y) (Int64.rem x y)].
    @raise Division_by_zero if the second argument is zero. *)

val succ : int64 -> int64
(** Successor.  [Int64.succ x] is [Int64.add x Int64.one]. *)

val pred : int64 -> int64
(** Predecessor.  [Int64.pred x] is [Int64.sub x Int64.one]. *)

val abs : int64 -> int64
(** Return the absolute value of its argument. *)

val max_int : int64
(** The greatest representable 64-bit integer, 2{^63} - 1. *)

val min_int : int64
(** The smallest representable 64-bit integer, -2{^63}. *)

external logand : int64 -> int64 -> int64 = "%int64_and"
(** Bitwise logical and. *)

external logor : int64 -> int64 -> int64 = "%int64_or"
(** Bitwise logical or. *)

external logxor : int64 -> int64 -> int64 = "%int64_xor"
(** Bitwise logical exclusive or. *)

val lognot : int64 -> int64
(** Bitwise logical negation *)

external shift_left : int64 -> int -> int64 = "%int64_lsl"
(** [Int64.shift_left x y] shifts [x] to the left by [y] bits.
    The result is unspecified if [y < 0] or [y >= 64]. *)

external shift_right : int64 -> int -> int64 = "%int64_asr"
(** [Int64.shift_right x y] shifts [x] to the right by [y] bits.
    This is an arithmetic shift: the sign bit of [x] is replicated
    and inserted in the vacated bits.
    The result is unspecified if [y < 0] or [y >= 64]. *)

external shift_right_logical : int64 -> int -> int64 = "%int64_lsr"
(** [Int64.shift_right_logical x y] shifts [x] to the right by [y] bits.
    This is a logical shift: zeroes are inserted in the vacated bits
    regardless of the sign of [x].
    The result is unspecified if [y < 0] or [y >= 64]. *)

val ( -- ) : t -> t -> t BatEnum.t
(** Enumerate an interval.

    [5L -- 10L] is the enumeration 5L,6L,7L,8L,9L,10L.
    [10L -- 5L] is the empty enumeration*)

val ( --- ) : t -> t -> t BatEnum.t
(** Enumerate an interval.

    [5L -- 10L] is the enumeration 5L,6L,7L,8L,9L,10L.
    [10L -- 5L] is the enumeration 10L,9L,8L,7L,6L,5L.*)

external of_int : int -> int64 = "%int64_of_int"
(** Convert the given integer (type [int]) to a 64-bit integer
    (type [int64]). *)

external to_int : int64 -> int = "%int64_to_int"
(** Convert the given 64-bit integer (type [int64]) to an
    integer (type [int]).  On 64-bit platforms, the 64-bit integer
    is taken modulo 2{^63}, i.e. the high-order bit is lost
    during the conversion.  On 32-bit platforms, the 64-bit integer
    is taken modulo 2{^31}, i.e. the top 33 bits are lost
    during the conversion. *)

external of_float : float -> int64 = "caml_int64_of_float"
(** Convert the given floating-point number to a 64-bit integer,
    discarding the fractional part (truncate towards 0).
    The result of the conversion is undefined if, after truncation,
    the number is outside the range \[{!Int64.min_int}, {!Int64.max_int}\]. *)

external to_float : int64 -> float = "caml_int64_to_float"
(** Convert the given 64-bit integer to a floating-point number. *)


external of_int32 : int32 -> int64 = "%int64_of_int32"
(** Convert the given 32-bit integer (type [int32])
    to a 64-bit integer (type [int64]). *)

external to_int32 : int64 -> int32 = "%int64_to_int32"
(** Convert the given 64-bit integer (type [int64]) to a
    32-bit integer (type [int32]). The 64-bit integer
    is taken modulo 2{^32}, i.e. the top 32 bits are lost
    during the conversion.  *)

external of_nativeint : nativeint -> int64 = "%int64_of_nativeint"
(** Convert the given native integer (type [nativeint])
    to a 64-bit integer (type [int64]). *)

external to_nativeint : int64 -> nativeint = "%int64_to_nativeint"
(** Convert the given 64-bit integer (type [int64]) to a
    native integer.  On 32-bit platforms, the 64-bit integer
    is taken modulo 2{^32}.  On 64-bit platforms,
    the conversion is exact. *)

external of_string : string -> int64 = "caml_int64_of_string"
(** Convert the given string to a 64-bit integer.
    The string is read in decimal (by default) or in hexadecimal,
    octal or binary if the string begins with [0x], [0o] or [0b]
    respectively.
    @raise Failure if the given string is not
    a valid representation of an integer, or if the integer represented
    exceeds the range of integers representable in type [int64]. *)

val to_string : int64 -> string
(** Return the string representation of its argument, in decimal. *)

external bits_of_float : float -> int64 = "caml_int64_bits_of_float"
(** Return the internal representation of the given float according
    to the IEEE 754 floating-point ``double format'' bit layout.
    Bit 63 of the result represents the sign of the float;
    bits 62 to 52 represent the (biased) exponent; bits 51 to 0
    represent the mantissa. *)

external float_of_bits : int64 -> float = "caml_int64_float_of_bits"
(** Return the floating-point number whose internal representation,
    according to the IEEE 754 floating-point ``double format'' bit layout,
    is the given [int64]. *)



val compare : t -> t -> int
(** The comparison function for 64-bit integers, with the same specification as
    {!Pervasives.compare}.  Along with the type [t], this function [compare]
    allows the module [Int64] to be passed as argument to the functors
    {!Set.Make} and {!Map.Make}. *)

val equal : t -> t -> bool
(** Equality function for 64-bit integers, useful for {!HashedType}. *)

val ord : t -> t -> BatOrd.order

(** {6 Submodules grouping all infix operators} *)

module Infix : BatNumber.Infix with type bat__infix_t = t
module Compare: BatNumber.Compare with type bat__compare_t = t

(**/**)

(** {6 Deprecated functions} *)

external format : string -> int64 -> string = "caml_int64_format"
(** [Int64.format fmt n] return the string representation of the
    64-bit integer [n] in the format specified by [fmt].
    [fmt] is a {!Printf}-style format consisting of exactly one
    [%d], [%i], [%u], [%x], [%X] or [%o] conversion specification.
    This function is deprecated; use {!Printf.sprintf} with a [%Lx] format
    instead. *)

(**/**)
val modulo : int64 -> int64 -> int64
val pow : int64 -> int64 -> int64

val ( + ) : t -> t -> t
val ( - ) : t -> t -> t
val ( * ) : t -> t -> t
val ( / ) : t -> t -> t
val ( ** ) : t -> t -> t
(* Available only in `Compare` submodule
   val ( <> ) : t -> t -> bool
   val ( >= ) : t -> t -> bool
   val ( <= ) : t -> t -> bool
   val ( > ) : t -> t -> bool
   val ( < ) : t -> t -> bool
   val ( = ) : t -> t -> bool
*)
val operations : t BatNumber.numeric

(** {6 Boilerplate code}*)

(** {7 Printing}*)

val print: 'a BatInnerIO.output -> t -> unit
(** prints as decimal string *)

val print_hex: 'a BatInnerIO.output -> t -> unit
  (** prints as hex string *)
