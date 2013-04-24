(*
 * BatInt32 - Extended 32-bit integers
 * Copyright (C) 1996 Xavier Leroy
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

(** 32-bit integers.

    This module provides operations on the type [int32]
    of signed 32-bit integers.  Unlike the built-in [int] type,
    the type [int32] is guaranteed to be exactly 32-bit wide on all
    platforms.  All arithmetic operations over [int32] are taken
    modulo 2{^32}.

    Any integer literal followed by [l] is taken to be an [int32].
    For instance, [1l] is {!Int32.one}.

    Performance notice: values of type [int32] occupy more memory
    space than values of type [int], and arithmetic operations on
    [int32] are generally slower than those on [int].  Use [int32]
    only when the application requires exact 32-bit arithmetic.


    This module extends Stdlib's
    {{:http://caml.inria.fr/pub/docs/manual-ocaml/libref/Int32.html}Int32}
    module, go there for documentation on the rest of the functions
    and types.

    @author Xavier Leroy (base module)
    @author Gabriel Scherer
    @author David Teller
*)
type t = int32

val zero : int32
(** The 32-bit integer 0. *)

val one : int32
(** The 32-bit integer 1. *)

val minus_one : int32
(** The 32-bit integer -1. *)

external neg : int32 -> int32 = "%int32_neg"
(** Unary negation. *)

external add : int32 -> int32 -> int32 = "%int32_add"
(** Addition. *)

external sub : int32 -> int32 -> int32 = "%int32_sub"
(** Subtraction. *)

external mul : int32 -> int32 -> int32 = "%int32_mul"
(** Multiplication. *)

external div : int32 -> int32 -> int32 = "%int32_div"
(** Integer division.
    This division rounds the real quotient of
    its arguments towards zero, as specified for {!Pervasives.(/)}.
    @raise Division_by_zero if the second argument is zero. *)

external rem : int32 -> int32 -> int32 = "%int32_mod"
(** Integer remainder.  If [y] is not zero, the result
    of [Int32.rem x y] satisfies the following property:
    [x = Int32.add (Int32.mul (Int32.div x y) y) (Int32.rem x y)].
    @raise Division_by_zero if the second argument is zero. *)


val modulo : int32 -> int32 -> int32
val pow  : int32 -> int32 -> int32
val min_num : int32
val max_num : int32

val succ : int32 -> int32
(** Successor.  [Int32.succ x] is [Int32.add x Int32.one]. *)

val pred : int32 -> int32
(** Predecessor.  [Int32.pred x] is [Int32.sub x Int32.one]. *)

val abs : int32 -> int32
(** Return the absolute value of its argument. *)

val max_int : int32
(** The greatest representable 32-bit integer, 2{^31} - 1. *)

val min_int : int32
(** The smallest representable 32-bit integer, -2{^31}. *)


external logand : int32 -> int32 -> int32 = "%int32_and"
(** Bitwise logical and. *)

external logor : int32 -> int32 -> int32 = "%int32_or"
(** Bitwise logical or. *)

external logxor : int32 -> int32 -> int32 = "%int32_xor"
(** Bitwise logical exclusive or. *)

val lognot : int32 -> int32
(** Bitwise logical negation *)

external shift_left : int32 -> int -> int32 = "%int32_lsl"
(** [Int32.shift_left x y] shifts [x] to the left by [y] bits.
    The result is unspecified if [y < 0] or [y >= 32]. *)

external shift_right : int32 -> int -> int32 = "%int32_asr"
(** [Int32.shift_right x y] shifts [x] to the right by [y] bits.
    This is an arithmetic shift: the sign bit of [x] is replicated
    and inserted in the vacated bits.
    The result is unspecified if [y < 0] or [y >= 32]. *)

external shift_right_logical : int32 -> int -> int32 = "%int32_lsr"
(** [Int32.shift_right_logical x y] shifts [x] to the right by [y] bits.
    This is a logical shift: zeroes are inserted in the vacated bits
    regardless of the sign of [x].
    The result is unspecified if [y < 0] or [y >= 32]. *)

val ( -- ) : t -> t -> t BatEnum.t
(** Enumerate an interval.

    [5l -- 10l] is the enumeration 5l,6l,7l,8l,9l,10l.
    [10l -- 5l] is the empty enumeration*)

val ( --- ) : t -> t -> t BatEnum.t
(** Enumerate an interval.

    [5l -- 10l] is the enumeration 5l,6l,7l,8l,9l,10l.
    [10l -- 5l] is the enumeration 10l,9l,8l,7l,6l,5l.*)

external of_int : int -> int32 = "%int32_of_int"
(** Convert the given integer (type [int]) to a 32-bit integer
    (type [int32]). *)

external to_int : int32 -> int = "%int32_to_int"
(** Convert the given 32-bit integer (type [int32]) to an
    integer (type [int]).  On 32-bit platforms, the 32-bit integer
    is taken modulo 2{^31}, i.e. the high-order bit is lost
    during the conversion.  On 64-bit platforms, the conversion
    is exact. *)

external of_float : float -> int32 = "caml_int32_of_float"
(** Convert the given floating-point number to a 32-bit integer,
    discarding the fractional part (truncate towards 0).
    The result of the conversion is undefined if, after truncation,
    the number is outside the range \[{!Int32.min_int}, {!Int32.max_int}\]. *)

external to_float : int32 -> float = "caml_int32_to_float"
(** Convert the given 32-bit integer to a floating-point number. *)

external of_int64 : int64 -> int32 = "%int64_to_int32"
(** Convert the given 64-bit integer (type [int64]) to a
    32-bit integer (type [int32]). The 64-bit integer
    is taken modulo 2{^32}, i.e. the top 32 bits are lost
    during the conversion.  *)

external to_int64 : int32 -> int64 = "%int64_of_int32"
(** Convert the given 32-bit integer (type [int32])
    to a 64-bit integer (type [int64]). *)

external of_nativeint : nativeint -> int32 = "%int32_of_nativeint"
(** Convert the given native integer (type [nativeint])
    to a 32-bit integer (type [int32]). On 64-bits platform
    the top 32 bits are lost. *)

external to_nativeint : int32 -> nativeint = "%int32_to_nativeint"
(** Convert the given 32-bit integer (type [int32]) to a
    native integer. *)

external of_string : string -> int32 = "caml_int32_of_string"
(** Convert the given string to a 32-bit integer.
    The string is read in decimal (by default) or in hexadecimal,
    octal or binary if the string begins with [0x], [0o] or [0b]
    respectively.
    @raise Failure if the given string is not
    a valid representation of an integer, or if the integer represented
    exceeds the range of integers representable in type [int32]. *)

val to_string : int32 -> string
(** Return the string representation of its argument, in signed decimal. *)

external bits_of_float : float -> int32 = "caml_int32_bits_of_float"
(** Return the internal representation of the given float according
    to the IEEE 754 floating-point ``single format'' bit layout.
    Bit 31 of the result represents the sign of the float;
    bits 30 to 23 represent the (biased) exponent; bits 22 to 0
    represent the mantissa. *)

external float_of_bits : int32 -> float = "caml_int32_float_of_bits"
(** Return the floating-point number whose internal representation,
    according to the IEEE 754 floating-point ``single format'' bit layout,
    is the given [int32]. *)

val of_byte : char -> int32
val to_byte : int32 -> char

val pack : string -> int -> int32 -> unit
(** [pack str off i] writes the little endian bit representation
    of [i] into string [str] at offset [off] *)

val pack_big : string -> int -> int32 -> unit
(** [pack_big str off i] writes the big endian bit
    representation of [i] into string [str] at offset [off] *)

val unpack : string -> int -> int32
(** [unpack str off] reads 4 bytes from string [str] starting at
    offset [off] as a little-endian int32 *)

val unpack_big : string -> int -> int32
(** [unpack str off] reads 4 bytes from string [str] starting at
    offset [off] as a big-endian int32 *)

val compare : t -> t -> int
(** The comparison function for 32-bit integers, with the same specification as
    {!Pervasives.compare}.  Along with the type [t], this function [compare]
    allows the module [Int32] to be passed as argument to the functors
    {!Set.Make} and {!Map.Make}. *)

val equal : t -> t -> bool
(** Equality function for 32-bit integers, useful for {!HashedType}. *)

val ord : t -> t -> BatOrd.order

(**/**)

(** {6 Deprecated functions} *)

external format : string -> int32 -> string = "caml_int32_format"
(** [Int32.format fmt n] return the string representation of the
    32-bit integer [n] in the format specified by [fmt].
    [fmt] is a [Printf]-style format consisting of exactly
    one [%d], [%i], [%u], [%x], [%X] or [%o] conversion specification.
    This function is deprecated; use {!Printf.sprintf} with a [%lx] format
    instead. *)

val ( + ) : t -> t -> t
val ( - ) : t -> t -> t
val ( * ) : t -> t -> t
val ( / ) : t -> t -> t
val ( ** ) : t -> t -> t
(* Available only in `Compare` submodule, as they override the polymorphic compare
   val ( <> ) : t -> t -> bool
   val ( >= ) : t -> t -> bool
   val ( <= ) : t -> t -> bool
   val ( > ) : t -> t -> bool
   val ( < ) : t -> t -> bool
   val ( = ) : t -> t -> bool
*)
val operations : t BatNumber.numeric

(** {6 Submodules grouping all infix operators} *)

module Infix : BatNumber.Infix with type bat__infix_t = t
module Compare : BatNumber.Compare with type bat__compare_t = t


(** {6 Boilerplate code}*)

(** {7 Printing}*)

val print: 'a BatInnerIO.output -> t -> unit
(** prints as decimal string *)

val print_hex: 'a BatInnerIO.output -> t -> unit
  (** prints as hex string *)
