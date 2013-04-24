(*
 * BatNativeint - Extended native ints
 * Copyright (C) 2005 Xavier Leroy
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

(** Processor-native integers.

    This module provides operations on the type [nativeint] of
    signed 32-bit integers (on 32-bit platforms) or
    signed 64-bit integers (on 64-bit platforms).
    This integer type has exactly the same width as that of a [long]
    integer type in the C compiler.  All arithmetic operations over
    [nativeint] are taken modulo 2{^32} or 2{^64} depending
    on the word size of the architecture.

    Performance notice: values of type [nativeint] occupy more memory
    space than values of type [int], and arithmetic operations on
    [nativeint] are generally slower than those on [int].  Use [nativeint]
    only when the application requires the extra bit of precision
    over the [int] type.

    Any integer literal followed by [n] is taken to be a [nativeint].
    For instance, [1n] is {!Native_int.one}.

    This module extends Stdlib's
    {{:http://caml.inria.fr/pub/docs/manual-ocaml/libref/Nativeint.html}Nativeint}
    module, go there for documentation on the rest of the functions
    and types.

    @author Xavier Leroy (base module)
    @author Gabriel Scherer
    @author David Teller
*)

type t = nativeint
(** An alias for the type of native integers. *)

val zero : nativeint
(** The native integer 0.*)

val one : nativeint
(** The native integer 1.*)

val minus_one : nativeint
(** The native integer -1.*)

external neg : nativeint -> nativeint = "%nativeint_neg"
(** Unary negation. *)

external add : nativeint -> nativeint -> nativeint = "%nativeint_add"
(** Addition. *)

external sub : nativeint -> nativeint -> nativeint = "%nativeint_sub"
(** Subtraction. *)

external mul : nativeint -> nativeint -> nativeint = "%nativeint_mul"
(** Multiplication. *)

external div : nativeint -> nativeint -> nativeint = "%nativeint_div"
(** Integer division. @raise Division_by_zero if the second
    argument is zero.  This division rounds the real quotient of
    its arguments towards zero, as specified for {!Pervasives.(/)}. *)

external rem : nativeint -> nativeint -> nativeint = "%nativeint_mod"
(** Integer remainder.  If [y] is not zero, the result
    of [Nativeint.rem x y] satisfies the following properties:
    [Nativeint.zero <= Nativeint.rem x y < Nativeint.abs y] and
    [x = Nativeint.add (Nativeint.mul (Nativeint.div x y) y) (Nativeint.rem x y)].
    If [y = 0], [Nativeint.rem x y] raises [Division_by_zero]. *)

val succ : nativeint -> nativeint
(** Successor.
    [Nativeint.succ x] is [Nativeint.add x Nativeint.one]. *)

val pred : nativeint -> nativeint
(** Predecessor.
    [Nativeint.pred x] is [Nativeint.sub x Nativeint.one]. *)

val abs : nativeint -> nativeint
(** Return the absolute value of its argument. *)

val size : int
(** The size in bits of a native integer.  This is equal to [32]
    on a 32-bit platform and to [64] on a 64-bit platform. *)

val max_int : nativeint
(** The greatest representable native integer,
    either 2{^31} - 1 on a 32-bit platform,
    or 2{^63} - 1 on a 64-bit platform. *)

val min_int : nativeint
(** The greatest representable native integer,
    either -2{^31} on a 32-bit platform,
    or -2{^63} on a 64-bit platform. *)

external logand : nativeint -> nativeint -> nativeint = "%nativeint_and"
(** Bitwise logical and. *)

external logor : nativeint -> nativeint -> nativeint = "%nativeint_or"
(** Bitwise logical or. *)

external logxor : nativeint -> nativeint -> nativeint = "%nativeint_xor"
(** Bitwise logical exclusive or. *)

val lognot : nativeint -> nativeint
(** Bitwise logical negation *)

external shift_left : nativeint -> int -> nativeint = "%nativeint_lsl"
(** [Nativeint.shift_left x y] shifts [x] to the left by [y] bits.
    The result is unspecified if [y < 0] or [y >= bitsize],
    where [bitsize] is [32] on a 32-bit platform and
    [64] on a 64-bit platform. *)

external shift_right : nativeint -> int -> nativeint = "%nativeint_asr"
(** [Nativeint.shift_right x y] shifts [x] to the right by [y] bits.
    This is an arithmetic shift: the sign bit of [x] is replicated
    and inserted in the vacated bits.
    The result is unspecified if [y < 0] or [y >= bitsize]. *)

external shift_right_logical :
  nativeint -> int -> nativeint = "%nativeint_lsr"
(** [Nativeint.shift_right_logical x y] shifts [x] to the right
    by [y] bits.
    This is a logical shift: zeroes are inserted in the vacated bits
    regardless of the sign of [x].
    The result is unspecified if [y < 0] or [y >= bitsize]. *)

val ( -- ) : t -> t -> t BatEnum.t
(** Enumerate an interval.

    [5n -- 10n] is the enumeration 5n,6n,7n,8n,9n,10n.
    [10n -- 5n] is the empty enumeration*)

val ( --- ) : t -> t -> t BatEnum.t
(** Enumerate an interval.

    [5n -- 10n] is the enumeration 5n,6n,7n,8n,9n,10n.
    [10n -- 5n] is the enumeration 10n,9n,8n,7n,6n,5n.*)

external of_int : int -> nativeint = "%nativeint_of_int"
(** Convert the given integer (type [int]) to a native integer
    (type [nativeint]). *)

external to_int : nativeint -> int = "%nativeint_to_int"
(** Convert the given native integer (type [nativeint]) to an
    integer (type [int]).  The high-order bit is lost during
    the conversion. *)

external of_float : float -> nativeint = "caml_nativeint_of_float"
(** Convert the given floating-point number to a native integer,
    discarding the fractional part (truncate towards 0).
    The result of the conversion is undefined if, after truncation,
    the number is outside the range
    \[{!Nativeint.min_int}, {!Nativeint.max_int}\]. *)

external to_float : nativeint -> float = "caml_nativeint_to_float"
(** Convert the given native integer to a floating-point number. *)

external of_int32 : int32 -> nativeint = "%nativeint_of_int32"
(** Convert the given 32-bit integer (type [int32])
    to a native integer. *)

external to_int32 : nativeint -> int32 = "%nativeint_to_int32"
(** Convert the given native integer to a
    32-bit integer (type [int32]).  On 64-bit platforms,
    the 64-bit native integer is taken modulo 2{^32},
    i.e. the top 32 bits are lost.  On 32-bit platforms,
    the conversion is exact. *)

external of_int64 : int64 -> nativeint = "%int64_to_nativeint"
(** Convert the given 64-bit integer (type [int64])
    to a native integer. On 32-bit platforms, the top
    32 bits are lost. *)

external to_int64 : nativeint -> int64 = "%int64_of_nativeint"
(** Convert the given native integer to a
    64-bit integer (type [int64]). *)

external of_string : string -> nativeint = "caml_nativeint_of_string"
(** Convert the given string to a native integer.
    The string is read in decimal (by default) or in hexadecimal,
    octal or binary if the string begins with [0x], [0o] or [0b]
    respectively.
    @raise Failure if the given string is not
    a valid representation of an integer, or if the integer represented
    exceeds the range of integers representable in type [nativeint]. *)

val to_string : nativeint -> string
(** Return the string representation of its argument, in decimal. *)



val compare : t -> t -> int
(** The comparison function for native integers, with the same specification as
    {!Pervasives.compare}.  Along with the type [t], this function [compare]
    allows the module [Nativeint] to be passed as argument to the functors
    {!Set.Make} and {!Map.Make}. *)

val equal : t -> t -> bool
(** Equality function for 64-bit integers, useful for {!HashedType}. *)

val ord : t -> t -> BatOrd.order

val modulo : nativeint -> nativeint -> nativeint
val pow : nativeint -> nativeint -> nativeint
val min_num : nativeint
val max_num : nativeint

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

(** {6 Submodules grouping all infix operators} *)

module Infix : BatNumber.Infix with type bat__infix_t = t
module Compare : BatNumber.Compare with type bat__compare_t = t


(** {6 Boilerplate code}*)

(** {7 Printing}*)

val print : (t,_) BatIO.printer

(**/**)

(** {6 Deprecated functions} *)

external format : string -> nativeint -> string = "caml_nativeint_format"
(** [Nativeint.format fmt n] return the string representation of the
    native integer [n] in the format specified by [fmt].
    [fmt] is a [Printf]-style format consisting of exactly
    one [%d], [%i], [%u], [%x], [%X] or [%o] conversion specification.
    @deprecated use {!Printf.sprintf} with a [%nx] format
    instead. *)
(**/**)
