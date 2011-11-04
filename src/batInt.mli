(* 
 * ExtInt - Extended operations on integers
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


(** 
    Operations on integers.
    
    This module provides operations on the type [int] of
    integers. Values of this type may be either 31 bits on 32-bit
    processors or 63 bits on 64-bit processors. All arithmetic
    operations over [int] are taken modulo 2{^number of bits}.
    
    This module implements {!Number.Numeric},
    {!Number.Bounded}, {!Number.Discrete}.
    
    @author Gabriel Scherer
    @author David Teller

    @documents Int
*)
    
    type t = int
	(** An alias for the type of integers. *)

    val zero : int
      (** The integer [0]. *)
      
    val one : int
      (** The integer [1]. *)

    val minus_one : int
      (** The integer [-1]. *)

    external neg : int -> int = "%negint"
      (** Unary negation. *)

    external add : int -> int -> int = "%addint"
      (** Addition. *)
    external ( + ) : int -> int -> int = "%addint"
      (** Addition. *)

    external sub : int -> int -> int = "%subint"
      (** Subtraction. *)
    external ( - ) : int -> int -> int = "%subint"
      (** Subtraction. *)

    external mul : int -> int -> int = "%mulint"
      (** Multiplication. *)
    external ( * ) : int -> int -> int = "%mulint"
      (** Multiplication. *)

    external div : int -> int -> int = "%divint"
      (** Integer division.  Raise [Division_by_zero] if the second
	  argument is zero.  This division rounds the real quotient of
	  its arguments towards zero, as specified for {!Pervasives.(/)}. *)
    external ( / ) : int -> int -> int = "%divint"
      (** Integer division.  Raise [Division_by_zero] if the second
	  argument is zero.  This division rounds the real quotient of
	  its arguments towards zero, as specified for {!Pervasives.(/)}. *)
      
    external rem : int -> int -> int = "%modint"
      (** Integer remainder.  If [y] is not zero, the result
	  of [Int.rem x y] satisfies the following property:
	  [x = Int.add (Int.mul (Int.div x y) y) (Int.rem x y)].
	  If [y = 0], [Int.rem x y] raises [Division_by_zero]. *)
      
    external modulo : int -> int -> int = "%modint"
      (** [modulo a b] computes the remainder of the integer
	  division of [a] by [b]. This is defined only if [b <> 0].

	  The result of [modulo a b] is a number [m] between
	  [0] and [abs ( b - 1 )] if [a >= 0] or between [~- ( abs ( b - 1 ) ) ]
	  if [a < 0] and such that [a * k + (abs b) = m],
	  for some [k]. *)


    val pow  : int -> int -> int
      (** [pow a b] computes a{^b}*)
    val ( ** ) : int -> int -> int
      (** [a ** b] computes a{^b}*)

    val ( <> ) : int -> int -> bool
    val ( > )  : int -> int -> bool
    val ( < )  : int -> int -> bool
    val ( >= ) : int -> int -> bool
    val ( <= ) : int -> int -> bool
    val ( = )  : int -> int -> bool

    val min_num : int
      (** The greatest representable integer, which is either 2{^30}-1 or 2{^62}-1. *)

    val max_num : int
      (** The smallest representable integer, -2{^30} or 2{^62}. *)

    external succ: int -> int  = "%succint"
      (** Successor.  [Int.succ x] is [Int.add x Int.one]. *)
      
    external pred: int -> int  = "%predint"
      (** Predecessor.  [Int.pred x] is [Int.sub x Int.one]. *)
      
    val abs : int -> int
      (** Return the absolute value of its argument. *)
      
    external of_float : float -> int = "%intoffloat"
      (** Convert the given floating-point number to integer integer,
	  discarding the fractional part (truncate towards 0).
	  The result of the conversion is undefined if, after truncation,
	  the number is outside the range \[{!Int.min_int}, {!Int.max_int}\]. *)

    external to_float : int -> float = "%floatofint"
      (** Convert the given integer to a floating-point number. *)
      
    val of_string : string -> int
      (** Convert the given string to an integer
	  The string is read in decimal (by default) or in hexadecimal,
	  octal or binary if the string begins with [0x], [0o] or [0b]
	  respectively.
	  Raise [Invalid_argument "int_of_string"] if the given string is not
	  a valid representation of an integer, or if the integer represented
	  exceeds the range of integers representable in type [int]. *)
      
    val to_string : int -> string
      (** Return the string representation of its argument, in signed decimal. *)
      
	
    val compare: t -> t -> int
      (** The comparison function for integers, with the same specification as
	  {!Pervasives.compare}.  Along with the type [t], this function [compare]
	  allows the module [Int] to be passed as argument to the functors
	  {!Set.Make} and {!Map.Make}. *)
      
    val operations : int BatNumber.numeric

    val ( -- ) : t -> t -> t BatEnum.t
      (** Enumerate an interval.
	  
	  [5 -- 10] is the enumeration 5,6,7,8,9,10.
	  [10 -- 5] is the empty enumeration*)

    val ( --- ) : t -> t -> t BatEnum.t
      (** Enumerate an interval.
	  
	  [5 -- 10] is the enumeration 5,6,7,8,9,10.
	  [10 -- 5] is the enumeration 10,9,8,7,6,5.*)


    external of_int : int -> int = "%identity"
    external to_int : int -> int = "%identity"

    (** {6 Submodules regrouping all infix operations} *)

    module Infix : BatNumber.Infix with type bat__infix_t = t
    module Compare : BatNumber.Compare with type bat__compare_t = t

    (** {6 Boilerplate code}*)

    (** {7 Printing}*)

    val print: 'a BatInnerIO.output -> int -> unit
    val xprint: 'a BatInnerIO.output -> int -> unit
      (** prints as hex string *)
(*    val bprint: 'a BatInnerIO.output -> t -> unit
      (** prints as binary string *) *)
    val t_printer : t BatValue_printer.t



(** 
    Safe operations on integers.
    
    This module provides operations on the type [int] of
    integers. Values of this type may be either 31 bits on 32-bit
    processors or 63 bits on 64-bit processors. Operations which
    overflow raise exception {!Number.Overflow}.
    
    This module implements {!Number.Numeric},
    {!Number.Bounded}, {!Number.Discrete}.
    
    {b Important note} Untested.
*)
module Safe_int :
  sig
    
    type t = int
	(** An alias for the type of integers. *)

    val zero : t
      (** The integer [0]. *)
      
    val one : t
      (** The integer [1]. *)

    val minus_one : t
      (** The integer [-1]. *)

    val neg : t -> t
      (** Unary negation. *)

    val add : t -> t -> t
      (** Addition. *)

    val ( + ) : t -> t -> t
      (** Addition. *)

    val sub : t -> t -> t
      (** Substraction. *)

    val ( - ) : t -> t -> t
      (** Substraction. *)

    val mul : t -> t -> t
      (** Multiplication. *)

    val ( * ) : t -> t -> t
      (** Multiplication. *)

    external div : t -> t -> t = "%divint"
      (** Integer division.  Raise [Division_by_zero] if the second
	  argument is zero.  This division rounds the real quotient of
	  its arguments towards zero, as specified for {!Pervasives.(/)}. *)
    external ( / ) : t -> t -> t = "%divint"
      (** Integer division.  Raise [Division_by_zero] if the second
	  argument is zero.  This division rounds the real quotient of
	  its arguments towards zero, as specified for {!Pervasives.(/)}. *)
      
    external rem : t -> t -> t = "%modint"
      (** Integer remainder.  If [y] is not zero, the result
	  of [Int.rem x y] satisfies the following property:
	  [x = Int.add (Int.mul (Int.div x y) y) (Int.rem x y)].
	  If [y = 0], [Int.rem x y] raises [Division_by_zero]. *)
      
    external modulo : t -> t -> t = "%modint"
      (** [modulo a b] computes the remainder of the integer
	  division of [a] by [b]. This is defined only if [b <> 0].

	  The result of [modulo a b] is a number [m] between
	  [0] and [abs ( b - 1 )] if [a >= 0] or between [~- ( abs ( b - 1 ) ) ]
	  if [a < 0] and such that [a * k + (abs b) = m],
	  for some [k]. *)


    val pow  : t -> t -> t
      (** [pow a b] computes a{^b}*)

    val ( ** ) : t -> t -> t
      (** [a ** b] computes a{^b}*)

    val ( <> ) : t -> t -> bool
      (** Comparaison: [a <> b] is true if and only if [a] and [b] have
	  different values. *)

    val ( > )  : t -> t -> bool
      (** Comparaison: [a > b] is true if and only if [a] is strictly greater than [b].*)

    val ( < )  : t -> t -> bool
      (** Comparaison: [a < b] is true if and only if [a] is strictly smaller than [b].*)

    val ( >= ) : t -> t -> bool
      (** Comparaison: [a >= b] is true if and only if [a] is greater or equal to [b].*)

    val ( <= ) : t -> t -> bool
      (** Comparaison: [a <= b] is true if and only if [a] is smaller or equalto [b].*)

    val ( = )  : t -> t -> bool
      (** Comparaison: [a = b] if and only if [a] and [b] have the same value.*)

    val min_num : t
      (** The greatest representable integer, which is either 2{^30}-1 or 2{^62}-1. *)

    val max_num : t
      (** The smallest representable integer, -2{^30} or 2{^62}. *)

    val succ: t -> t
      (** Successor.  [succ x] is [add x one]. *)
      
    val pred: t -> t
      (** Predecessor.  [pred x] is [sub x one]. *)
      
    val abs : t -> t
      (** Return the absolute value of its argument. *)
      
    external of_float : float -> t = "%intoffloat"
      (** Convert the given floating-point number to integer integer,
	  discarding the fractional part (truncate towards 0).
	  The result of the conversion is undefined if, after truncation,
	  the number is outside the range \[{!Int.min_int}, {!Int.max_int}\]. *)

    external to_float : t -> float = "%floatofint"
      (** Convert the given integer to a floating-point number. *)
      
    val of_string : string -> t
      (** Convert the given string to an integer
	  The string is read in decimal (by default) or in hexadecimal,
	  octal or binary if the string begins with [0x], [0o] or [0b]
	  respectively.
	  Raise [Invalid_argument "int_of_string"] if the given string is not
	  a valid representation of an integer, or if the integer represented
	  exceeds the range of integers representable in type [int]. *)
      
    val to_string : t -> string
      (** Return the string representation of its argument, in signed decimal. *)
      
	
    val compare: t -> t -> int
      (** The comparison function for integers, with the same specification as
	  {!Pervasives.compare}.  Along with the type [t], this function [compare]
	  allows the module [Int] to be passed as argument to the functors
	  {!Set.Make} and {!Map.Make}. *)
      
    val operations : t BatNumber.numeric

    external of_int : int -> t = "%identity"
    external to_int : t -> int = "%identity"

    (** {6 Submodules regrouping all infix operations on safe integers} *)

    module Infix : BatNumber.Infix with type bat__infix_t = t
    module Compare : BatNumber.Compare with type bat__compare_t = t

    (** {6 Boilerplate code}*)

    (** {7 Printing}*)

    val print: 'a BatInnerIO.output -> t -> unit
  end

