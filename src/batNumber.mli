(* 
 * Number - Generic interface for numbers
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

(**
   A common interface for numbers.

   @author Gabriel Scherer
   @author David Teller
*)


(**
   Arithmetic overflow.

   This kind of exception is raised by "safe" numeric modules whenever
   the number which should be returned is too large to be represented.

   Non-"safe" numeric modules will return a result which depends on
   the internal representation. For instance, with module {!Int},
   [max_num + 1] returns [min_num]. By opposition, with module
   {!Safe_int}, [max_num + 1] raises [Overflow].
   
*)
exception Overflow

(**
   Not a Number

   This kind of exception is raised by "safe" modules whenever the
   number which should be returned is not a number.

   For instance, with module {!Safe_float}, [0.0 / 0.0] raises [NaN].
   By opposition, with module {!Float}, [0.0 / 0.0] does not interrupt
   computation and returns a special value [nan].
*)
exception NaN

(**
   The smallest set of operations supported by every set of numbers.

   This is presented as record to permit lightweight typeclass-style
   computation.
*)
type 'a numeric =
{
    zero : 'a;
    one : 'a;
    neg : 'a -> 'a;
    succ : 'a -> 'a;
    pred : 'a -> 'a;
    abs : 'a -> 'a;
    add : 'a -> 'a -> 'a;
    sub : 'a -> 'a -> 'a;
    mul : 'a -> 'a -> 'a;
    div : 'a -> 'a -> 'a;
    modulo : 'a -> 'a -> 'a;
    pow : 'a -> 'a -> 'a;
    compare : 'a -> 'a -> int;
    of_int : int -> 'a;
    to_int : 'a -> int;
    of_string : string -> 'a;
    to_string : 'a -> string;
    of_float: float -> 'a;
    to_float: 'a -> float;
}

(**
   The infix operators available with any type of numbers
*)
module type Infix = sig
  type bat__infix_t
  val ( + ) : bat__infix_t -> bat__infix_t -> bat__infix_t
  val ( - ) : bat__infix_t -> bat__infix_t -> bat__infix_t
  val ( * ) : bat__infix_t -> bat__infix_t -> bat__infix_t
  val ( / ) : bat__infix_t -> bat__infix_t -> bat__infix_t
  val ( ** ) : bat__infix_t -> bat__infix_t -> bat__infix_t
  val ( -- ): bat__infix_t -> bat__infix_t -> bat__infix_t BatEnum.t
  val ( --- ): bat__infix_t -> bat__infix_t -> bat__infix_t BatEnum.t
end

(**
   And if you are ready to drop generic comparison operators,
    then you can open this one as well
*)
module type Compare = sig
  type bat__compare_t
  val ( <> ) : bat__compare_t -> bat__compare_t -> bool
  val ( >= ) : bat__compare_t -> bat__compare_t -> bool
  val ( <= ) : bat__compare_t -> bat__compare_t -> bool
  val ( > ) : bat__compare_t -> bat__compare_t -> bool
  val ( < ) : bat__compare_t -> bat__compare_t -> bool
  val ( = ) : bat__compare_t -> bat__compare_t -> bool
end

(**
   The full set of operations of a type of numbers
*)
module type Numeric =
sig  
  type t
  val zero : t
  val one : t
  val neg : t -> t
  val abs : t -> t
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val div : t -> t -> t
  val modulo : t -> t -> t
  val pow : t -> t -> t
  val compare : t -> t -> int
  val of_int : int -> t
  val to_int : t -> int
  val of_float: float -> t
  val to_float: t     -> float
  val of_string : string -> t
  val to_string : t -> string

  val operations : t numeric

  type discrete = t
  (* to_int already provided *)
  val succ : t -> t
  val pred : t -> t

  include Infix with type bat__infix_t = t
  include Compare with type bat__compare_t = t

end

module type Bounded =
sig
  type bounded
  val min_num: bounded
  val max_num: bounded
end

module type Discrete =
sig
  type discrete
  val to_int: discrete -> int
  val succ  : discrete -> discrete
  val pred  : discrete -> discrete
  val ( -- ): discrete -> discrete -> discrete BatEnum.t
  val ( --- ): discrete -> discrete -> discrete BatEnum.t
end

(**/**)

(** {6 Utilities}*)

(**
   The smallest set of operations supported by every set of numbers
*)
module type NUMERIC_BASE =
sig
  type t

  val zero : t
  val one  : t

    (** {6 Arithmetic operations} 
	
	Depending on the implementation, some of these operations
	{i may} raise exceptions at run-time to represent over/under-flows.*)
  val neg : t -> t
  val succ : t -> t
  val pred : t -> t
  val abs : t -> t
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val div : t -> t -> t
  val modulo : t -> t -> t
  val pow : t -> t -> t
  val compare : t -> t -> int

    (** {6 Conversions} *)
  val of_int : int -> t
    (** Convert this number to the closest integer.*)

  val to_int : t -> int
    (** Convert an integer to the closest element of set [t].*)

  val of_string : string -> t
    (** Convert the representation of a number to the corresponding
	number. Raises [Invalid_arg] if the string does not represent
	a valid number of type [t]*)

  val to_string : t -> string

  val of_float : float -> t
  val to_float : t -> float

end

(** Automated definition of infix operators for a given numeric type,
    so that you can open it without poluting your namespace.
	(apart from the type bat__infix_t) *)

module MakeInfix :
  functor (Base : NUMERIC_BASE) -> Infix with type bat__infix_t = Base.t

(** Automated definition of infix comparison operators for a given numeric type,
    so that you can open it only when you mean it.
	(apart from the type bat__compare_t) *)

module MakeCompare :
  functor (Base : NUMERIC_BASE) -> Compare with type bat__compare_t = Base.t

(** Automated definition of operators for a given numeric type.
    You will only need this if you develop your own numeric modules.*)

module MakeNumeric :
  functor (Base : NUMERIC_BASE) -> Numeric with type t = Base.t

(* a generic exponentiation function which efficiently computes a^n as
   the product of repeated squares, depending on the base-2 expansion
   of the exponent. ex. a^1 * a^4 * ... a^8 for n=13 *)
val generic_pow : zero:'a -> one:'a -> div_two:('a -> 'a) -> mod_two:('a -> 'a) -> mul:('a -> 'a -> 'a) -> 'a -> 'a -> 'a
