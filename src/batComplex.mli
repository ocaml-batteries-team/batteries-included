(*
 * BatComplex - Extended Complex
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

(** Additional and modified functions for complex numbers.*)

(** Complex numbers.

    This module provides arithmetic operations on complex numbers.
    Complex numbers are represented by their real and imaginary parts
    (cartesian representation).  Each part is represented by a
    double-precision floating-point number (type {!float}).

    @author Xavier Leroy (base module)
    @author Gabriel Scherer
    @author David Teller
*)

type t = Complex.t = { re : float; im : float; }

val zero: t
(** The complex number [0]. *)

val one: t
(** The complex number [1]. *)

val i: t
(** The complex number [i]. *)

val neg: t -> t
(** Unary negation. *)

val conj: t -> t
(** Conjugate: given the complex [x + i.y], returns [x - i.y]. *)

val add: t -> t -> t
(** Addition *)

val sub: t -> t -> t
(** Subtraction *)

val mul: t -> t -> t
(** Multiplication *)

val inv: t -> t
(** Multiplicative inverse ([1/z]). *)

val div: t -> t -> t
(** Division *)

val sqrt: t -> t
(** Square root.  The result [x + i.y] is such that [x > 0] or [x =
    0] and [y >= 0].  This function has a discontinuity along the
    negative real axis. *)

val norm2: t -> float
(** Norm squared: given [x + i.y], returns [x^2 + y^2]. *)

val norm: t -> float
(** Norm: given [x + i.y], returns [sqrt(x^2 + y^2)]. *)

val arg: t -> float
(** Argument.  The argument of a complex number is the angle
    in the complex plane between the positive real axis and a line
    passing through zero and the number.  This angle ranges from
    [-pi] to [pi].  This function has a discontinuity along the
    negative real axis. *)

val polar: float -> float -> t
(** [polar norm arg] returns the complex having norm [norm]
    and argument [arg]. *)

val exp: t -> t
(** Exponentiation.  [exp z] returns [e] to the [z] power. *)

val log: t -> t
(** Natural logarithm (in base [e]). *)

val pow: t -> t -> t
(** Power function.  [pow z1 z2] returns [z1] to the [z2] power. *)

val operations : t BatNumber.numeric

val inv : t -> t
(** [inv x] returns the value of [1/x]*)

val succ : t -> t
(** Add {!one} to this number.*)

val pred : t -> t
(** Remove {!one} from this number.*)

val abs : t -> t
(** [abs c] returns the module of this complex number,
    i.e. [abs c = Float.sqrt((re c) *. (re c) +. (im c) *. (im c) )]*)

val modulo : t -> t -> t
val pow : t -> t -> t
val compare : t -> t -> int
val ord : t -> t -> BatOrd.order
val equal : t -> t -> bool

val of_int : int -> t
val to_int : t -> int
(** [to_int c] is the integer part of the real part of [c] *)

val of_string : string -> t
(** [of_string s] accepts strings with the following formats:

    (<int>|<float>) (+|-) i ( * | . |  ) (<int>|<float>)

    where (a|b|c) is either a or b or c.

    In addition the following degenerate formats are also accepted:

    (+|-) i ( * | . |  ) (<int>|<float>)

    (<int>|<float>) (+|-) i

    (<int>|<float>)
*)

val to_string : t -> string
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
val ( -- ): t -> t -> t BatEnum.t
val ( --- ): t -> t -> t BatEnum.t

val of_float : float -> t
(** [Complex.of_float x] returns the complex number [x+0i] *)

val to_float : t -> float
(** [Complex.to_float (a+bi)] returns the float [a] *)

(** {6 Submodules grouping all infix operators} *)

module Infix : BatNumber.Infix with type bat__infix_t = t
module Compare : BatNumber.Compare with type bat__compare_t = t

(** {6 Boilerplate code}*)

(** {7 Printing}*)
val print: 'a BatInnerIO.output -> t -> unit
