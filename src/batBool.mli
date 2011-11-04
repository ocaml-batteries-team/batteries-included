(* 
 * ExtBool - Extended booleans
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

(**Operations on booleans
   
   @author Gabriel Scherer
   @author David Teller
*)

type t = bool
  (**The type of booleans. Formally, this is defined as [type t = true | false] *)

external not : bool -> bool = "%boolnot"
  (** The boolean negation. *)

external ( && ) : bool -> bool -> bool = "%sequand"
  (** The boolean ``and''. Evaluation is sequential, left-to-right:
      in [e1 && e2], [e1] is evaluated first, and if it returns [false],
      [e2] is not evaluated at all. *)

external ( || ) : bool -> bool -> bool = "%sequor"
  (** The boolean ``or''. Evaluation is sequential, left-to-right:
      in [e1 || e2], [e1] is evaluated first, and if it returns [true],
      [e2] is not evaluated at all. *)

val zero : bool
val one : bool
val neg : bool -> bool
val succ : bool -> bool
val pred : bool -> bool
val abs : bool -> bool
val add : bool -> bool -> bool
val mul : bool -> bool -> bool
val sub : bool -> bool -> bool
val div : t -> t -> t
val modulo : t -> t -> t
val pow : t -> t -> t
val min_num : bool
val max_num : bool
val compare : bool -> bool -> int
val of_int : int -> bool
val to_int : bool -> int
val of_string : string -> bool
  (** Convert the given string to a boolean.
      Raise [Invalid_argument "Bool.of_string"] if the string is not
      ["true"], ["false"], ["0"], ["1"], ["tt"] or ["ff"]. *)

val to_string : bool -> string
val of_float  : float -> bool
val to_float  : bool  -> float

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

(** {6 Submodules grouping all infix operators} *)

module Infix : BatNumber.Infix with type bat__infix_t = t
module Compare : BatNumber.Compare with type bat__compare_t = t

(** {6 Boilerplate code}*)

(** {7 Printing}*)
val print: 'a BatInnerIO.output -> t -> unit
val t_printer : t BatValue_printer.t

