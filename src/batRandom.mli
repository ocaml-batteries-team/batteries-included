(* 
 * ExtRandom - Additional randomization operations
 * Copyright (C) 1996 Damien Doligez
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

(** Pseudo-random number generators (PRNG). 
    
    This module extends Stdlib's
    {{:http://caml.inria.fr/pub/docs/manual-ocaml/libref/Random.html}Random}
    module, go there for documentation on the rest of the functions
    and types.

    @author Damien Doligez (base library)
    @author David Teller
    @author Pierre Chambart

    @documents Random
*)

(** {6 Basic functions} *)

val init : int -> unit
(** Initialize the generator, using the argument as a seed.
     The same seed will always yield the same sequence of numbers. *)

val full_init : int array -> unit
(** Same as {!Random.init} but takes more data as seed. *)

val self_init : unit -> unit
(** Initialize the generator with a more-or-less random seed chosen
   in a system-dependent way. *)

val bits : unit -> int
(** Return 30 random bits in a nonnegative integer. *)

val int : int -> int
(** [Random.int bound] returns a random integer between 0 (inclusive)
     and [bound] (exclusive).  [bound] must be greater than 0 and less
     than 2{^30}. *)

val int32 : Int32.t -> Int32.t;;
(** [Random.int32 bound] returns a random integer between 0 (inclusive)
     and [bound] (exclusive).  [bound] must be greater than 0. *)

val nativeint : Nativeint.t -> Nativeint.t;;
(** [Random.nativeint bound] returns a random integer between 0 (inclusive)
     and [bound] (exclusive).  [bound] must be greater than 0. *)
val int64 : Int64.t -> Int64.t;;
(** [Random.int64 bound] returns a random integer between 0 (inclusive)
     and [bound] (exclusive).  [bound] must be greater than 0. *)

val float : float -> float
(** [Random.float bound] returns a random floating-point number
   between 0 (inclusive) and [bound] (exclusive).  If [bound] is
   negative, the result is negative or zero.  If [bound] is 0,
   the result is 0. *)

val bool : unit -> bool
(** [Random.bool ()] returns [true] or [false] with probability 0.5 each. *)

val char : unit -> char
(** Return a random Latin-1 character.*)

(*val uchar : unit -> UChar.t
(** Return a random Unicode character.*)*)

val full_range : unit -> int
(** [Random.full_range ()] returns the maximum entropy possible in a
    single int: 31 bits on 32-bit platforms and 63 bits on 64-bit
    platforms *)


(** {6 Enumerations of random values.} 

    These enumerations may be cloned without loss of performance,
    to obtain reproducible enumerations of pseudo-random numbers.
 *)

val enum_bits  : unit    -> int BatEnum.t

val enum_int   : int     -> int BatEnum.t

val enum_bool  : unit    -> bool BatEnum.t

val enum_float : float   -> float BatEnum.t

val enum_int32 : Int32.t -> Int32.t BatEnum.t

val enum_int64 : Int64.t -> Int64.t BatEnum.t

val enum_nativeint : Nativeint.t -> Nativeint.t BatEnum.t

val enum_char : unit -> char BatEnum.t

(*val enum_uchar : unit -> UChar.t BatEnum.t*)

(** {6 Working with data structures.} *)

val choice : 'a BatEnum.t -> 'a
(** [choice e] returns a randomly-chosen element of [e].

    This function only works on finite enumerations with
    less than 2{^30} elements.*)

val shuffle: 'a BatEnum.t -> 'a array
(** [shuffle e] returns a new array, containing the
    same set of elements as [e], but in a random order.

    Shuffling is implemented using the Fisher-Yates
    algorithm and works in O(n), where n is the number
    of elements of [e].

    This function only works on finite enumerations with
    less than 2{^30} elements. *)

(** {6 Advanced functions} *)

(** Manipulate the current state of the random generator.

    This allows using one or several deterministic PRNGs, even in a
    multi-threaded program, without interference from other parts of
    the program.
*)
module State : sig
  type t
    (** The type of PRNG states. *)

  val make : int array -> t
  (** Create a new state and initialize it with the given seed. *)

  val make_self_init : unit -> t
  (** Create a new state and initialize it with a system-dependent
      low-entropy seed. *)

  val copy : t -> t
  (** Return a copy of the given state. *)

  val bits       : t -> int
  val int        : t -> int -> int
  val int32      : t -> Int32.t -> Int32.t
  val nativeint  : t -> Nativeint.t -> Nativeint.t
  val int64      : t -> Int64.t -> Int64.t
  val float      : t -> float -> float
  val bool       : t -> bool
  val char       : t -> char
(*  val uchar      : t -> UChar.t*)
  val enum_bits  : t -> unit    -> int BatEnum.t
  val enum_int   : t -> int     -> int BatEnum.t
  val enum_bool  : t -> unit    -> bool BatEnum.t
  val enum_float : t -> float   -> float BatEnum.t
  val enum_int32 : t -> Int32.t -> Int32.t BatEnum.t
  val enum_int64 : t -> Int64.t -> Int64.t BatEnum.t
  val enum_nativeint : t -> Nativeint.t -> Nativeint.t BatEnum.t
  val enum_char  : t -> unit    -> char BatEnum.t
(*  val enum_uchar : t -> unit    -> UChar.t BatEnum.t*)

  (** These functions are the same as the basic functions, except that they
      use (and update) the given PRNG state instead of the default one.
  *)


  (** {6 Boilerplate code}*)

end;;


val get_state : unit -> State.t
(** Return the current state of the generator used by the basic functions. *)

val set_state : State.t -> unit
(** Set the state of the generator used by the basic functions. *)


