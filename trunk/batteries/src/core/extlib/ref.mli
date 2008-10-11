(* 
 * Ref - Operations on references
 * Copyright (C) 2008 David Teller
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

(** Operations on references. 

    References are imperative-style mutable values, i.e. "variables"
    which may change value during their life-time.

    @author Xavier Leroy (base module)
    @author David Teller
*)

type 'a t = 'a ref
    (** The type of references.*)

external ref : 'a -> 'a ref = "%makemutable"
    (** Return a fresh reference containing the given value. *)

external ( ! ) : 'a ref -> 'a = "%field0"
    (** [!r] returns the current contents of reference [r].
    Equivalent to [fun r -> r.contents]. *)

external ( := ) : 'a ref -> 'a -> unit = "%setfield0"
    (** [r := a] stores the value of [a] in reference [r].
	Equivalent to [fun r v -> r.contents <- v]. *)


val pre : 'a ref -> ( 'a -> 'a ) -> 'a
  (** Perform an operation on a reference and return the
      previous value of that reference. 

      For instance, if [x] is a reference to [1],
      [pre x ( ( + ) 1) ] returns [1] and sets [x] to [2].*)

val post: 'a ref -> ('a -> 'a) -> 'a
  (** Perform an operation on a reference and return the
      new value of that reference. 

      For instance, if [x] is a reference to [1],
      [pre x ( ( + ) 1)] returns [2] and sets [x] to [2].*)


val swap: 'a ref -> 'a ref -> unit
  (**[swap a b] puts [!b] in [a] and [!a] in [b]*)

val pre_incr : int ref -> int
  (**Increment an integer, return the old value.

     Comparable to C or Java's [i++].*)

val pre_decr : int ref -> int
  (**Decrement an integer, return the old value.


     Comparable to C or Java 's [i--].*)

val post_incr: int ref -> int
  (**Increment an integer, return the new value.

     Comparable to C or Java's [++i]. *)

val post_decr: int ref -> int
  (**Increment an integer, return the new value.

     Comparable to C or Java's [--i]. *)

(** {6 Boilerplate code}*)
(** {7 S-Expressions}*)

val t_of_sexp : (Sexplib.Sexp.t -> 'a) -> Sexplib.Sexp.t -> 'a t
val sexp_of_t : ('a -> Sexplib.Sexp.t) -> 'a t -> Sexplib.Sexp.t

