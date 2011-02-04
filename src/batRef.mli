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

    References are mutable values, i.e. "variables" which may actually
    change value during their life-time, as variables in imperative
    languages. References can be understood as 1-cell arrays and
    are typically used to implement imperative algorithms in OCaml.

    References are useful but don't abuse them.

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

external set : 'a ref -> 'a -> unit = "%setfield0"
    (** As [ := ] *)

external get : 'a ref -> 'a = "%field0"
    (** As [ ! ]*)
    
val copy: 'a ref -> 'a ref
  (** [copy r] returns a new reference with the same initial
      content as [r].*)

val pre : 'a ref -> ( 'a -> 'a ) -> 'a
  (** Perform an operation on a reference and return the
      new value of that reference. 

      For instance, if [x] is a reference to [1],
      [pre x ( ( + ) 1) ] returns [2] and sets [x] to [2].*)

val post: 'a ref -> ('a -> 'a) -> 'a
  (** Perform an operation on a reference and return the
      previous value of that reference. 

      For instance, if [x] is a reference to [1],
      [post x ( ( + ) 1)] returns [1] and sets [x] to [2].*)


val swap: 'a ref -> 'a ref -> unit
  (**[swap a b] puts [!b] in [a] and [!a] in [b]*)

val post_incr : int ref -> int
  (**Increment an integer, return the old value.

     Comparable to C or Java's [i++].*)

val post_decr : int ref -> int
  (**Decrement an integer, return the old value.


     Comparable to C or Java 's [i--].*)

val pre_incr: int ref -> int
  (**Increment an integer, return the new value.

     Comparable to C or Java's [++i]. *)

val pre_decr: int ref -> int
  (**Increment an integer, return the new value.

     Comparable to C or Java's [--i]. *)

val protect : 'a ref -> 'a -> (unit -> 'b) -> 'b
  (**Assign a reference temporarily.

     [protect r v body] sets the value of [r] to [v] and executes
     [body]. Once body has been executed, whether termination happens
     as a consequence of regular evaluation or exception, the previous
     value of [r] is restored. *)




(** {6 Boilerplate code}*)


(** {7 Printing}*)

val print: ('b BatInnerIO.output -> 'a -> unit) -> 'b BatInnerIO.output -> 'a t -> unit
