(*
 * ExtQueue - Extended operations on queues
 * Copyright (C) 1996 Xavier Leroy
 *               2008 David Teller, LIFO, Universite d'Orleans
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


(** Last-in first-out stacks.

    This module implements stacks (LIFOs), with in-place modification.

    This module extends Stdlib's
    {{:http://caml.inria.fr/pub/docs/manual-ocaml/libref/Stack.html}Stack}
    module, go there for documentation on the rest of the functions
    and types.


    @author Xavier Leroy (Base module)
    @author David Teller
*)
open Stack

include BatEnum.Enumerable with type 'a enumerable = 'a t

val enum : 'a t -> 'a BatEnum.t
(** [enum s] returns a destructive enumeration of the elements of stack
    [s], from the most recently entered to the least recently entered.
    Reading the enumeration will progressively empty [s].*)

val of_enum : 'a BatEnum.t -> 'a t
(** [of_enum e] returns a new stack containing all the elements of [e].
    This is equivalent to calling [push] with the first element of the
    enumeration, then with the second, etc.*)

	(** {6 Boilerplate code}*)

(** {7 Printing}*)

val print : ?first:string -> ?last:string -> ?sep:string -> ('a BatInnerIO.output -> 'b -> unit) ->  'a BatInnerIO.output -> 'b t -> unit


module Exceptionless : sig
  val top : 'a t -> 'a option
  val pop : 'a t -> 'a option
end
