(*
 * Monad -- Base monadic operations
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

(** Definition of monads.

    Monads are a design pattern which may be used to enforce strong
    functional or non-functional constraints on the manipulation of
    resources, while remaining in the world of functional programming.
    For instance, monads may serve to implement approximations of a
    types-and-effects type system, to enforce functional use of
    arrays or other mutable data structures, or to enforce the
    fact that only files opened for writing may be actually used
    for writing.

    For more information on monads, see {{:http://enfranchisedmind.com/blog/2007/08/06/a-monad-tutorial-for-ocaml/}
    A Monad Tutorial for Ocaml}.

    This definition is compatible with the standard syntax extension for monads.
    For more information, see {{:http://www.cas.mcmaster.ca/~carette/pa_monad/} the documentation of pa_monad}.
*)

(** Signature for monads *)
module type S =
sig
  type 'a t
    (** The type of a monad producing values of type ['a].*)

  val bind : 'a t -> ('a -> 'b t) -> 'b t
    (** Monadic binding.

	[bind m f] executes first [m] then [f], using the
	result of [m]. *)

  val return: 'a -> 'a t
    (**Return a value.*)

  val failwith: string -> 'a t
    (**Report a fatal error and leave the monadic world.

       [failwith msg] leaves the monadic world as a consequence of
       a programming accident, such as a pattern match error.
       This function should not be used for {e exceptions}, only
       for {e fatal errors}.*)    
end
