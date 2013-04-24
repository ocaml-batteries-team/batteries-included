(*
 * Interfaces - Common interfaces for data structures
 * Copyright (C) 2008 David Teller, LIFO, Universite d'Orleans
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
   Common signatures for data structures.
*)


(** A signature for data structures which have a
    [map : ('a -> 'b) -> ('a t -> 'b t)] operation.

    If you create a new data structure, you should make it compatible
    with [Mappable].
*)
module type Mappable = sig
  type 'a mappable (** The data structure, e.g. ['a List.t] *)

  val map : ('a -> 'b) -> ('a mappable -> 'b mappable)
    (** [map f e] applies [f] to every element of [e] and returns the corresponding data structure *)
end

module type OrderedType =
sig
  type t
  val compare : t -> t -> int
    (** A total ordering function
        This is a two-argument function [f] such that
        [f e1 e2] is zero if the values [e1] and [e2] are equal,
        [f e1 e2] is strictly negative if [e1] is smaller than [e2],
        and [f e1 e2] is strictly positive if [e1] is greater than [e2].
        Example: a suitable ordering function is the generic structural
        comparison function {!Pervasives.compare}. *)
end

(** Monads are a design pattern which may be used to enforce strong
    functional or non-functional constraints on the manipulation of
    resources, while remaining in the world of functional programming.
    For instance, monads may serve to implement approximations of a
    types-and-effects type system, to enforce functional use of arrays
    or other mutable data structures, or to enforce the fact that only
    files opened for writing may be actually used for writing.

    For more information on monads, see
    {{:http://enfranchisedmind.com/blog/2007/08/06/a-monad-tutorial-for-ocaml/}
    A Monad Tutorial for Ocaml}.

    This definition is compatible with the standard syntax extension
    for monads.  For more information, see
    {{:http://www.cas.mcmaster.ca/~carette/pa_monad/} the
    documentation of pa_monad}.

    @author David Teller
*)

(** Signature for monads *)
module type Monad = sig

  (** The type of a monad producing values of type ['a].*)
  type 'a m

  (** Monadic binding.

      [bind m f] executes first [m] then [f], using the
      result of [m]. *)
  val bind : 'a m -> ('a -> 'b m) -> 'b m

  (**Return a value, that is, put a value in the monad.*)
  val return: 'a -> 'a m

end
