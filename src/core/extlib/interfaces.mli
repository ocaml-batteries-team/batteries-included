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

   @documents Data
*)

(** A signature for data structures which may be converted to [enum].

    If you create a new data structure, you should make it compatible
    with [Enumerable].
*)
module type Enumerable = sig
  type container (**The data structure, e.g. [string]*)
  type contents  (**The contents of the data structure, e.g. [char]*)
  val enum : container -> contents Enum.t
end

(** A signature for data structures which may be converted to [enum].

    If you create a new data structure, if possible, you should make
    it compatible with [Of_enum].
*)
module type Of_enum = sig
  type container (**The data structure, e.g. [string]*)
  type contents  (**The contents of the data structure, e.g. [char]*)
  val of_enum : contents Enum.t -> container
end

module type OrderedType =
sig
  type t
    (** The type of the set elements. *)
  val compare : t -> t -> int
    (** A total ordering function over the set elements.
        This is a two-argument function [f] such that
        [f e1 e2] is zero if the elements [e1] and [e2] are equal,
        [f e1 e2] is strictly negative if [e1] is smaller than [e2],
        and [f e1 e2] is strictly positive if [e1] is greater than [e2].
        Example: a suitable ordering function is the generic structural
        comparison function {!Pervasives.compare}. *)
end
