(*
 * Uref -- unifiable references
 * Copyright (C) 2011  Batteries Included Development Team
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

(** Unifiable references using destructive union-find *)

type 'a uref
  (** A [t uref] is a reference to a cell that contains a
      value of type [t]. *)

type 'a t = 'a uref
  (** A synonym for convenience *)

val uref : 'a -> 'a uref
  (** [uref x] allocates a new uref and places the value [x] in it. *)

val uget : 'a uref -> 'a
  (** [uget ur] returns the value stored in the uref [ur]. *)

val uset : 'a uref -> 'a -> unit
  (** [uset ur x] updates the contents of [ur] with [x]. *)

val unite : ?sel:('a -> 'a -> 'a) -> 'a uref -> 'a uref -> unit
  (** [unite ~sel ur1 ur2] unites the urefs [ur1] and [ur2], selecting
      the result of [sel (uget ur1) (uget ur2)] for the contents of
      the resulting united uref. After this operation, [uget ur1 ==
      uget ur2]. By default, [sel] is {!fst}. *)

val equal : 'a uref -> 'a uref -> bool
  (** [equal ur1 ur2] returns [true] iff [ur1] and [ur2] are equal
      urefs, either because they are physically the same or because
      they have been {!unite}d. *)

(** {6 Printing} *)

val print : ('a BatInnerIO.output -> 'b -> unit)
  -> 'a BatInnerIO.output -> 'b uref -> unit
  (** Print the uref. *)

val uref_printer : 'a BatValue_printer.t -> 'a uref BatValue_printer.t
  (** See {!BatValue_printer}. *)

val t_printer : 'a BatValue_printer.t -> 'a t BatValue_printer.t
  (** See {!BatValue_printer}. *)
