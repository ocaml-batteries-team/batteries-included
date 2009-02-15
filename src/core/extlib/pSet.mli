(*
 * PSet - Polymorphic sets
 * Copyright (C) 1996-2003 Xavier Leroy, Nicolas Cannasse, Markus Mottl
 * Copyright (C)      2008 David Teller
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
   Polymorphic sets.

   @author Xavier Leroy
   @author Nicolas Cannasse
   @author Markus Mottl
   @author David Teller
*)

type 'a t
  (** The type of sets. *)
  
val empty: 'a t
  (** The empty set, using [compare] as comparison function *)

val create : ('a -> 'a -> int) -> 'a t
  (** Creates a new empty set, using the provided function for key comparison.*)

val is_empty: 'a t -> bool
  (** Test whether a set is empty or not. *)
  
val mem: 'a -> 'a t -> bool
  (** [mem x s] tests whether [x] belongs to the set [s]. *)
  
val add: 'a -> 'a t -> 'a t
  (** [add x s] returns a set containing all elements of [s],
      plus [x]. If [x] was already in [s], [s] is returned unchanged. *)
  
val remove: 'a -> 'a t -> 'a t
  (** [remove x s] returns a set containing all elements of [s],
      except [x]. If [x] was not in [s], [s] is returned unchanged. *)
  
  
val iter: ('a -> unit) -> 'a t -> unit
  (** [iter f s] applies [f] in turn to all elements of [s].
      The elements of [s] are presented to [f] in increasing order
      with respect to the ordering over the type of the elements. *)
  
val fold: ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  (** [fold f s a] computes [(f xN ... (f x2 (f x1 a))...)],
      where [x1 ... xN] are the elements of [s], in increasing order. *)
  
val exists: ('a -> bool) -> 'a t -> bool
  (** [exists p s] checks if at least one element of
      the set satisfies the predicate [p]. *)
  
  
val cardinal: 'a t -> int
  (** Return the number of elements of a set. *)
  
val enum: 'a t -> 'a Enum.t
  (** Return an enumeration of all elements of the given set.
      The returned enumeration is sorted in increasing order with respect
      to the ordering of this set.*)
  
val of_enum: 'a Enum.t -> 'a t

(** {6 Boilerplate code}*)
(** {7 S-Expressions}*)

val t_of_sexp : (Sexplib.Sexp.t -> 'a) -> Sexplib.Sexp.t -> 'a t
val sexp_of_t : ('a -> Sexplib.Sexp.t) -> 'a t -> Sexplib.Sexp.t


(** {7 Printing}*)
  
val print :  ?first:string -> ?last:string -> ?sep:string -> 
  ('a InnerIO.output -> 'c -> unit) -> 
  'a InnerIO.output -> 'c t -> unit
