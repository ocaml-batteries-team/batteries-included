(*
 * Hashcons -- a hashconsing library
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

(** Hash consing of data structures *)

(** The type [t hobj] represents hashed objects of type [t]. A hashed
    object contains a unique tag and a hash code. *)
type 'a hobj = private {
  obj   : 'a ;
  tag   : int ;                       (** Unique id for this object *)
  hcode : int ;                       (** Hash code for this object *)
}

type 'a t = 'a hobj
  (** A synonym for convenience *)

val compare : 'a hobj -> 'a hobj -> int
  (** Comparison on the tags *)

(** Hashcons tables *)
module type Table = sig
  type key
    (** type of objects in the table *)

  type t
    (** type of the table *)

  val create : int -> t
    (** [create n] creates a table with at least [n] cells. *)

  val clear  : t -> unit
    (** [clear tab] removes all entries from the table [tab]. *)

  val hashcons : t -> key -> key hobj
    (** [hashcons tab k] returns either [k], adding it to the table
        [tab] as a side effect, or if [k] is already in the table then
        it returns the hashed object corresponding to that entry. *)

  val iter : (key hobj -> unit) -> t -> unit
    (** [iter f tab] applied [f] to every live hashed object in the
        table [tab]. *)
end

module MakeTable (HT : BatHashtbl.HashedType)
  : Table with type key = HT.t

(** Hashing utilities *)
module H : sig
  val hc0_ : int -> int
    (** [hc0_ h] corresponds to the hashcode of a first constructor
        applied to an object of hashcode [h] *)

  val hc0  : 'a hobj -> int
    (** [hc0 ho] is the hashcode of a first constructor applied to the
        hashed object [ho] *)

  val hc1_ : int -> int -> int
    (** [hc1_ h k] corresponds to the hashcode of the [k]th
        constructor applied to an object of hashcode [h]. *)

  val hc1  : 'a hobj -> int -> int
    (** [hc1 ho k] corresponds to the hashcode of the [k]th
        constructor applied to the hashed object [ho]. *)
end


