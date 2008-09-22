(*
 * Batlib_ExtLib_ExtHashtbl.Hashtbl - Importing ExtLib module ExtHashtbl.Hashtbl
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

include Extlib.ExtHashtbl.Hashtbl

module type HashedType =
  sig
    type t
      (** The type of the hashtable keys. *)
    val equal : t -> t -> bool
      (** The equality predicate used to compare keys. *)
    val hash : t -> int
      (** A hashing function on keys. It must be such that if two keys are
          equal according to [equal], then they have identical hash values
          as computed by [hash].
          Examples: suitable ([equal], [hash]) pairs for arbitrary key
          types include
          ([(=)], {!Hashtbl.hash}) for comparing objects by structure,
          ([(fun x y -> compare x y = 0)], {!Hashtbl.hash})
          for comparing objects by structure and handling {!Pervasives.nan}
          correctly, and
          ([(==)], {!Hashtbl.hash}) for comparing objects by addresses
          (e.g. for cyclic keys). *)
   end
(** The input signature of the functor {!Hashtbl.Make}. *)

module type S =
  sig
    type key
    type 'a t
    val create   : int -> 'a t
    val clear    : 'a t -> unit
    val copy     : 'a t -> 'a t
    val add      : 'a t -> key -> 'a -> unit
    val remove   : 'a t -> key -> unit
    val find     : 'a t -> key -> 'a
    val find_all : 'a t -> key -> 'a list
    val replace  : 'a t -> key -> 'a -> unit
    val mem      : 'a t -> key -> bool
    val iter     : (key -> 'a -> unit) -> 'a t -> unit
    val fold     : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val length   : 'a t -> int
  end
(** The output signature of the functor {!Hashtbl.Make}. *)


module Make(H: HashedType) =
struct
  include Hashtbl.Make(H)
end

let hash = Hashtbl.hash
external hash_param : int -> int -> 'a -> int = "caml_hash_univ_param" "noalloc"
