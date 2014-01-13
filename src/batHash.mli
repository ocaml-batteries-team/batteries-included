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

(** Hashing *)

(** Simple and efficient hash functions for use in Hashtbl. Not designed
    for cryptographic hashing. *)

type 'a hash = 'a -> int
(** A function that maps values of type [t] to {b positive} integers.
    It must be compatible with equality, that is, if equality is defined
    on [t] (say a function [eq : t -> t -> bool], then for use in
    a Hash table it is required that whenever, for [a:t] and [b:t],
    [eq a b] holds, then [hash a = hash b] must also hold. *)

type mix_int = int -> int -> int
(** [mix_int] describes functions to combine (positive) integers together,
    in the present context to mix hash values together (say, when hashing
    a list) *)

val sdbm : mix_int
