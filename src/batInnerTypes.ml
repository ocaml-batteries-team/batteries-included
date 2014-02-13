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

(** Common type definition for structural sharing *)

type 'a enum = {
  mutable count : unit -> int;
    (** Return the number of remaining elements in the enumeration. *)
  mutable next  : unit -> 'a;
    (** Return the next element of the enumeration or raise [No_more_elements].*)
  mutable clone : unit -> 'a enum;
    (** Return a copy of the enumeration. *)
  mutable fast  : bool;
    (** [true] if [count] can be done without reading all elements, [false] otherwise.*)
}

type 'a weak_set = ('a, unit) BatInnerWeaktbl.t

type input = {
  mutable in_read  : unit -> char;
  mutable in_input : string -> int -> int -> int;
  mutable in_close : unit -> unit;
  in_id: int; (**A unique identifier.*)
  in_upstream: input weak_set;
}

type 'a output = {
  mutable out_write : char -> unit;
  mutable out_output: string -> int -> int -> int;
  mutable out_close : unit -> 'a;
  mutable out_flush : unit -> unit;
  out_id:    int; (**A unique identifier.*)
  out_upstream: unit output weak_set;
    (** The set of outputs which have been created to write to this output.*)
}

type ('a, 'b) printer = 'b output -> 'a -> unit
(** The type of a printing function to print a ['a] to an output that
    produces ['b] as result. *)

type 'a f_printer = Format.formatter -> 'a -> unit
