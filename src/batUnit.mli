(* 
 * ExtUnit - Operations on Unit
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
 * Foundation, Inc.
 *)

(**
   Operations on [unit].
   
   @author David Teller
*)

    type t = unit
	(**The unit type, i.e. a type with only one element, [()].*)

    val string_of : t -> string
    (**Convert the given unit to a string.
       
       Returns ["()"]. *)

    val of_string : string -> t
      (**Convert the given string to a unit.

	 Accepts ["()"]. Raises [Invalid_argument "unit_of_string"] if the
	 given string is not ["()"].
      *)
      
    val compare : t -> t -> int
      (** Compare two units.

	  Always returns 0.*)

    (** {6 Boilerplate code}*)

    (** {7 Printing}*)
    val print: 'a BatInnerIO.output -> unit -> unit

