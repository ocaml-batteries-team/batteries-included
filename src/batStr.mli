(*
 * ExtStr - Additional functions for regular expressions
 * Copyright (C) 1996 Xavier Leroy, INRIA Rocquencourt
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



(** Regular expressions and high-level string processing 

    @author Xavier Leroy (Base module)
    @author David Teller
*)

open Str

val search : ?offset:int -> ?backwards:bool -> regexp -> string -> (int * int * string) BatEnum.t
  (**[search r s] searches for all the substrings of [s] matching
     regular expression [r]. The result is a triple start offset/end offset/
     matched string.

     @param offset The offset at which to start searching in the string. If
     unspecified, start search at the beginning of [s].
     @param backwards If [false] or unspecified, search forward. Otherwise,
     search backwards.
  *)
