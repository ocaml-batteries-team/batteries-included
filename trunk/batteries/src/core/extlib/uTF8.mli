(* 
 * UTF-8 - UTF-8 encoded Unicode string
 * Copyright 2002, 2003 (C) Yamagata Yoriyuki. 
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
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

(** UTF-8 encoded Unicode strings. 

   The Module for UTF-8 encoded Unicode strings.
*)

(** UTF-8 encoded Unicode strings. The type is normal string. *)
type t (* = string -- no access to internals *)

exception Malformed_code

(** [validate s]
   successes if s is valid UTF-8, otherwise raises Malformed_code.
   Other functions assume strings are valid UTF-8, so it is prudent
   to test their validity for strings from untrusted origins. *)
val validate : t -> unit

(* All functions below assume string are valid UTF-8.  If not,
 * the result is unspecified. *)

(** [get s n] returns [n]-th Unicode character of [s].
   The call requires O(n)-time. *)
val get : t -> int -> UChar.t
val unsafe_get : t -> int -> UChar.t (* not really unsafe *)

val copy : t -> t
val copy_set : t -> int -> UChar.t -> t
val sub : t -> int -> int -> t

val make : int -> UChar.t -> t

(** [length s] returns the number of Unicode characters contained in s *)
val length : t -> int
    
(** Positions in the string represented by the number of bytes from the head.
   The location of the first character is [0] *)
type index = int

val length_at : t -> index -> int

(** [nth s n] returns the position of the [n]-th Unicode character. 
   The call requires O(n)-time *)
val nth : t -> int -> index

(** The position of the head of the first Unicode character. *)
val first : t -> index

(** The position of the head of the last Unicode character. *)
val last : t -> index

(** [look s i]
   returns the Unicode character of the location [i] in the string [s]. *)
val look : t -> index -> UChar.t

(** [out_of_range s i]
   tests whether [i] is a position inside of [s]. *)
val out_of_range : t -> index -> bool

(** [compare_index s i1 i2] returns
   a value < 0 if [i1] is the position located before [i2], 
   0 if [i1] and [i2] points the same location,
   a value > 0 if [i1] is the position located after [i2]. *)
val compare_index : t -> index -> index -> int

(** [next s i]
   returns the position of the head of the Unicode character
   located immediately after [i]. 
   If [i] is inside of [s], the function always successes.
   If [i] is inside of [s] and there is no Unicode character after [i],
   the position outside [s] is returned.  
   If [i] is not inside of [s], the behaviour is unspecified. *)
val next : t -> index -> index

(** [prev s i]
   returns the position of the head of the Unicode character
   located immediately before [i]. 
   If [i] is inside of [s], the function always successes.
   If [i] is inside of [s] and there is no Unicode character before [i],
   the position outside [s] is returned.  
   If [i] is not inside of [s], the behaviour is unspecified. *)
val prev : t -> index -> index

(** [move s i n]
   returns [n]-th Unicode character after [i] if n >= 0,
   [n]-th Unicode character before [i] if n < 0.
   If there is no such character, the result is unspecified. *)
val move : t -> index -> int -> index
    
(** [iter f s]
   applies [f] to all Unicode characters in [s].  
   The order of application is same to the order 
   of the Unicode characters in [s]. *)
val iter : (UChar.t -> unit) -> t -> unit

(** Code point comparison by the lexicographic order.
   [compare s1 s2] returns
   a positive integer if [s1] > [s2],
   0 if [s1] = [s2],
   a negative integer if [s1] < [s2]. *)
val compare : t -> t -> int

val append : t -> t -> t

val empty : t

val join : t list -> t

val of_string : string -> t

val to_string : t -> string

val of_enum : UChar.t Enum.t -> t

val to_enum : t -> UChar.t Enum.t
