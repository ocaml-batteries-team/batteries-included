(* 
 * BatUTF-8 - Additional functions for UTF8 string manipulation
 * Copyright 2002, 2003 (C) Yamagata Yoriyuki. 
 * Copyright (C) 2008 Edgar Friendly, David Teller
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

open BatUChar
open BatCamomile

(** UTF-8 encoded Unicode strings. 

    This module defines UTF-8 encoded Unicode strings, implemented in
    a manner comparable to native OCaml strings. This module is
    provided essentially for internal use and should be regarded as
    mostly obsoleted by {!Rope}.

    {b Note} For type-safety reasons, the definition of type {!t} is
    kept abstract. This may cause incompatibilities with Camomile library.

    @author Yamagata Yoriyuki (Camomile)
    @author Edgar Friendly
    @author David Teller

    @documents UTF8
*)

(** UTF-8 encoded Unicode strings.  If you coerce it to a string,
    modify it at your own risk.  Call {!validate} to verify that the
    contents are still valid UTF-8.  *)
type t = private string
    
exception Malformed_code

val validate : string -> unit
  (** [validate s] succeeds if s is valid UTF-8, otherwise raises
      [Malformed_code].  Other functions assume strings are valid
      UTF-8, so it is prudent to test their validity for strings from
      untrusted origins. *)


(* All functions below assume string are valid UTF-8.  If not,
 * the result is unspecified. 
 * Note: invalid strings could happen only as a consequence of
 * tampering with undocumented functions.*)

val append : t -> t -> t
  (**Concatenate two UTF8 strings*)

val empty : t
  (**The empty UTF8 string*)

val of_char : UChar.t -> t
  (**As {!String.of_char}*)

val make : int -> UChar.t -> t
  (**As {!String.make}*)

val of_string : string -> t
  (**Adopt a string. Involves copying.*)

val to_string : t -> string
  (**Return an UTF-8 encoded string representing this Unicode string.*)

val adopt : string -> t
  (** Adopt a string without copying. Modifying the original string
      will modify this value, possibly breaking safety guarantees. *)

val enum : t -> UChar.t BatEnum.t
  (**As {!String.enum}*)

val of_enum: UChar.t BatEnum.t -> t
  (**As {!String.of_enum}*)

val backwards : t -> UChar.t BatEnum.t
  (**As {!String.backwards}*)

val of_backwards: UChar.t BatEnum.t -> t
  (**As {!String.of_backwards}*)

val sub : t -> int -> int ->  t
  (** As {!String.sub}*)

val get : t -> int -> UChar.t
  (** [get s n] returns the [n]-th Unicode character of [s].  The call
      requires O(n)-time. *)


val init : int -> (int -> UChar.t) -> t
(** [init len f] 
    returns a new string which contains [len] Unicode characters.
    The i-th Unicode character is initialized by [f i] *)


val length : t -> int
(** [length s] returns the number of Unicode characters contained in s *)
    
val length0: int -> int
  (** UTF8 encoding often calls for the encoding of a Unicode character
      with several bytes. If [c] is the beginning of a UTF8 encoded
      character, [length0 c] returns the total number of bytes which
      must be read for the Unicode character to be complete.

      @return 1 if the character is complete, n >= 2 otherwise*)

(** Positions in the string as indexes by characters.  The location of
    the first character is [0].  The location of the second is [1]. *)
type char_idx = int

(** Positions in the string represented by the number of bytes from
    the head.  The location of the first character is [Byte.first].
    The location of the second is [Byte.next Byte.first]. *)
module Byte : sig
  type b_idx(* = private int*)
  val of_int_unsafe : int -> b_idx
  val to_int : b_idx -> int
  val next : t -> b_idx -> b_idx
  val prev : t -> b_idx -> b_idx
  val of_char_idx : t -> char_idx -> b_idx
    (** [of_char_idx s n] returns the position of the [n]-th Unicode
	character.  The call requires O(n)-time *)
  val at_end : t -> b_idx -> bool
  val out_of_range : t -> b_idx -> bool
  val first : b_idx
  val last : t -> b_idx
  val move : t -> b_idx -> int -> b_idx
end

type index = int

val look : t -> Byte.b_idx -> UChar.t
(** [look s i]
   returns the Unicode character of the location [i] in the string [s]. *)

val out_of_range : t -> Byte.b_idx -> bool
  (** [out_of_range s i]
      tests whether [i] is a position inside of [s]. *)

val compare_index : t -> index -> index -> int
(** [compare_index s i1 i2] returns
   a value < 0 if [i1] is the position located before [i2], 
   0 if [i1] and [i2] points the same location,
   a value > 0 if [i1] is the position located after [i2]. *)

val next : t -> index -> index
(** [next s i]
   returns the position of the head of the Unicode character
   located immediately after [i]. 
   If [i] is inside of [s], the function always successes.
   If [i] is inside of [s] and there is no Unicode character after [i],
   the position outside [s] is returned.  
   If [i] is not inside of [s], the behaviour is unspecified. *)

val prev : t -> index -> index
(** [prev s i]
   returns the position of the head of the Unicode character
   located immediately before [i]. 
   If [i] is inside of [s], the function always successes.
   If [i] is inside of [s] and there is no Unicode character before [i],
   the position outside [s] is returned.  
   If [i] is not inside of [s], the behaviour is unspecified. *)

val move : t -> index -> int -> index
(** [move s i n]
   returns [n]-th Unicode character after [i] if n >= 0,
   [n]-th Unicode character before [i] if n < 0.
   If there is no such character, the result is unspecified. *)
    
val iter : (UChar.t -> unit) -> t -> unit
(** [iter f s]
   applies [f] to all Unicode characters in [s].  
   The order of application is same to the order 
   of the Unicode characters in [s]. *)

val compare : t -> t -> int
  (** Code point comparison by lexicographic order.  [compare s1 s2]
      returns a positive integer if [s1] > [s2], 0 if [s1] = [s2], a
      negative integer if [s1] < [s2]. *)

val concat : t -> t list -> t
  (** [concat sep [a;b;c...] ] returns the concatenation of
      [a], [sep], [b], [sep], [c], [sep]... *)

val join : t -> t list -> t
  (**as [concat]*)


val uppercase : t -> t
(** Return a copy of the argument, with all lowercase letters
   translated to uppercase.*)

val lowercase : t -> t
(** Return a copy of the argument, with all uppercase letters
   translated to lowercase.*)

val casefold : t -> t
(** Do the case folding transform to the input text (for doing case-insensitive comparison) *)


val init : int -> (int -> UChar.t) -> t
(** As [String.init] *)

val map : (UChar.t -> UChar.t) -> t -> t
(** As [String.map] *)

val filter_map : (UChar.t -> UChar.t option) -> t -> t
(** As [String.filter_map] *)

val filter : (UChar.t -> bool) -> t -> t
(** As {!String.filter}*)

val index : t -> UChar.t -> int
(** As [String.index] *)

val rindex : t -> UChar.t -> int
(** As [String.rindex] *)

val contains : t -> UChar.t -> bool
(** As [String.contains] *)

val contains_from : t -> UChar.t -> Byte.b_idx -> bool

val rcontains_from : t -> UChar.t -> Byte.b_idx -> bool

val escaped : t -> t

(** Buffer module for UTF-8 strings *)
module Buf : sig
  (** Buffers for UTF-8 strings. *) 
  type buf

  (** [create n] creates the buffer with the initial size [n]-bytes. *)   
  val create : int -> buf

  (* The rest of functions is similar to the ones of Buffer in stdlib. *)
  (** [contents buf] returns the contents of the buffer. *)
  val contents : buf -> t

  (** Empty the buffer, 
     but retains the internal storage which was holding the contents *)
  val clear : buf -> unit

  (** Empty the buffer and de-allocate the internal storage. *)
  val reset : buf -> unit

  (** Add one Unicode character to the buffer. *)
  val add_char : buf -> UChar.t -> unit

  (** Add the UTF-8 string to the buffer. *)
  val add_string : buf -> t -> unit

  (** [add_buffer b1 b2] adds the contents of [b2] to [b1].
     The contents of [b2] is not changed. *)
  val add_buffer : buf -> buf -> unit
end with type buf = Buffer.t

(** {6 Boilerplate code}*)

(** {7 Printing}*)
val print: 'a BatInnerIO.output -> t -> unit

val t_printer : t BatValue_printer.t

(**/**)
external of_string_unsafe : string -> t = "%identity"
external to_string_unsafe : t -> string = "%identity"
  (**Adopt a string without copying*)

val unsafe_get : t -> int -> UChar.t
val copy_set   : t -> int -> UChar.t -> t
(**/**)


