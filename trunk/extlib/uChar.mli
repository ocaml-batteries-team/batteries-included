(* $Id: uChar.mli,v 1.4 2004/09/04 16:07:38 yori Exp $ *)
(* Copyright 2002, 2003 Yamagata Yoriyuki. distributed with LGPL *)

(** Unicode (ISO-UCS) characters.

   This module implements Unicode (actually ISO-UCS) characters.  All
   31-bit code points are allowed.
*)

(** Unicode characters. All 31bit code points are allowed.*) 
type t

exception Out_of_range

(** [char_of u] returns the Latin-1 representation of [u].
   If [u] can not be represented by Latin-1, raises Out_of_range *)
val char_of : t -> char

(** [of_char c] returns the Unicode character of the Latin-1 character [c] *)
val of_char : char -> t

(** [code u] returns the Unicode code number of [u].
   If the value can not be represented by a positive integer,
   raise Out_of_range *)
val code : t -> int

(** [code n] returns the Unicode character with the code number [n]. 
   If n >= 2^32 or n < 0, raises [invalid_arg] *)
val chr : int -> t

(** [uint_code u] returns the Unicode code number of [u].
   The returned int is unsigned, that is, on 32-bits platforms,
   the sign bit is used for storing the 31-th bit of the code number. *)
external uint_code : t -> int = "%identity"

(** [chr_of_uint n] returns the Unicode character of the code number [n].
   [n] is interpreted as unsigned, that is, on 32-bits platforms,
   the sign bit is treated as the 31-th bit of the code number.
   If n exceed 31-bits values, then raise [invalid_arg]. *)
val chr_of_uint : int -> t

(** Equality by code point comparison *)
val eq : t -> t -> bool

(** [compare u1 u2] returns, 
   a value > 0 if [u1] has a larger Unicode code number than [u2], 
   0 if [u1] and [u2] are the same Unicode character,
   a value < 0 if [u1] has a smaller Unicode code number than [u2]. *)
val compare : t -> t -> int

(** Aliases of [type t] *)
type uchar = t

(** Alias of [uint_code] *)
val int_of : uchar -> int

(** Alias of [chr_of_uint] *)
val of_int : int -> uchar
