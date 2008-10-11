(* $Id: uChar.mli,v 1.4 2004/09/04 16:07:38 yori Exp $ *)
(* Copyright 2002, 2003 Yamagata Yoriyuki. distributed with LGPL *)
(* TODO: Check -- this is actually part of a package distributed with LGPL + linking exception *)

(** Unicode (ISO-UCS) characters.

   This module implements Unicode (actually ISO-UCS) characters.  All
   31-bit code points are allowed.

    @author Yamagata Yoriyuki (Camomile module)
    @author Edgar Friendly

    @documents UChar
*)
module UChar :
sig

(** Unicode characters. All 31bit code points are allowed.*) 
type t = CamomileLibrary.UChar.t

exception Out_of_range


val of_char : char -> t
(** [of_char c] returns the Unicode character of the Latin-1 character [c] *)

val to_char : t -> char
  (** [to_char u] returns the Latin-1 representation of [u].  If [u] can
      not be represented by Latin-1, raises Out_of_range *)

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

val chr_of_uint : int -> t
(** [chr_of_uint n] returns the Unicode character of the code number [n].
   [n] is interpreted as unsigned, that is, on 32-bits platforms,
   the sign bit is treated as the 31-th bit of the code number.
   If n exceed 31-bits values, then raise [invalid_arg]. *)


val eq : t -> t -> bool
(** Equality by code point comparison *)


val compare : t -> t -> int
(** [compare u1 u2] returns, 
   a value > 0 if [u1] has a larger Unicode code number than [u2], 
   0 if [u1] and [u2] are the same Unicode character,
   a value < 0 if [u1] has a smaller Unicode code number than [u2]. *)

(** Alias of [uint_code] *)
val to_int : t   -> int

(** Alias of [chr_of_uint] *)
val of_int : int -> t

(**/**)
val char_of : t -> char
(**As {!to_char}*)

val int_of : t -> int
(**As {!to_int}*)

type uchar = t
(**Alias of type [t]*)

(** {6 Boilerplate code}*)
(** {7 S-Expressions}*)
val t_of_sexp : Sexplib.Sexp.t -> t
val sexp_of_t : t -> Sexplib.Sexp.t


end
