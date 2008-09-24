(* $Id: uChar.ml,v 1.4 2004/09/04 16:07:38 yori Exp $ *)
(* Copyright 2002, 2003 Yamagata Yoriyuki. distributed with LGPL *)

type t = int

exception Out_of_range

external unsafe_chr_of_uint : int -> t = "%identity"
external uint_code : t -> int = "%identity"

let char_of c = 
  if c >= 0 && c < 0x100 then Char.chr c else raise Out_of_range

let of_char = Char.code

let code c = if c >= 0 then c else raise Out_of_range

let chr n =
  if n >= 0 && n lsr 31 = 0 then n else invalid_arg "UChar.chr"

let chr_of_uint n = 
  if n lsr 31 = 0 then n else 
  invalid_arg "UChar.char_of_uint"
  
let eq (u1 : t) (u2 : t) = u1 = u2
let compare u1 u2 =
  let sgn = (u1 lsr 16) - (u2 lsr 16) in
  if sgn = 0 then (u1 land 0xFFFF) -  (u2 land 0xFFFF) else sgn

type uchar = t

let int_of u = uint_code u
let of_int n = chr_of_uint n
