(* $Id: uChar.ml,v 1.4 2004/09/04 16:07:38 yori Exp $ *)
(* Copyright 2002, 2003 Yamagata Yoriyuki. distributed with LGPL *)
(* TODO: Check -- this is actually part of a package distributed with LGPL + linking exception *)

TYPE_CONV_PATH "Batteries.Data.Text" (*For Sexplib, Bin-prot...*)

module UChar =
struct
  include CamomileLibrary.UChar

let to_char = char_of
let to_int   = int_of

let sexp_of_t t = Sexplib.Conv.sexp_of_int (to_int t)
let t_of_sexp s = of_int (Sexplib.Conv.int_of_sexp s)
end
