(*
 * ExtInt32 - Extended Big integers
 * Copyright (C) 2007 Bluestorm <bluestorm dot dylc on-the-server gmail dot com>
 *               2008 David Teller
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

let big_int_base_default_symbols = ref (Array.init (10 + 26*2)
  (let get off c k = char_of_int (k - off + (int_of_char c)) in fun k ->
   if k < 10 then get  0 '0' k else if k < 36 then get 10 'A' k else get 36 'a' k ))

   
let to_string_in_base
      ? (symbols = !big_int_base_default_symbols)
      b (* base, int > 1 *)
      n (* big integer *)
      = let open Big_int in
  if b <= 1 then invalid_arg
    "Big_int.to_string_in_base: base must be > 1";
  if b > Array.length symbols then invalid_arg (
    "Big_int.to_string_in_base: big_int_base_default_symbols too small for base "
      ^ (string_of_int b) ^ ": only " ^ (string_of_int (Array.length symbols)) ^ ".");
  (* reverse a mutable string in its place *)
  let in_place_string_rev s =
    let len = String.length s in
    for k = 0 to (len - 1)/2 do
      let old = s.[k]
      and mirror = len - 1 - k in
      s.[k] <- s.[mirror];
      s.[mirror] <- old;
    done
  in
  (* generously over-approximate number of decimal digits of n;
     num_digits_big_int actually returns the number of _words_ *)
  let base10digits = (Sys.word_size / 32) * 10 * num_digits_big_int n in
  (* over-approximate resulting digits in base b, using following theorem:
               k          k
      Log[b, 10 ] == ---------- .
                     Log[10, b]         *)
  let basebdigits = int_of_float (ceil (
    (float_of_int base10digits)
     /.
    (log10 (float_of_int b)))) in
  let buff = Buffer.create basebdigits in (* we know the buffer is large enough *)
  (* switch base to big int representation and n to mutable, and loop *)
  let b = big_int_of_int b and n = ref n in
  while compare_big_int !n b >= 0 do
    let q,d = quomod_big_int !n b in
    n := q; Buffer.add_char buff symbols.(int_of_big_int d);
  done;
  Buffer.add_char buff symbols.(int_of_big_int !n);
  let res = Buffer.contents buff in
  in_place_string_rev res; res

(*$= to_string_in_base & ~printer:identity
  (to_string_in_base 16 (big_int_of_int 9485))    "250D"
  (to_string_in_base 10 (big_int_of_int 9485))    "9485"
  (to_string_in_base  8 (big_int_of_int 9485))    "22415"
  (to_string_in_base  2 (big_int_of_int 9485))    "10010100001101"
  (to_string_in_base 36 (big_int_of_int 948565))  "KBX1"
  (to_string_in_base 62 (big_int_of_int 948565))  "3ylR"
*) (*$T to_string_in_base
  try ignore (to_string_in_base 63 (big_int_of_int 948565)); false \
    with Invalid_argument _ -> true
  try ignore (to_string_in_base 1 (big_int_of_int 948565)); false \
    with Invalid_argument _ -> true
*)


open BatNumber

module BaseBig_int = struct
  open Big_int

  type t = big_int
  let zero = zero_big_int
  let one  = unit_big_int
  let succ = succ_big_int
  let pred = pred_big_int
  let neg  = minus_big_int
  let abs  = abs_big_int
  let add  = add_big_int
  let sub  = sub_big_int
  let mul  = mult_big_int
  let div  = div_big_int

  let modulo = mod_big_int
  let pow  = power_big_int_positive_big_int

  let to_string = string_of_big_int
  let of_string = big_int_of_string
  let to_int    = int_of_big_int
  let of_int    = big_int_of_int

  let compare   = compare_big_int

  let of_float f =
    try of_string (Printf.sprintf "%.0f" f)
    with Failure _ -> invalid_arg "batBig_int.of_float"
  (*$T of_float
    to_int (of_float 4.46) = 4
    to_int (of_float 4.56) = 5
    to_int (of_float (-4.46)) = -4
    to_int (of_float (-4.56)) = -5
    try ignore (of_float nan); false with Invalid_argument _ -> true
    try ignore (of_float (1. /. 0.)); false with Invalid_argument _ -> true
    try ignore (of_float (-1. /. 0.)); false with Invalid_argument _ -> true
  *)

  let to_float  = float_of_big_int
end

include Big_int
include MakeNumeric(BaseBig_int)

let print out t = BatIO.nwrite out (to_string t)
(*$T print
  BatIO.to_string print (of_int 456) = "456"
  BatIO.to_string print (power_int_positive_int 10 31) = "10000000000000000000000000000000"
  BatIO.to_string print (power_int_positive_int (-10) 31) = "-10000000000000000000000000000000"
*)
