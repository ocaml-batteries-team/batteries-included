(*
 * BatInt32 - Extended Big integers
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

let big_int_base_default_symbols =
  let s = String.create (10 + 26*2) in
  let set off c k = s.[k] <- char_of_int (k - off + (int_of_char c)) in
  for k = 0 to String.length s - 1 do
    if k < 10 then set  0 '0' k else if k < 36 then set 10 'a' k else set 36 'A' k
  done; s


let to_string_in_custom_base
    symbols (* vector of digit symbols 0,1,...,a,b,... *)
    b       (* base, int > 1 and <= number of defined symbols *)
    n       (* big integer *)
  = let open Big_int in
  if b <= 1 then invalid_arg
      "Big_int.to_string_in_custom_base: base must be > 1";
  if b > String.length symbols then invalid_arg (
      "Big_int.to_string_in_custom_base: big_int_base_default_symbols too small for base "
      ^ (string_of_int b) ^ ": only " ^ string_of_int (String.length symbols));
  let isnegative = sign_big_int n < 0 in
  (* generously over-approximate number of binary digits of n;
     num_digits_big_int actually returns the number of _words_ *)
  let base2digits = Sys.word_size * num_digits_big_int n in
  (* over-approximate resulting digits in base b, using following theorem,
     where k = base2digits :
                                          k                    k      k * Log[2]
      digits in base b <= Ceiling[Log[b, 2 ]]    and   Log[b, 2 ] == -----------
                                                                       Log[b]    *)
  let basebdigits = int_of_float (ceil (
        ((float_of_int base2digits) *. (log 2.))
        /.
          (log (float_of_int b))))
    +
      (if isnegative then 1 else 0) (* the pesky '-' sign *)
  in
  let buff = String.create basebdigits in (* we know the buffer is large enough *)
  let curr = ref (basebdigits - 1) and count = ref 0 in
  let addchar c = buff.[!curr] <- c ; incr count; decr curr in
  (* switch base to big int representation and n to mutable, and loop *)
  let b = big_int_of_int b and n = ref (abs_big_int n) in
  while compare_big_int !n b >= 0 do
    let q,d = quomod_big_int !n b in
    n := q; addchar symbols.[int_of_big_int d];
  done;
  addchar symbols.[int_of_big_int !n];
  if isnegative then addchar '-';
  String.sub buff (!curr + 1) !count

let to_string_in_base b n =
  if b <= 1 || b > 36 then invalid_arg
      "Big_int.to_string_in_base: base must be in 2..36"
  else to_string_in_custom_base big_int_base_default_symbols b n


let to_string_in_binary = to_string_in_base 2
let to_string_in_octal  = to_string_in_base 8
let to_string_in_hexa   = to_string_in_base 16

(*$= to_string_in_base & ~printer:identity
  (to_string_in_base 16 (big_int_of_int 9485))    "250d"
  (to_string_in_base 16 (big_int_of_int (-9485))) "-250d"
  (to_string_in_base 10 (big_int_of_int 9485))    "9485"
  (to_string_in_base  8 (big_int_of_int 9485))    "22415"
  (to_string_in_base  2 (big_int_of_int 9485))    "10010100001101"
  (to_string_in_base 36 (big_int_of_int 948565))  "kbx1"
  (to_string_in_base  3 (big_int_of_int 2765353)) "12012111100111"
*) (*$= to_string_in_custom_base & ~printer:identity
     (to_string_in_custom_base "*/!" 3 (big_int_of_int 2765353)) "/!*/!////**///"
   *) (*$= to_string_in_binary & ~printer:identity
        (to_string_in_binary (big_int_of_int 9485))     "10010100001101"
      *) (*$= to_string_in_octal & ~printer:identity
        (to_string_in_octal (big_int_of_int 9485))      "22415"
      *) (*$= to_string_in_hexa & ~printer:identity
        (to_string_in_hexa (big_int_of_int 9485))       "250d"
      *) (*$T to_string_in_base
        try ignore (to_string_in_base 37 (big_int_of_int 948565)); false \
        with Invalid_argument _ -> true
        try ignore (to_string_in_base 1 (big_int_of_int 948565)); false \
        with Invalid_argument _ -> true
      *) (*$Q to_string_in_base
        Q.int (fun i-> let bi = big_int_of_int i in \
        to_string_in_base 10 bi = string_of_big_int bi)
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
  let ord       = BatOrd.ord compare
  let equal a b = compare a b = 0

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
