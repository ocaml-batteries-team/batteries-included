(*
 * BatComplex - Extended Complex Numbers
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

module BaseComplex = struct
  include Complex

  let modulo _ _ =
    failwith "BatComplex.modulo is meaningless" (*BISECT-VISIT*)

  let to_string x =
    ( string_of_float x.re ) ^ " + i " ^ ( string_of_float x.im )

  let pred x = {x with re = x.re -. 1.}

  let succ x = {x with re = x.re +. 1.}

  let to_int x   = int_of_float x.re
  let of_int x   = {re = float_of_int x; im = 0.}
  let to_float x = x.re
  let of_float x = {re = x; im = 0.}

  let abs x    = { re = norm x; im = 0. }

  let compare t1 t2 =
    match compare t1.re t2.re with
    | 0 -> compare t1.im t2.im
    | c -> c

  let ord = BatOrd.ord compare

  let equal t1 t2 =
    t1.re = t2.re && t1.im = t2.im

  let of_string x =
    let fail s =
      failwith (Printf.sprintf "BatComplex.of_string %S: %s" x s) in
    let open Genlex in
    let enum =
      BatGenlex.to_enum_filter
        ( BatGenlex.of_list ["."; "i"; "+"; "-"; "*"] )
        ( BatString.enum x ) in

    let rec parse_re () =
      match BatEnum.peek enum with
      | None -> fail "the string is empty"
      | Some (Int i) -> BatEnum.junk enum; parse_separation (float_of_int i)
      | Some (Float f) -> BatEnum.junk enum; parse_separation f
      | Some (Kwd "-") -> BatEnum.junk enum; parse_i_im ~multiplier:(-1.) 0.
      | Some (Kwd "+") -> BatEnum.junk enum; parse_i_im ~multiplier:1. 0.
      | Some _token -> parse_i_im ~multiplier:1. 0.

    and parse_separation re =
      match BatEnum.get enum with
      | None -> {re; im = 0.}
      | Some (Kwd "-") -> parse_i_im ~multiplier:(-1.) re
      | Some (Kwd "+") -> parse_i_im ~multiplier:1. re
      | Some _ -> fail "unexpected token after real part"

    and parse_i_im ~multiplier re =
      match BatEnum.get enum with
      | Some (Kwd "i") -> (
          match BatEnum.peek enum with
          | None -> {re; im = multiplier}
          | Some (Kwd ".")
          | Some (Kwd "*") ->
            BatEnum.junk enum;
            parse_im ~multiplier re
          | Some _token ->
            parse_im ~multiplier re
        )
      | _ -> fail "expected \"i\" before the imaginary part"

    and parse_im ~multiplier re =
      match BatEnum.peek enum with
      | Some (Int i) ->
        BatEnum.junk enum; parse_end {re; im = multiplier *. float_of_int i}
      | Some (Float f) ->
        BatEnum.junk enum; parse_end {re; im = multiplier *. f}
      | _ -> fail "expected a number for the imaginary part"

    and parse_end c =
      match BatEnum.peek enum with
      | None -> c
      | Some _ -> fail "unexpected trailing tokens" in

    parse_re ()

end

(* need to fix problem with Functor return type being `type t =
   Complex.t` and needing `type t = Complex.t = {re: float; im:float}` *)
module CN = BatNumber.MakeNumeric(BaseComplex)
include BaseComplex
let operations = CN.operations
module Infix = BatNumber.MakeInfix(BaseComplex)
include Infix
module Compare = BatNumber.MakeCompare(BaseComplex)

let inv    = Complex.inv
let i      = Complex.i
let conj   = Complex.conj
let sqrt   = Complex.sqrt
let norm2  = Complex.norm2
let norm   = Complex.norm
let arg    = Complex.arg
let polar  = Complex.polar
let exp    = Complex.exp
let log    = Complex.log
let pow    = Complex.pow

let print out t = BatInnerIO.nwrite out (to_string t)

  (*$T succ
    succ {re = 2.; im = 4.} = {re = 3.; im = 4.}
  *)

  (*$T pred
    pred {re = 2.; im = 4.} = {re = 1.; im = 4.}
  *)

  (*$T abs
    abs {re = 3.; im = 4.} = {re = 5.; im = 0.}
  *)

  (*$T to_int
    to_int {re = 2.; im = 3.} = 2
  *)

  (*$T of_int
    of_int 2 = {re = 2.; im = 0.}
  *)

  (*$T to_float
    to_float {re = 2.; im = 3.} = 2.
  *)

  (*$T of_float
    of_float 2. = {re = 2.; im = 0.}
  *)

  (*$T compare
    compare {re = 2.; im = 3.} {re = 2.; im = 3.} = 0
    compare {re = 2.; im = 3.} {re = 3.; im = 2.} = -1
    compare {re = 3.; im = 3.} {re = 2.; im = 3.} = 1
    compare {re = 3.; im = 4.} {re = 3.; im = 3.} = 1
    compare {re = 3.; im = -4.} {re = 3.; im = 3.} = -1
  *)

  (*$T equal
    equal {re = 2.; im = 3.} {re = 2.; im = 3.}
    not (equal {re = 2.; im = 3.} {re = 3.; im = 2.})
    not (equal {re = 3.; im = 3.} {re = 2.; im = 3.})
    not (equal {re = 3.; im = 4.} {re = 3.; im = 3.})
  *)

  (*$T of_string
    of_string "1." = of_float 1.
    of_string "-1." = {re = -1.; im = 0.}
    of_string "1 + i 2." = {re = 1.; im = 2.}
    of_string "1 - i 2." = {re = 1.; im = -2.}
    of_string "1 - i 2e3" = {re = 1.; im = -2e3}
    of_string "-1. - i -2e3" = {re = -1.; im = 2e3}
    of_string "-1+i 2e3" = {re = -1.; im = 2e3}
    of_string "-1. - i. -2e3" = {re = -1.; im = 2e3}
    of_string "-1. - i * -2e3" = {re = -1.; im = 2e3}
    of_string "  - i *   -2e3" = {re = 0.; im = 2e3}
    of_string "+ i * -2e3" = {re = 0.; im = -2e3}
    of_string "i * -2e3" = {re = 0.; im = -2e3}
    of_string "i" = {re = 0.; im = 1.}
    of_string "-i" = {re = 0.; im = -1.}
    of_string "1 + i" = {re = 1.; im = 1.}
    try ignore (of_string "  "); false with Failure _ -> true
    try ignore (of_string "("); false with BatGenlex.LexerError _ -> true
    try ignore (of_string "1 +"); false with Failure _ -> true
    try ignore (of_string "i +"); false with Failure _ -> true
    try ignore (of_string "1 + i * 3 4"); false with Failure _ -> true
    try ignore (of_string "1 2"); false with Failure _ -> true
  *)

  (*$T print
    BatIO.to_string print {re=3.4; im= -5.6} = "3.4 + i -5.6"
  *)
