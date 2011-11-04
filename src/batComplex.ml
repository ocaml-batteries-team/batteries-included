(* 
 * ExtComplex - Extended Complex Numbers
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

  let modulo _ = assert false
    (** Meaningless*)

  let to_string x = 
    ( string_of_float x.re ) ^ "+ i" ^ ( string_of_float x.im )

  let pred x = {x with re = x.re -. 1.}

  let succ x = {x with re = x.re +. 1.}

  let to_int x   = int_of_float x.re
  let of_int x   = {re = float_of_int x; im = 0.}
  let to_float x = x.re
  let of_float x = {re = x; im = 0.}

  let abs x    = { re = norm x; im = 0. }

  let compare = Pervasives.compare

  open Genlex

  let of_string x =
    let enum = 
      BatGenlex.to_enum_filter
	( BatGenlex.of_list ["("; ","; ")"; "i"; "+"; "*"] )
	( BatString.enum x ) in
    let rec parse_re () = match BatEnum.get enum with
      | None           -> zero
      | Some (Int   i) -> parse_im (float_of_int i)
      | Some (Float f) -> parse_im f
      | Some (Kwd "(") -> parse_re ()
      | _              -> parse_im 0.
    and parse_im x = match BatEnum.get enum with
      | None           -> {re = x; im = 0.}
      | Some (Kwd "+") -> parse_im x
      | Some (Kwd ",") -> parse_im x
      | Some (Kwd "i") -> parse_im x
      | Some (Kwd "*") -> parse_im x
      | Some (Float f) -> {re = x; im = f}
      | Some (Int   i) -> {re = x; im = float_of_int i}
      | _              -> {re = x; im = 0.}
    in parse_re ()

end

include BatNumber.MakeNumeric(BaseComplex)
module Infix = BatNumber.MakeInfix(BaseComplex)
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

