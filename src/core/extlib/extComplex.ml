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

open Sexplib
TYPE_CONV_PATH "Batteries.Data.Numeric" (*For Sexplib, Bin-prot...*)

open Number

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

  open ExtGenlex.Genlex
  open ExtString

  let of_string x =
    let enum = 
      to_enum_filter
	( of_list ["("; ","; ")"; "i"; "+"; "*"] )
	( String.enum x ) in
    let rec parse_re () = match Enum.get enum with
      | None           -> zero
      | Some (Int   i) -> parse_im (float_of_int i)
      | Some (Float f) -> parse_im f
      | Some (Kwd "(") -> parse_re ()
      | _              -> parse_im 0.
    and parse_im x = match Enum.get enum with
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

module Complex = struct
  include Number.MakeNumeric(BaseComplex)
  include BaseComplex
  let sexp_of_t {re=re;im=im} = Conv.sexp_of_pair Conv.sexp_of_float Conv.sexp_of_float (re, im)
  let t_of_sexp s = let (re, im) = Conv.pair_of_sexp Conv.float_of_sexp Conv.float_of_sexp s in {re = re; im = im}
  let print out t = InnerIO.nwrite out (to_string t)
end
