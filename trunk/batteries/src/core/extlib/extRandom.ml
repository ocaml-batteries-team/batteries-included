(* 
 * ExtRandom - Additional randomization operations
 * Copyright (C) 1996 Damien Doligez
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
open Conv
TYPE_CONV_PATH "Batteries.Util" (*For Sexplib, Bin-prot...*)

module Random = struct
  open Random
  let init = init
  let full_init = full_init
  let self_init = self_init
  let bits      = bits
  let int       = int
  let int32     = int32
  let int64     = int64
  let nativeint = nativeint
  let float     = float
  let bool      = bool

  let enum_int       bound = Enum.from (fun () -> int bound)

  let enum_int32     bound = Enum.from (fun () -> int32 bound)

  let enum_int64     bound = Enum.from (fun () -> int64 bound)

  let enum_nativeint bound = Enum.from (fun () -> nativeint bound)

  let enum_float     bound = Enum.from (fun () -> float bound)

  let enum_bool            = Enum.from bool

  open ExtArray

  let choice e =
    let a   = Array.of_enum e in
    let len = Array.length  a in
      Array.get a (int len)

  let shuffle e =
    let a = Array.of_enum e in
      for n = Array.length a - 1 downto 1 do
	let k    = int ( n + 1 ) in
	  if k <> n then
	    let buf  = Array.get a n in
	      Array.set a n (Array.get a k);
	      Array.set a k buf
      done;
      a

  let get_state = get_state
  let set_state = set_state

  module State =
  struct
    include State (*Note: here, we use [Marshal] to avoid breaking abstraction.*)
    let sexp_of_t t =
      sexp_of_string (Marshal.to_string t [])
    let t_of_sexp s =
      Marshal.from_string (string_of_sexp s) 0
  end
end
