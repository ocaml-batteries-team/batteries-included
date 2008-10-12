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

module Random = struct
  include Random

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
      Array.unsafe_get a (int len)

  let shuffle e =
    let a = Array.of_enum e in
      for n = Array.length a - 1 downto 1 do
	let k    = int ( n + 1 ) in
	  if k <> n then
	    let buf  = Array.unsafe_get a n in
	      Array.unsafe_set a n (Array.unsafe_get a k);
	      Array.unsafe_set a k buf
      done;
      a
end
