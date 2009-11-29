(* 
 * ExtMarshal - Extended marshaling operations 
 * Copyright (C) 1997 Xavier Leroy
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


open Marshal

  let output out ?(sharing=true) ?(closures=false) v =
    let buf = to_string v ((if sharing then [] else [No_sharing]) @ (if closures then [Closures] else [])) in
      BatInnerIO.nwrite out buf

  let input inp =
    let header = BatInnerIO.really_nread inp header_size in
    let size   = data_size header 0                   in 
      from_string (header ^ (BatInnerIO.really_nread inp size)) 0

  let to_channel out v flags =
    output out ~sharing:(not (List.mem No_sharing flags))
               ~closures:(List.mem Closures flags)
      v

  let from_channel = input

