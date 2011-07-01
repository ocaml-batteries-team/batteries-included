(*
 * Pair - functions for pairs of values
 * Copyright (C) 2003 Nicolas Cannasse
 *               2008 David Teller (Contributor)
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
 
type ('a,'b) t = 'a * 'b

type 'a enumerable = 'a * 'a
type 'a mappable = 'a * 'a

let map f (x,y) =
  (* force left-to-right evaluation order (this principle of least
     surprise is already applied in stdlib's List.map) *)
  let a = f x in
  (a, f y)

let compare ?(c1=Pervasives.compare) ?(c2=Pervasives.compare) (a,b) (c,d) = 
  let comp = c1 a c in 
  if comp <> 0 then comp else c2 b d

let enum (x,y) = BatList.enum [x;y] (* Make efficient? *)

let of_enum e = match BatEnum.get e with 
    None -> failwith "Pair.of_enum: not enough elements" 
  | Some x -> match BatEnum.get e with 
	None -> failwith "Pair.of_enum: not enough elements" 
      | Some y -> (x,y)

let print ?(first="(") ?(sep=",") ?(last=")") print_a print_b out (a,b) = 
  BatIO.nwrite out first;
  print_a out a;
  BatIO.nwrite out sep;
  print_b out b;
  BatIO.nwrite out last

let print2 printer out pair = print printer printer out pair
  
