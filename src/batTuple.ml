(*
 * Pair - functions for pairs of values
 * Copyright (C) 2003 Nicolas Cannasse
 *               2008 David Teller (Contributor)
 *               2011 Ashish Agarwal
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

module Tuple2 = struct 
  type ('a,'b) t = 'a * 'b
      
  type 'a enumerable = 'a * 'a
  type 'a mappable = 'a * 'a
      
  external fst : 'a * 'b -> 'a = "%field0"
  external snd : 'a * 'b -> 'b = "%field1"
      
  let map f (x,y) =
    (* force left-to-right evaluation order (this principle of least
       surprise is already applied in stdlib's List.map) *)
    let a = f x in
    (a, f y)
      
  let mapn f g (a,b) =
    let a = f a in
    (a, g b)
      
  let map1 f (a,b) = (f a, b)
  let map2 f (a,b) = (a, f b)
    
  let curry f x y = f (x,y)
  let uncurry f (x,y) = f x y
    
  let enum (x,y) = BatList.enum [x;y] (* Make efficient? *)
    
  let of_enum e = match BatEnum.get e with 
      None -> failwith "Tuple2.of_enum: not enough elements" 
    | Some x -> match BatEnum.get e with 
	  None -> failwith "Tuple2.of_enum: not enough elements" 
        | Some y -> (x,y)
            
  let printn print_a print_b out (a,b) = 
    BatIO.write out '(';
    print_a out a;
    BatIO.write out ',';
    print_b out b;
    BatIO.write out ')'
      
  let print printer out pair = printn printer printer out pair
    
  let compare ?(cmp1=Pervasives.compare) ?(cmp2=Pervasives.compare) (a,b) (c,d) = 
    let comp = cmp1 a c in 
    if comp <> 0 then comp else cmp2 b d
end
