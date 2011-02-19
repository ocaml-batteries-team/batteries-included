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

module Tuple3 = struct
  type ('a,'b,'c) t = 'a * 'b * 'c

  type 'a enumerable = 'a * 'a * 'a
  type 'a mappable = 'a * 'a * 'a

  let fst (a,_,_) = a
  let snd (_,b,_) = b
  let thrd (_,_,c) = c

  let prj12 (a,b,_) = (a,b)
  let prj13 (a,_,c) = (a,c)
  let prj23 (_,b,c) = (b,c)

  let map f (a,b,c) =
    let a = f a in
    let b = f b in
    (a, b, f c)

  let mapn f1 f2 f3 (a,b,c) =
    let a = f1 a in
    let b = f2 b in
    (a, b, f3 c)

  let map1 f (a,b,c) = (f a, b, c)
  let map2 f (a,b,c) = (a, f b, c)
  let map3 f (a,b,c) = (a, b, f c)

  let curry f a b c = f (a,b,c)
  let uncurry f (a,b,c) = f a b c

  let enum (a,b,c) = BatList.enum [a;b;c] (* Make efficient? *)

  let of_enum e = match BatEnum.get e with
      None -> failwith "Tuple3.of_enum: not enough elements"
    | Some a -> match BatEnum.get e with
	  None -> failwith "Tuple3.of_enum: not enough elements"
        | Some b -> match BatEnum.get e with
	      None -> failwith "Tuple3.of_enum: not enough elements"
            | Some c -> (a,b,c)

  let printn print_a print_b print_c out (a,b,c) =
    BatIO.write out '(';
    print_a out a;
    BatIO.write out ',';
    print_b out b;
    BatIO.write out ',';
    print_c out c;
    BatIO.write out ')'

  let print printer out pair = printn printer printer printer out pair

  let compare ?(cmp1=Pervasives.compare) ?(cmp2=Pervasives.compare) ?(cmp3=Pervasives.compare) (a1,a2,a3) (b1,b2,b3) =
    let c1 = cmp1 a1 b1 in
    if c1 <> 0 then c1 else
      let c2 = cmp2 a2 b2 in
      if c2 <> 0 then c2 else
        cmp3 a3 b3
end

module Tuple4 = struct
  type ('a,'b,'c,'d) t = 'a * 'b * 'c * 'd

  type 'a enumerable = 'a * 'a * 'a * 'a
  type 'a mappable = 'a * 'a * 'a * 'a

  let fst (a,_,_,_) = a
  let snd (_,b,_,_) = b
  let thrd (_,_,c,_) = c
  let frth (_,_,_,d) = d

  let prj12 (a,b,_,_) = (a,b)
  let prj13 (a,_,c,_) = (a,c)
  let prj14 (a,_,_,d) = (a,d)
  let prj23 (_,b,c,_) = (b,c)
  let prj24 (_,b,_,d) = (b,d)
  let prj34 (_,_,c,d) = (c,d)

  let prj123 (a,b,c,_) = (a,b,c)
  let prj124 (a,b,_,d) = (a,b,d)
  let prj234 (_,b,c,d) = (b,c,d)

  let map f (a,b,c,d) =
    let a = f a in
    let b = f b in
    let c = f c in
    (a, b, c, f d)

  let mapn f1 f2 f3 f4 (a,b,c,d) =
    let a = f1 a in
    let b = f2 b in
    let c = f3 c in
    (a, b, c, f4 d)

  let map1 f (a,b,c,d) = (f a, b, c, d)
  let map2 f (a,b,c,d) = (a, f b, c, d)
  let map3 f (a,b,c,d) = (a, b, f c, d)
  let map4 f (a,b,c,d) = (a, b, c, f d)

  let curry f a b c d = f (a,b,c,d)
  let uncurry f (a,b,c,d) = f a b c d

  let enum (a,b,c,d) = BatList.enum [a;b;c;d] (* Make efficient? *)

  let of_enum e = match BatEnum.get e with
      None -> failwith "Tuple4.of_enum: not enough elements"
    | Some a -> match BatEnum.get e with
	  None -> failwith "Tuple4.of_enum: not enough elements"
        | Some b -> match BatEnum.get e with
	      None -> failwith "Tuple4.of_enum: not enough elements"
            | Some c -> match BatEnum.get e with
	          None -> failwith "Tuple4.of_enum: not enough elements"
                | Some d -> (a,b,c,d)

  let printn print_a print_b print_c print_d out (a,b,c,d) =
    BatIO.write out '(';
    print_a out a;
    BatIO.write out ',';
    print_b out b;
    BatIO.write out ',';
    print_c out c;
    BatIO.write out ',';
    print_d out d;
    BatIO.write out ')'

  let print printer out pair = printn printer printer printer printer out pair

  let compare ?(cmp1=Pervasives.compare) ?(cmp2=Pervasives.compare) ?(cmp3=Pervasives.compare) ?(cmp4=Pervasives.compare) (a1,a2,a3,a4) (b1,b2,b3,b4) =
    let c1 = cmp1 a1 b1 in
    if c1 <> 0 then c1 else
      let c2 = cmp2 a2 b2 in
      if c2 <> 0 then c2 else
        let c3 = cmp3 a3 b3 in
        if c3 <> 0 then c3 else
          cmp4 a4 b4
end
