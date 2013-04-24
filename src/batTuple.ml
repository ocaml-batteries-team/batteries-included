(*
 * Tuples - functions for tuples
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

  external first : 'a * 'b -> 'a = "%field0"
  external second : 'a * 'b -> 'b = "%field1"

  let swap (a,b) = (b,a)

  let map f g (a,b) =
    let a = f a in
    (a, g b)

  let mapn f (x,y) =
    (* force left-to-right evaluation order (this principle of least
       surprise is already applied in stdlib's List.map) *)
    let a = f x in
    (a, f y)

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

  let print ?(first="(") ?(sep=",") ?(last=")") print_a print_b out (a,b) =
    BatIO.nwrite out first;
    print_a out a;
    BatIO.nwrite out sep;
    print_b out b;
    BatIO.nwrite out last

  let printn ?(first="(") ?(sep=",") ?(last=")") printer out pair =
    print ~first ~sep ~last printer printer out pair

  let compare ?(cmp1=Pervasives.compare) ?(cmp2=Pervasives.compare) (a,b) (c,d) =
    let comp = cmp1 a c in
    if comp <> 0 then comp else cmp2 b d

  open BatOrd
  let eq eq1 eq2 =
    fun (t1, t2) (t1', t2') ->
      bin_eq eq1 t1 t1' eq2 t2 t2'

  let ord ord1 ord2 =
    fun (t1, t2) (t1', t2') ->
      bin_ord ord1 t1 t1' ord2 t2 t2'

  let comp comp1 comp2 =
    fun (t1, t2) (t1', t2') ->
      bin_comp comp1 t1 t1' comp2 t2 t2'

  module Eq (A : Eq) (B : Eq) = struct
    type t = A.t * B.t
    let eq = eq A.eq B.eq
  end

  module Ord (A : Ord) (B : Ord) = struct
    type t = A.t * B.t
    let ord = ord A.ord B.ord
  end

  module Comp (A : Comp) (B : Comp) = struct
    type t = A.t * B.t
    let compare = comp A.compare B.compare
  end

end

module Tuple3 = struct
  type ('a,'b,'c) t = 'a * 'b * 'c

  type 'a enumerable = 'a * 'a * 'a

  let first (a,_,_) = a
  let second (_,b,_) = b
  let third (_,_,c) = c

  let get12 (a,b,_) = (a,b)
  let get13 (a,_,c) = (a,c)
  let get23 (_,b,c) = (b,c)

  let map f1 f2 f3 (a,b,c) =
    let a = f1 a in
    let b = f2 b in
    (a, b, f3 c)

  let mapn f (a,b,c) =
    let a = f a in
    let b = f b in
    (a, b, f c)

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

  let print ?(first="(") ?(sep=",") ?(last=")") print_a print_b print_c out (a,b,c) =
    BatIO.nwrite out first;
    print_a out a;
    BatIO.nwrite out sep;
    print_b out b;
    BatIO.nwrite out sep;
    print_c out c;
    BatIO.nwrite out last

  let printn ?(first="(") ?(sep=",") ?(last=")") printer out pair =
    print ~first ~sep ~last printer printer printer out pair

  let compare ?(cmp1=Pervasives.compare) ?(cmp2=Pervasives.compare) ?(cmp3=Pervasives.compare) (a1,a2,a3) (b1,b2,b3) =
    let c1 = cmp1 a1 b1 in
    if c1 <> 0 then c1 else
      let c2 = cmp2 a2 b2 in
      if c2 <> 0 then c2 else
        cmp3 a3 b3

  open BatOrd
  let eq eq1 eq2 eq3 =
    fun (t1, t2, t3) (t1', t2', t3') ->
      bin_eq eq1 t1 t1'
        (bin_eq eq2 t2 t2' eq3) t3 t3'

  let ord ord1 ord2 ord3 =
    fun (t1, t2, t3) (t1', t2', t3') ->
      bin_ord ord1 t1 t1'
        (bin_ord ord2 t2 t2' ord3) t3 t3'

  let comp comp1 comp2 comp3 =
    fun (t1, t2, t3) (t1', t2', t3') ->
      bin_comp comp1 t1 t1'
        (bin_comp comp2 t2 t2' comp3) t3 t3'

  module Eq (A : Eq) (B : Eq) (C : Eq) = struct
    type t = A.t * B.t * C.t
    let eq = eq A.eq B.eq C.eq
  end

  module Ord (A : Ord) (B : Ord) (C : Ord) = struct
    type t = A.t * B.t * C.t
    let ord = ord A.ord B.ord C.ord
  end

  module Comp (A : Comp) (B : Comp) (C : Comp)= struct
    type t = A.t * B.t * C.t
    let compare = comp A.compare B.compare C.compare
  end
end

module Tuple4 = struct
  type ('a,'b,'c,'d) t = 'a * 'b * 'c * 'd

  type 'a enumerable = 'a * 'a * 'a * 'a

  let first (a,_,_,_) = a
  let second (_,b,_,_) = b
  let third (_,_,c,_) = c
  let fourth (_,_,_,d) = d

  let get12 (a,b,_,_) = (a,b)
  let get13 (a,_,c,_) = (a,c)
  let get14 (a,_,_,d) = (a,d)
  let get23 (_,b,c,_) = (b,c)
  let get24 (_,b,_,d) = (b,d)
  let get34 (_,_,c,d) = (c,d)

  let get123 (a,b,c,_) = (a,b,c)
  let get124 (a,b,_,d) = (a,b,d)
  let get234 (_,b,c,d) = (b,c,d)

  let map f1 f2 f3 f4 (a,b,c,d) =
    let a = f1 a in
    let b = f2 b in
    let c = f3 c in
    (a, b, c, f4 d)

  let mapn f (a,b,c,d) =
    let a = f a in
    let b = f b in
    let c = f c in
    (a, b, c, f d)

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

  let print ?(first="(") ?(sep=",") ?(last=")") print_a print_b print_c print_d out (a,b,c,d) =
    BatIO.nwrite out first;
    print_a out a;
    BatIO.nwrite out sep;
    print_b out b;
    BatIO.nwrite out sep;
    print_c out c;
    BatIO.nwrite out sep;
    print_d out d;
    BatIO.nwrite out last

  let printn ?(first="(") ?(sep=",") ?(last=")") printer out pair =
    print ~first ~sep ~last printer printer printer printer out pair

  let compare ?(cmp1=Pervasives.compare) ?(cmp2=Pervasives.compare) ?(cmp3=Pervasives.compare) ?(cmp4=Pervasives.compare) (a1,a2,a3,a4) (b1,b2,b3,b4) =
    let c1 = cmp1 a1 b1 in
    if c1 <> 0 then c1 else
      let c2 = cmp2 a2 b2 in
      if c2 <> 0 then c2 else
        let c3 = cmp3 a3 b3 in
        if c3 <> 0 then c3 else
          cmp4 a4 b4

  open BatOrd
  let eq eq1 eq2 eq3 eq4 =
    fun (t1, t2, t3, t4) (t1', t2', t3', t4') ->
      bin_eq eq1 t1 t1'
        (bin_eq eq2 t2 t2'
           (bin_eq eq3 t3 t3' eq4)) t4 t4'

  let ord ord1 ord2 ord3 ord4 =
    fun (t1, t2, t3, t4) (t1', t2', t3', t4') ->
      bin_ord ord1 t1 t1'
        (bin_ord ord2 t2 t2'
           (bin_ord ord3 t3 t3' ord4)) t4 t4'

  let comp comp1 comp2 comp3 comp4 =
    fun (t1, t2, t3, t4) (t1', t2', t3', t4') ->
      bin_comp comp1 t1 t1'
        (bin_comp comp2 t2 t2'
           (bin_comp comp3 t3 t3' comp4)) t4 t4'

  module Eq (A : Eq) (B : Eq) (C : Eq) (D : Eq) = struct
    type t = A.t * B.t * C.t * D.t
    let eq = eq A.eq B.eq C.eq D.eq
  end

  module Ord (A : Ord) (B : Ord) (C : Ord) (D : Ord) = struct
    type t = A.t * B.t * C.t * D.t
    let ord = ord A.ord B.ord C.ord D.ord
  end

  module Comp (A : Comp) (B : Comp) (C : Comp) (D : Comp) = struct
    type t = A.t * B.t * C.t * D.t
    let compare = comp A.compare B.compare C.compare D.compare
  end
end

module Tuple5 = struct
  type ('a,'b,'c,'d,'e) t = 'a * 'b * 'c * 'd * 'e

  type 'a enumerable = 'a * 'a * 'a * 'a * 'a

  let first (a,_,_,_,_) = a
  let second (_,b,_,_,_) = b
  let third (_,_,c,_,_) = c
  let fourth (_,_,_,d,_) = d
  let fifth (_,_,_,_,e) = e

  let get12 (a,b,_,_,_) = (a,b)
  let get13 (a,_,c,_,_) = (a,c)
  let get14 (a,_,_,d,_) = (a,d)
  let get15 (a,_,_,_,e) = (a,e)
  let get23 (_,b,c,_,_) = (b,c)
  let get24 (_,b,_,d,_) = (b,d)
  let get25 (_,b,_,_,e) = (b,e)
  let get34 (_,_,c,d,_) = (c,d)
  let get35 (_,_,c,_,e) = (c,e)
  let get45 (_,_,_,d,e) = (d,e)

  let get123 (a,b,c,_,_) = (a,b,c)
  let get124 (a,b,_,d,_) = (a,b,d)
  let get125 (a,b,_,_,e) = (a,b,e)
  let get134 (a,_,c,d,_) = (a,c,d)
  let get135 (a,_,c,_,e) = (a,c,e)
  let get145 (a,_,_,d,e) = (a,d,e)
  let get234 (_,b,c,d,_) = (b,c,d)
  let get235 (_,b,c,_,e) = (b,c,e)
  let get245 (_,b,_,d,e) = (b,d,e)
  let get345 (_,_,c,d,e) = (c,d,e)

  let get1234 (a,b,c,d,_) = (a,b,c,d)
  let get1235 (a,b,c,_,e) = (a,b,c,e)
  let get1245 (a,b,_,d,e) = (a,b,d,e)
  let get1345 (a,_,c,d,e) = (a,c,d,e)
  let get2345 (_,b,c,d,e) = (b,c,d,e)

  let map f1 f2 f3 f4 f5 (a,b,c,d,e) =
    let a = f1 a in
    let b = f2 b in
    let c = f3 c in
    let d = f4 d in
    (a, b, c, d, f5 e)

  let mapn f (a,b,c,d,e) =
    let a = f a in
    let b = f b in
    let c = f c in
    let d = f d in
    (a, b, c, d, f e)

  let map1 f (a,b,c,d,e) = (f a, b, c, d, e)
  let map2 f (a,b,c,d,e) = (a, f b, c, d, e)
  let map3 f (a,b,c,d,e) = (a, b, f c, d, e)
  let map4 f (a,b,c,d,e) = (a, b, c, f d, e)
  let map5 f (a,b,c,d,e) = (a, b, c, d, f e)

  let curry f a b c d e = f (a,b,c,d,e)
  let uncurry f (a,b,c,d,e) = f a b c d e

  let enum (a,b,c,d,e) = BatList.enum [a;b;c;d;e] (* Make efficient? *)

  let of_enum e = match BatEnum.get e with
      None -> failwith "Tuple5.of_enum: not enough elements"
    | Some a -> match BatEnum.get e with
        None -> failwith "Tuple5.of_enum: not enough elements"
      | Some b -> match BatEnum.get e with
          None -> failwith "Tuple5.of_enum: not enough elements"
        | Some c -> match BatEnum.get e with
            None -> failwith "Tuple5.of_enum: not enough elements"
          | Some d -> match BatEnum.get e with
              None -> failwith "Tuple5.of_enum: not enough elements"
            | Some e -> (a,b,c,d,e)

  let print ?(first="(") ?(sep=",") ?(last=")") print_a print_b print_c print_d print_e out (a,b,c,d,e) =
    BatIO.nwrite out first;
    print_a out a;
    BatIO.nwrite out sep;
    print_b out b;
    BatIO.nwrite out sep;
    print_c out c;
    BatIO.nwrite out sep;
    print_d out d;
    BatIO.nwrite out sep;
    print_e out e;
    BatIO.nwrite out last

  let printn ?(first="(") ?(sep=",") ?(last=")") printer out pair =
    print ~first ~sep ~last printer printer printer printer printer out pair

  let compare ?(cmp1=Pervasives.compare) ?(cmp2=Pervasives.compare) ?(cmp3=Pervasives.compare) ?(cmp4=Pervasives.compare) ?(cmp5=Pervasives.compare) (a1,a2,a3,a4,a5) (b1,b2,b3,b4,b5) =
    let c1 = cmp1 a1 b1 in
    if c1 <> 0 then c1 else
      let c2 = cmp2 a2 b2 in
      if c2 <> 0 then c2 else
        let c3 = cmp3 a3 b3 in
        if c3 <> 0 then c3 else
          let c4 = cmp4 a4 b4 in
          if c4 <> 0 then c4 else
            cmp5 a5 b5

  open BatOrd
  let eq eq1 eq2 eq3 eq4 eq5 =
    fun (t1, t2, t3, t4, t5) (t1', t2', t3', t4', t5') ->
      bin_eq eq1 t1 t1'
        (bin_eq eq2 t2 t2'
           (bin_eq eq3 t3 t3'
              (bin_eq eq4 t4 t4' eq5))) t5 t5'

  let ord ord1 ord2 ord3 ord4 ord5 =
    fun (t1, t2, t3, t4, t5) (t1', t2', t3', t4', t5') ->
      bin_ord ord1 t1 t1'
        (bin_ord ord2 t2 t2'
           (bin_ord ord3 t3 t3'
              (bin_ord ord4 t4 t4' ord5))) t5 t5'

  let comp comp1 comp2 comp3 comp4 comp5 =
    fun (t1, t2, t3, t4, t5) (t1', t2', t3', t4', t5') ->
      bin_comp comp1 t1 t1'
        (bin_comp comp2 t2 t2'
           (bin_comp comp3 t3 t3'
              (bin_comp comp4 t4 t4' comp5))) t5 t5'

  module Eq (A : Eq) (B : Eq) (C : Eq) (D : Eq) (E : Eq) = struct
    type t = A.t * B.t * C.t * D.t * E.t
    let eq = eq A.eq B.eq C.eq D.eq E.eq
  end

  module Ord (A : Ord) (B : Ord) (C : Ord) (D : Ord) (E : Ord) = struct
    type t = A.t * B.t * C.t * D.t * E.t
    let ord = ord A.ord B.ord C.ord D.ord E.ord
  end

  module Comp (A : Comp) (B : Comp) (C : Comp) (D : Comp) (E : Comp) = struct
    type t = A.t * B.t * C.t * D.t * E.t
    let compare = comp A.compare B.compare C.compare D.compare E.compare
  end
end
