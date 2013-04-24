type order = Lt | Eq | Gt

type 'a comp = 'a -> 'a -> int
type 'a ord = 'a -> 'a -> order

module type Comp = sig
  type t
  val compare : t comp
end

module type Ord = sig
  type t
  val ord : t ord
end

let ord0 n =
  if n < 0 then Lt
  else if n > 0 then Gt
  else Eq

let ord comp = fun a b -> ord0 (comp a b)

let poly_comp = Pervasives.compare

(* eta-expand to avoid value restriction *)
let poly_ord = fun a b -> ord poly_comp a b

let poly = poly_ord

module Ord (Comp : Comp) : Ord with type t = Comp.t = struct
  type t = Comp.t
  let ord = ord Comp.compare
end

let comp0 = function
  | Lt -> -1
  | Eq -> 0
  | Gt -> 1

let comp ord = fun a b -> comp0 (ord a b)

module Comp (Ord : Ord) : Comp with type t = Ord.t = struct
  type t = Ord.t
  let compare = comp Ord.ord
end

let rev_ord0 = function
  | Lt -> Gt
  | Eq -> Eq
  | Gt -> Lt

let rev_comp0 n =
  if n < 0 then 1
  else if n > 0 then -1
  else 0

let rev_ord ord = fun a b -> rev_ord0 (ord a b)
let rev_comp comp = fun a b -> rev_comp0 (comp a b)

let rev = rev_ord

module RevOrd (Ord : Ord) : Ord with type t = Ord.t = struct
  type t = Ord.t
  let ord = rev Ord.ord
end

module RevComp (Comp : Comp) : Comp with type t = Comp.t = struct
  type t = Comp.t
  let compare = rev_comp Comp.compare
end

module Rev = RevOrd

type 'a eq = 'a -> 'a -> bool

let eq_ord0 = function
  | Eq -> true
  | Lt | Gt -> false

let eq_comp0 = function
  | 0 -> true
  | _ -> false

let eq_ord ord = fun a b -> eq_ord0 (ord a b)
let eq_comp comp = fun a b -> eq_comp0 (comp a b)

let eq = eq_ord

module type Eq = sig
  type t
  val eq : t eq
end

module EqOrd (Ord : Ord) : Eq with type t = Ord.t = struct
  type t = Ord.t
  let eq = eq_ord Ord.ord
end

module EqComp (Comp : Comp) : Eq with type t = Comp.t = struct
  type t = Comp.t
  let eq = eq_comp Comp.compare
end

module Eq = EqOrd

type 'a choice = 'a -> 'a -> 'a

let min_ord ord = fun a b ->
  match ord a b with
  | Lt | Eq -> a
  | Gt -> b

let min_comp comp = fun a b ->
  if comp a b <= 0 then a else b

(*$T max_ord
   max_ord poly_ord 1 2 = 2
*)
(*$T max_comp
  max_comp poly_comp 1 2 = 2
*)
let max_ord ord = min_ord (rev_ord ord)
let max_comp comp = min_comp (rev_comp comp)

let min = min_ord
let max = max_ord

let bin_eq eq1 t1 t1' eq2 t2 t2' =
  eq1 t1 t1' && eq2 t2 t2'

let bin_ord ord1 t1 t1' ord2 t2 t2' =
  match ord1 t1 t1' with
  | Eq -> ord2 t2 t2'
  | (Lt | Gt) as neq -> neq

let bin_comp comp1 t1 t1' comp2 t2 t2' =
  match comp1 t1 t1' with
  | 0 -> comp2 t2 t2'
  | nzero -> nzero

let map_eq f eq = fun a b -> eq (f a) (f b)
let map_comp f comp = fun a b -> comp (f a) (f b)
let map_ord f ord = fun a b -> ord (f a) (f b)

(*$T map_eq
  map_eq List.length Int.equal [3] [7]
  not (map_eq List.length Int.equal [] [8;9])
*)
(*$T map_comp
  map_comp Array.length Int.compare [|5;6;7|] [|1;2;3|] = 0
  map_comp Array.length Int.compare [||] [|8|] < 0
*)
(*$T map_ord
  map_ord List.hd String.ord ["foo"; "bar"] ["foo"] = Eq
  map_ord List.tl (List.ord Int.ord) [1;2;3] [8;2;3] = Eq
  map_ord String.length Int.ord "Foo" "Foobar" = Lt
*)

module Incubator = struct
  let eq_by proj = fun x y -> proj x = proj y
  let comp_by proj = fun x y -> Pervasives.compare (proj x) (proj y)
  let ord_by proj = fun x y -> ord0 (Pervasives.compare (proj x) (proj y))
end
