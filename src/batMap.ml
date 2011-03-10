(* 
 * ExtMap - Additional map operations
 * Copyright (C) 1996 Xavier Leroy
 *               2009 David Rajchenbach-Teller, LIFO, Universite d'Orleans
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



  module type OrderedType = BatInterfaces.OrderedType

  module type S =
  sig
    type key
      
    type (+'a) t
      
    val empty: 'a t
      
    val is_empty: 'a t -> bool
      
    val add: key -> 'a -> 'a t -> 'a t
      
    val find: key -> 'a t -> 'a
      
    val remove: key -> 'a t -> 'a t

    val modify: key -> ('a -> 'a) -> 'a t -> 'a t

    val modify_def: 'a -> key -> ('a -> 'a) -> 'a t -> 'a t

      
    val mem: key -> 'a t -> bool
      
    val iter: (key -> 'a -> unit) -> 'a t -> unit
      
    val map: ('a -> 'b) -> 'a t -> 'b t
      
    val mapi: (key -> 'a -> 'b) -> 'a t -> 'b t

    val fold: (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b

    val filter: ('a -> bool) -> 'a t -> 'a t

    val filteri: (key -> 'a -> bool) -> 'a t -> 'a t
                
    val filter_map: (key -> 'a -> 'b option) -> 'a t -> 'b t
    
    val compare: ('a -> 'a -> int) -> 'a t -> 'a t -> int
          
    val equal: ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
          
    val keys : _ t -> key BatEnum.t
          
    val values: 'a t -> 'a BatEnum.t
        
    val min_binding : 'a t -> (key * 'a)
    
    val max_binding : 'a t -> (key * 'a)
    
    val choose : 'a t -> (key * 'a)
    
(*
    val split : key -> 'a t -> ('a t * 'a option * 'a t)
*)
    val enum  : 'a t -> (key * 'a) BatEnum.t
    
    val backwards  : 'a t -> (key * 'a) BatEnum.t

    val of_enum: (key * 'a) BatEnum.t -> 'a t

    (** {6 Boilerplate code}*)

    (** {7 Printing}*)

    val print :  ?first:string -> ?last:string -> ?sep:string -> 
      ('a BatInnerIO.output -> key -> unit) -> 
      ('a BatInnerIO.output -> 'c -> unit) -> 
      'a BatInnerIO.output -> 'c t -> unit

    module Exceptionless : sig
      val find: key -> 'a t -> 'a option
    end

    module Infix : sig
      val (-->) : 'a t -> key -> 'a
      val (<--) : 'a t -> key * 'a -> 'a t
    end

    module Labels : sig
      val add : key:key -> data:'a -> 'a t -> 'a t
      val iter : f:(key:key -> data:'a -> unit) -> 'a t -> unit
      val map : f:('a -> 'b) -> 'a t -> 'b t
      val mapi : f:(key:key -> data:'a -> 'b) -> 'a t -> 'b t
      val filter: f:('a -> bool) -> 'a t -> 'a t
      val filteri:f:(key -> 'a -> bool) -> 'a t -> 'a t
      val fold :
	f:(key:key -> data:'a -> 'b -> 'b) ->
	'a t -> init:'b -> 'b
      val compare: cmp:('a -> 'a -> int) -> 'a t -> 'a t -> int
      val equal: cmp:('a -> 'a -> bool) -> 'a t -> 'a t -> bool

    end
  end

  module Make(Ord : OrderedType) =
    struct
      include Map.Make(Ord)

	(*We break the abstraction so as to be able to create enumerations
	  lazily*)
      type 'a implementation = 
	  Empty
	| Node of 'a implementation * key * 'a * 'a implementation * int


      external t_of_impl: 'a implementation -> 'a t = "%identity"
      external impl_of_t: 'a t -> 'a implementation = "%identity"

      type 'a iter = E | C of key * 'a * 'a implementation * 'a iter

      let rec cardinal = function
          Empty -> 0
        | Node(l, _, _, r, _) -> cardinal l + 1 + cardinal r

      let rec cons_iter s t = match s with
          Empty -> t
        | Node (l, k, v, r, _) -> cons_iter l (C (k, v, r, t))

      let rec rev_cons_iter s t = match s with
          Empty -> t
        | Node (l, k, v, r, _) -> rev_cons_iter r (C (k, v, l, t))

      let rec enum_next l () = match !l with
          E -> raise BatEnum.No_more_elements
        | C (k, v, m, t) -> l := cons_iter m t; (k, v)

      let rec enum_backwards_next l () = match !l with
          E -> raise BatEnum.No_more_elements
        | C (k, v, m, t) -> l := rev_cons_iter m t; (k, v)

      let rec enum_count l () =
        let rec aux n = function
            E -> n
          | C (_, _, m, t) -> aux (n + 1 + cardinal m) t
        in aux 0 !l

      let enum t =
        let rec make l =
          let l = ref l in
          let clone() = make !l in
            BatEnum.make ~next:(enum_next l) ~count:(enum_count l) ~clone
        in make (cons_iter (impl_of_t t) E)

      let backwards t =
        let rec make l =
          let l = ref l in
          let clone() = make !l in
            BatEnum.make ~next:(enum_backwards_next l) ~count:(enum_count l) ~clone
        in make (rev_cons_iter (impl_of_t t) E)

      let keys    t = BatEnum.map fst (enum t)
      let values  t = BatEnum.map snd (enum t)
      let of_enum e =
	BatEnum.fold (fun acc (key, data) -> add key data acc) empty e


      let print ?(first="{\n") ?(last="\n}") ?(sep=",\n") print_k print_v out t =
	BatEnum.print ~first ~last ~sep (fun out (k,v) -> BatPrintf.fprintf out "%a: %a" print_k k print_v v) out (enum t)

      (*We rely on [fold] rather than on ['a implementation] to
	make future changes of implementation in the base
	library's version of [Map] easier to track, even if the
	result is a tad slower.*)
      let filter  f t = fold (fun k a acc -> if f a then add k a acc else acc) t empty
      let filteri f t = fold (fun k a acc -> if f k a then add k a acc else acc) t empty
      let filter_map f t = fold (fun k a acc -> match f k a with
				   | None   -> acc
				   | Some v -> add k v acc)  t empty

      let rec min_binding = function
	  Empty -> raise Not_found
	| Node(Empty, x, d, r, _) -> (x, d)
	| Node(l, x, d, r, _) -> min_binding l
      let min_binding s = min_binding (impl_of_t s) (* define properly for t *)

      let rec max_binding = function
	  Empty -> raise Not_found
	| Node(l, x, d, Empty, _) -> (x, d)
	| Node(l, x, d, r, _) -> max_binding r
      let max_binding s = max_binding (impl_of_t s) (* define properly for t *)

      let choose = function
	  Empty -> raise Not_found
	| Node(_, x, d, _, _) -> (x, d)
      let choose s = choose (impl_of_t s) (* define properly for t *)

      let modify x f m =
	let rec loop = function
	  | Node (l, k, v, r, h) ->
              let c = Ord.compare x k in
              if c = 0 then Node (l, x, f v, r, h)
              else if c < 0 then Node (loop l, k, v, r, h)
              else (* c > 0 *)	Node (l, k, v, loop r, h)
	  | Empty -> raise Not_found in
	t_of_impl (loop (impl_of_t m))

      let modify_def v0 x f m =
	let rec loop = function
	  | Node (l, k, v, r, h) ->
              let c = Ord.compare x k in
              if c = 0 then Node (l, x, f v, r, h)
              else if c < 0 then Node (loop l, k, v, r, h)
              else (* c > 0 *)	Node (l, k, v, loop r, h)
	  | Empty -> Node (Empty, x, f v0, Empty, 1) in
	t_of_impl (loop (impl_of_t m))

(*	NEEDS BAL FROM MAP IMPLEMENTATION
      (* needed by split, not exposed atm *)
      let rec join (l : 'a implementation) (x : key) (d : 'a) (r : 'a implementation) =
	match (l, r) with
            (Empty, _) -> add x d (t_of_impl r)
	  | (_, Empty) -> add x d (t_of_impl l)
	  | (Node(ll, lx, ld, lr, lh), Node(rl, rx, rd, rr, rh)) ->
              if lh > rh + 2 then bal ll lx ld (join lr x d r) else
		if rh > lh + 2 then bal (join l x d rl) rx rd rr else
		  create l x d r

      let rec split key = function
          Empty ->
            (Empty, None, Empty)
	| Node(l, x, d, r, _) ->
            let c = Ord.compare key x in
            if c = 0 then (l, Some d, r)
            else if c < 0 then
              let (ll, pres, rl) = split key l in (ll, pres, join rl x d r)
            else
              let (lr, pres, rr) = split key r in (join l x d lr, pres, rr)
      let split k s = split k (impl_of_t s) (* define properly for t *)
*)


      module Exceptionless =
      struct
	let find k t = try Some (find k t) with Not_found -> None
      end

      module Infix =
      struct
        let (-->) map key = find key map
        let (<--) map (key, value) = add key value map
      end
	
      module Labels =
      struct
	let add ~key ~data t = add key data t
	let iter ~f t = iter (fun key data -> f ~key ~data) t
	let map ~f t = map f t
	let mapi ~f t = mapi (fun key data -> f ~key ~data) t
	let fold ~f t ~init = fold (fun key data acc -> f ~key ~data acc) t init
	let compare ~cmp a b = compare cmp a b
	let equal ~cmp a b = equal cmp a b
	let filter ~f = filter f
	let filteri ~f = filteri f
      end  

    end

  module StringMap  = Make(String)
  module IStringMap = Make(BatString.IString)
  module NumStringMap = Make(BatString.NumString)
(*  module RopeMap    = Make(BatRope) 
  module IRopeMap   = Make(BatRope.IRope) *)
  module IntMap     = Make(BatInt)

(*
 * PMap - Polymorphic maps
 * Copyright (C) 1996-2003 Xavier Leroy, Nicolas Cannasse, Markus Mottl
 *               2009 David Rajchenbach-Teller, LIFO, Universite d'Orleans
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


type ('k, 'v) map =
  | Empty
  | Node of ('k, 'v) map * 'k * 'v * ('k, 'v) map * int

type ('k, 'v) t =
  {
    cmp : 'k -> 'k -> int;
    map : ('k, 'v) map;
  }

let height = function
  | Node (_, _, _, _, h) -> h
  | Empty -> 0

(* The create and bal functions are from stdlib's map.ml (3.12)
   differences from the old (extlib) implementation :

   1. create use direct integer comparison instead of calling
   polymorphic 'max'

   2. the two calls of 'height' for hl and hr in the beginning of 'bal'
   (hot path) are inlined

   The difference in performances is important for bal-heavy worflows,
   such as "adding a lot of elements". On a test system, we go from
   1800 op/s to 2500 op/s.
*)
let make l x d r =
  let hl = height l and hr = height r in
  Node(l, x, d, r, (if hl >= hr then hl + 1 else hr + 1))

let bal l x d r =
  let hl = match l with Empty -> 0 | Node(_,_,_,_,h) -> h in
  let hr = match r with Empty -> 0 | Node(_,_,_,_,h) -> h in
  if hl > hr + 2 then begin
    match l with
        Empty -> invalid_arg "Map.bal"
      | Node(ll, lv, ld, lr, _) ->
        if height ll >= height lr then
          make ll lv ld (make lr x d r)
        else begin
          match lr with
              Empty -> invalid_arg "Map.bal"
            | Node(lrl, lrv, lrd, lrr, _)->
              make (make ll lv ld lrl) lrv lrd (make lrr x d r)
        end
  end else if hr > hl + 2 then begin
    match r with
        Empty -> invalid_arg "Map.bal"
      | Node(rl, rv, rd, rr, _) ->
        if height rr >= height rl then
          make (make l x d rl) rv rd rr
        else begin
          match rl with
              Empty -> invalid_arg "Map.bal"
            | Node(rll, rlv, rld, rlr, _) ->
              make (make l x d rll) rlv rld (make rlr rv rd rr)
        end
  end else
      Node(l, x, d, r, (if hl >= hr then hl + 1 else hr + 1))

let rec min_binding = function (* shadowed by definition at end of file *)
  | Node (Empty, k, v, _, _) -> k, v
  | Node (l, _, _, _, _) -> min_binding l
  | Empty -> raise Not_found

let rec remove_min_binding = function
  | Node (Empty, _, _, r, _) -> r
  | Node (l, k, v, r, _) -> bal (remove_min_binding l) k v r
  | Empty -> invalid_arg "PMap.remove_min_binding"

let merge t1 t2 =
  match t1, t2 with
  | Empty, _ -> t2
  | _, Empty -> t1
  | _ ->
      let k, v = min_binding t2 in
      bal t1 k v (remove_min_binding t2)

let create cmp = { cmp = cmp; map = Empty }
let empty = { cmp = compare; map = Empty }

let is_empty x =
	x.map = Empty

let add x d { cmp = cmp; map = map } =
  let rec loop = function
    | Node (l, k, v, r, h) ->
        let c = cmp x k in
        if c = 0 then Node (l, x, d, r, h)
        else if c < 0 then
          let nl = loop l in
          bal nl k v r
        else
          let nr = loop r in
          bal l k v nr
    | Empty -> Node (Empty, x, d, Empty, 1) in
  { cmp = cmp; map = loop map }

let find x { cmp = cmp; map = map } =
  let rec loop = function
    | Node (l, k, v, r, _) ->
        let c = cmp x k in
        if c < 0 then loop l
        else if c > 0 then loop r
        else v
    | Empty -> raise Not_found in
  loop map

let remove x { cmp = cmp; map = map } =
  let rec loop = function
    | Node (l, k, v, r, _) ->
        let c = cmp x k in
        if c = 0 then merge l r else
        if c < 0 then bal (loop l) k v r else bal l k v (loop r)
    | Empty -> Empty in
  { cmp = cmp; map = loop map }

let mem x { cmp = cmp; map = map } =
  let rec loop = function
    | Node (l, k, v, r, _) ->
        let c = cmp x k in
        c = 0 || loop (if c < 0 then l else r)
    | Empty -> false in
  loop map

let exists = mem

let iter f { map = map } =
  let rec loop = function
    | Empty -> ()
    | Node (l, k, v, r, _) -> loop l; f k v; loop r in
  loop map

let map f { cmp = cmp; map = map } =
  let rec loop = function
    | Empty -> Empty
    | Node (l, k, v, r, h) ->
	  let l = loop l in
	  let r = loop r in
	  Node (l, k, f v, r, h) in
  { cmp = cmp; map = loop map }

let mapi f { cmp = cmp; map = map } =
  let rec loop = function
    | Empty -> Empty
    | Node (l, k, v, r, h) ->
	  let l = loop l in
	  let r = loop r in
	  Node (l, k, f k v, r, h) in
  { cmp = cmp; map = loop map }

let fold f { cmp = cmp; map = map } acc =
  let rec loop acc = function
    | Empty -> acc
    | Node (l, k, v, r, _) ->
	  loop (f v (loop acc l)) r in
  loop acc map

let foldi f { cmp = cmp; map = map } acc =
  let rec loop acc = function
    | Empty -> acc
    | Node (l, k, v, r, _) ->
      loop (f k v (loop acc l)) r in
  loop acc map

type ('key,'a) iter = E | C of 'key * 'a * ('key,'a) map * ('key,'a) iter

let rec cardinal = function
  | Empty -> 0
  | Node(l, _, _, r, _) -> cardinal l + 1 + cardinal r

let rec cons_iter s t = match s with
  | Empty -> t
  | Node (l, k, v, r, _) -> cons_iter l (C (k, v, r, t))
    
let rec rev_cons_iter s t = match s with
  | Empty -> t
  | Node (l, k, v, r, _) -> rev_cons_iter r (C (k, v, l, t))

let rec enum_next l () = match !l with
    E -> raise BatEnum.No_more_elements
  | C (k, v, m, t) -> l := cons_iter m t; (k, v)
    
let rec enum_backwards_next l () = match !l with
    E -> raise BatEnum.No_more_elements
  | C (k, v, m, t) -> l := rev_cons_iter m t; (k, v)
    
let rec enum_count l () =
  let rec aux n = function
    | E -> n
    | C (_, _, m, t) -> aux (n + 1 + cardinal m) t
  in aux 0 !l
  
let enum t =
  let rec make l =
    let l = ref l in
    let clone() = make !l in
    BatEnum.make ~next:(enum_next l) ~count:(enum_count l) ~clone
  in make (cons_iter t.map E)
  
let backwards t =
  let rec make l =
    let l = ref l in
    let clone() = make !l in
    BatEnum.make ~next:(enum_backwards_next l) ~count:(enum_count l) ~clone
  in make (rev_cons_iter t.map E)

let keys    t = BatEnum.map fst (enum t)
let values  t = BatEnum.map snd (enum t)


(*let rec enum m =
  let rec make l =
    let l = ref l in
    let rec next() =
      match !l with
      | [] -> raise BatEnum.No_more_elements
      | Empty :: tl -> l := tl; next()
      | Node (m1, key, data, m2, h) :: tl ->
        l := m1 :: m2 :: tl;
        (key, data)
    in
    let count() =
      let n = ref 0 in
      let r = !l in
      try
        while true do
          ignore (next());
          incr n
        done;
        assert false
      with
		BatEnum.No_more_elements -> l := r; !n
    in
    let clone() = make !l in
	BatEnum.make ~next ~count ~clone
  in
  make [m.map]*)


let uncurry_add m (k, v) = add k v m
let of_enum ?(cmp = compare) e = BatEnum.fold uncurry_add (create cmp) e


let print ?(first="{\n") ?(last="\n}") ?(sep=",\n") print_k print_v out t =
  BatEnum.print ~first ~last ~sep (fun out (k,v) -> BatPrintf.fprintf out "%a: %a" print_k k print_v v) out (enum t)

let filter  f t = foldi (fun k a acc -> if f a then add k a acc else acc) t empty
let filteri f t = foldi (fun k a acc -> if f k a then add k a acc else acc) t empty
let filter_map f t = foldi (fun k a acc -> match f k a with
			     | None   -> acc
			     | Some v -> add k v acc)  t empty

let _choose = function
  | Empty -> invalid_arg "PMap.choose: empty tree"
  | Node (_l,k,v,_r,_h) -> (k,v)

let choose t = _choose t.map

let rec max_binding = function (* shadowed by below definition *)
  | Node (_, k, v, Empty, _) -> k, v
  | Node (_, _, _, r, _) -> max_binding r
  | Empty -> invalid_arg "PMap.max_binding: empty tree"

let max_binding t = max_binding t.map (* shadows earlier definition *)

let min_binding t = min_binding t.map (* shadows earlier definition *)

let singleton ?(cmp = compare) k v = add k v (create cmp)

let for_all f { cmp = cmp; map = map } =
  let rec loop = function
    | Empty -> true
    | Node (l, k, v, r, _) ->
	  f k v && loop l && loop r in
  loop map

let exists_f f { cmp = cmp; map = map } =
  let rec loop = function
    | Empty -> false
    | Node (l, k, v, r, _) ->
	  f k v || loop l || loop r in
  loop map

let partition f { cmp = cmp; map = map } =
  let rec loop m1 m2 = function
    | Empty -> (m1,m2)
    | Node (l, k, v, r, _) ->
	let m1, m2 = loop m1 m2 l in
	let m1, m2 = loop m1 m2 r in
	if f k v then
	  (add k v m1, m2)
	else
	  (m1, add k v m2)
  in
  loop (create cmp) (create cmp) map

let cardinal {cmp = cmp; map = map} =
  let rec loop acc = function
    | Empty -> acc
    | Node (l, _, _, r, _) ->
	loop (loop (acc+1) r) l
  in
  loop 0 map

let choose {cmp = cmp; map = map} =
  match map with
    | Empty -> raise Not_found
    | Node (_, k, v, _, _) -> (k,v)


let add_carry x d { cmp = cmp; map = map } =
  let rec loop = function
    | Node (l, k, v, r, h) ->
        let c = cmp x k in
        if c = 0 then Node (l, x, d, r, h), Some v
        else if c < 0 then
          let nl,carry = loop l in
          bal nl k v r, carry
        else
          let nr, carry = loop r in
          bal l k v nr, carry
    | Empty -> Node (Empty, x, d, Empty, 1), None in
  let map, carry = loop map in
  { cmp = cmp; map = map }, carry

let modify x f { cmp = cmp; map = map } =
  let rec loop = function
    | Node (l, k, v, r, h) ->
        let c = cmp x k in
        if c = 0 then Node (l, x, f v, r, h)
        else if c < 0 then
          let nl = loop l in
          bal nl k v r
        else
          let nr = loop r in
          bal l k v nr
    | Empty -> raise Not_found
  in
  { cmp = cmp; map = loop map }

let modify_def v0 x f { cmp = cmp; map = map } =
  let rec loop = function
    | Node (l, k, v, r, h) ->
        let c = cmp x k in
        if c = 0 then Node (l, x, f v, r, h)
        else if c < 0 then
          let nl = loop l in
          bal nl k v r
        else
          let nr = loop r in
          bal l k v nr
    | Empty -> Node (Empty, x, f v0, Empty, 1)
  in
  { cmp = cmp; map = loop map }


let extract x { cmp = cmp; map = map } =
  let rec loop = function
    | Node (l, k, v, r, _) ->
        let c = cmp x k in
        if c = 0 then v, merge l r else
        if c < 0 then
	  let vout, nl = loop l in
	  vout, bal nl k v r
	else
	  let vout, nr = loop r in
	  vout, bal l k v nr
    | Empty -> raise Not_found in
  let vout, nmap = loop map in
  vout, { cmp = cmp; map = nmap }

let pop {cmp = cmp; map = map} =
  match map with
    | Empty -> raise Not_found
    | Node (l, k, v, r, _) ->
	(k, v), {cmp = cmp; map = merge l r}

let rec join l x d r cmp =
  let m tree = {cmp = cmp; map = tree} in
  match (l, r) with
  | (Empty, _) -> (add x d (m r)).map
  | (_, Empty) -> (add x d (m l)).map
  | (Node(ll, lx, ld, lr, lh), Node(rl, rx, rd, rr, rh)) ->
      if lh > rh + 2 then
        bal ll lx ld (join lr x d r cmp)
      else if rh > lh + 2 then
        bal (join l x d rl cmp) rx rd rr
      else
        make l x d r

let rec split key map cmp =
  match map with
  | Empty -> (Empty, None, Empty)
  | Node(l, x, d, r, _) ->
      let c = cmp key x in
      if c = 0 then
        (l, Some d, r)
      else if c < 0 then
        let (ll, pres, rl) = split key l cmp in (ll, pres, join rl x d r cmp)
      else
        let (lr, pres, rr) = split key r cmp in (join l x d lr cmp, pres, rr)

let split key {cmp = cmp; map = map} =
  let ltree, ov, rtree = split key map cmp in
  let m tree = {cmp = cmp; map = tree} in
  m ltree, ov, m rtree

let union m1 m2 =
  foldi add m1 m2
(* TODO: use split/bal to merge similarly compared maps more efficiently *)

let diff m1 m2 =
  foldi (fun k _ acc -> remove k acc) m2 m1
    (* TODO: as union - use tree operations for large maps *)

module Exceptionless = 
struct
  let find k m = try Some (find k m) with Not_found -> None
end

module Infix =
struct
  let (-->) map key = find key map
  let (<--) map (key, value) = add key value map
end

let intersect merge m1 m2 = 
  foldi (fun k v acc -> try add k (merge v (find k m2)) acc
                        with Not_found -> acc) m1 (create m1.cmp)
  (* TODO: implement and compare with tree-based implementation *)
