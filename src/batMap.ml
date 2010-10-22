(* 
 * ExtMap - Additional map operations
 * Copyright (C) 1996 Xavier Leroy
 *               1996-2003 Nicolas Cannasse, Markus Mottl
 *               2009-2011 David Rajchenbach-Teller, Eric Norige, Gabriel Scherer 
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

(* A concrete implementation for the direct balanced maps structure,
   without carrying the ordering information with the data.

   This implementation directly expose the map structure, and should
   be the basis of both functorized Map and polymorphic PMap
   operations (both providing their own way to access the ordering
   information, and to possibly pass it along with the result).

   I tried to keep the interface mininal with respect to ordering
   information : function that do not need an order (they do not need
   to find the position of a specific key in the map) do not have
   a 'cmp' parameter.

   Most of those implementations are derived from Extlib's PMap
   module.

   Please keep in mind that our Map module currently relies on the
   fact that the (('k, 'v) Concrete.map) implementation is physically
   equal to stdlib's ('a Map.S.t). Changing Concrete.map is not a good
   idea.
*)
module Concrete = struct

  type ('k, 'v) map =
    | Empty
    | Node of ('k, 'v) map * 'k * 'v * ('k, 'v) map * int

  let height = function
    | Node (_, _, _, _, h) -> h
    | Empty -> 0

  let empty = Empty

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
  let create l x d r =
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
            create ll lv ld (create lr x d r)
          else begin
            match lr with
                Empty -> invalid_arg "Map.bal"
              | Node(lrl, lrv, lrd, lrr, _)->
                create (create ll lv ld lrl) lrv lrd (create lrr x d r)
          end
    end else if hr > hl + 2 then begin
      match r with
          Empty -> invalid_arg "Map.bal"
        | Node(rl, rv, rd, rr, _) ->
          if height rr >= height rl then
            create (create l x d rl) rv rd rr
          else begin
            match rl with
                Empty -> invalid_arg "Map.bal"
              | Node(rll, rlv, rld, rlr, _) ->
                create (create l x d rll) rlv rld (create rlr rv rd rr)
          end
    end else
        Node(l, x, d, r, (if hl >= hr then hl + 1 else hr + 1))

  let rec min_binding = function
    | Node (Empty, k, v, _, _) -> k, v
    | Node (l, _, _, _, _) -> min_binding l
    | Empty -> raise Not_found

  let rec max_binding = function
    | Node (_, k, v, Empty, _) -> k, v
    | Node (_, _, _, r, _) -> max_binding r
    | Empty -> invalid_arg "PMap.max_binding: empty tree"

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
    
  let add x d cmp map =
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
    loop map

  let find x cmp map =
    let rec loop = function
      | Node (l, k, v, r, _) ->
        let c = cmp x k in
        if c < 0 then loop l
        else if c > 0 then loop r
        else v
      | Empty -> raise Not_found in
    loop map

  let remove x cmp map =
    let rec loop = function
      | Node (l, k, v, r, _) ->
        let c = cmp x k in
        if c = 0 then merge l r else
          if c < 0 then bal (loop l) k v r else bal l k v (loop r)
      | Empty -> Empty in
    loop map

  let mem x cmp map =
    let rec loop = function
      | Node (l, k, v, r, _) ->
        let c = cmp x k in
        c = 0 || loop (if c < 0 then l else r)
      | Empty -> false in
    loop map

  let iter f map =
    let rec loop = function
      | Empty -> ()
      | Node (l, k, v, r, _) -> loop l; f k v; loop r in
    loop map

  let map f map =
    let rec loop = function
      | Empty -> Empty
      | Node (l, k, v, r, h) ->
        (* ensure evaluation in increasing order *)
        let l' = loop l in
        let v' = f v in
        let r' = loop r in
        Node (l', k, v', r', h) in
    loop map

  let mapi f map =
    let rec loop = function
      | Empty -> Empty
      | Node (l, k, v, r, h) ->
        (* ensure evaluation in increasing order *)
        let l' = loop l in
        let v' = f k v in
        let r' = loop r in
        Node (l', k, v', r', h) in
    loop map

  let fold f map acc =
    let rec loop acc = function
      | Empty -> acc
      | Node (l, k, v, r, _) ->
        loop (f v (loop acc l)) r in
    loop acc map

  let foldi f map acc =
    let rec loop acc = function
      | Empty -> acc
      | Node (l, k, v, r, _) ->
        loop (f k v (loop acc l)) r in
    loop acc map

  (* join and split also are from stdlib 3.12 *)
  (* Same as create and bal, but no assumptions are made on the
     relative heights of l and r. *)
  let rec join cmp l v d r =
    match (l, r) with
        (Empty, _) -> add v d cmp r
      | (_, Empty) -> add v d cmp l
      | (Node(ll, lv, ld, lr, lh), Node(rl, rv, rd, rr, rh)) ->
          if lh > rh + 2 then bal ll lv ld (join cmp lr v d r) else
          if rh > lh + 2 then bal (join cmp l v d rl) rv rd rr else
          create l v d r
  (* join doesn't actually need a comparison function [cmp]; [add] is
     only used here for convenience, in reality we could implement two
     add_min_binding and add_max_binding functions that could be used
     here, without needing any comparison function. *)
            
  let rec split key cmp = function
    | Empty -> (Empty, None, Empty)
    | Node(l, x, d, r, _) ->
      let c = cmp key x in
      if c = 0 then (l, Some d, r)
      else if c < 0 then
        let (ll, pres, rl) = split key cmp l in (ll, pres, join cmp rl x d r)
      else
        let (lr, pres, rr) = split key cmp r in (join cmp l x d lr, pres, rr)

  type ('key,'a) iter = E | C of 'key * 'a * ('key,'a) map * ('key,'a) iter

  let cardinal map =
    let rec loop acc = function
      | Empty -> acc
      | Node (l, _, _, r, _) ->
	loop (loop (acc+1) r) l
    in
    loop 0 map

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
    in make (cons_iter t E)
    
  let backwards t =
    let rec make l =
      let l = ref l in
      let clone() = make !l in
      BatEnum.make ~next:(enum_backwards_next l) ~count:(enum_count l) ~clone
    in make (rev_cons_iter t E)

  let keys    t = BatEnum.map fst (enum t)
  let values  t = BatEnum.map snd (enum t)

  let of_enum cmp e = BatEnum.fold (fun m (k, v) -> add k v cmp m) empty e

  let print ?(first="{\n") ?(last="\n}") ?(sep=",\n") print_k print_v out t =
    BatEnum.print ~first ~last ~sep (fun out (k,v) -> BatPrintf.fprintf out "%a: %a" print_k k print_v v) out (enum t)

  (*We rely on [fold] rather than on ['a implementation] to
    make future changes of implementation in the base
    library's version of [Map] easier to track, even if the
    result is a tad slower.*)
  let filter  f cmp t =
    foldi (fun k a acc -> if f a then add k a cmp acc else acc) t empty
  let filteri f cmp t =
    foldi (fun k a acc -> if f k a then add k a cmp acc else acc) t empty
  let filter_map f cmp t =
    foldi (fun k a acc -> match f k a with
      | None   -> acc
      | Some v -> add k v cmp acc) t empty

  let choose = function
    | Empty -> invalid_arg "PMap.choose: empty tree"
    | Node (_l,k,v,_r,_h) -> (k,v)

  let singleton x d = Node(Empty, x, d, Empty, 1)

  let for_all f map =
    let rec loop = function
      | Empty -> true
      | Node (l, k, v, r, _) ->
	f k v && loop l && loop r in
    loop map

  let exists f map =
    let rec loop = function
      | Empty -> false
      | Node (l, k, v, r, _) ->
	f k v || loop l || loop r in
    loop map

  let partition f cmp map =
    let rec loop m1 m2 = function
      | Empty -> (m1,m2)
      | Node (l, k, v, r, _) ->
	let m1, m2 = loop m1 m2 l in
	let m1, m2 = loop m1 m2 r in
	if f k v then
	  (add k v cmp m1, m2)
	else
	  (m1, add k v cmp m2)
    in
    loop empty empty map

  let choose = function
    | Empty -> raise Not_found
    | Node (_, k, v, _, _) -> (k,v)

  let add_carry x d cmp map =
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
    loop map

  let modify x f cmp map =
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
    loop map

  let modify_def v0 x f cmp map =
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
    loop map

  let extract x cmp map =
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
    loop map

  let pop map =
    match map with
      | Empty -> raise Not_found
      | Node (l, k, v, r, _) ->
	(k, v), merge l r

  let union m1 cmp2 m2 =
    foldi (fun k v acc -> add k v cmp2 acc) m1 m2
  (* TODO: use split/bal to merge similarly compared maps more efficiently *)

  let diff cmp1 m1 m2 =
    foldi (fun k _ acc -> remove k cmp1 acc) m2 m1
(* TODO: as union - use tree operations for large maps *)

  let intersect merge cmp1 m1 cmp2 m2 = 
    foldi (fun k v acc -> try add k (merge v (find k cmp2 m2)) cmp1 acc
                          with Not_found -> acc) m1 empty
  (* TODO: implement and compare with tree-based implementation *)
end


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

  val split : key -> 'a t -> ('a t * 'a option * 'a t)

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

      (* We break the abstraction of stdlib's Map module by exposing
         it's underlyding datatype, which is exactly ((key, 'a)
         Concrete.map). We therefore have O(1) conversion to and from
         Concrete, which allow us to add new features to the Map
         module while reusing stdlib's implementation (and, in fact,
         compiled code) for the old ones.

         If this was ever to be a problem, we could desynchronize our
         Map implementation from stdlib's, simply reusing Concrete
         implementations everywhere. Breaking this abstraction is not
         our fate, it's only a convenient choice for now.
      *)
  type 'a implementation = (key, 'a) Concrete.map

  external t_of_impl: 'a implementation -> 'a t = "%identity"
  external impl_of_t: 'a t -> 'a implementation = "%identity"

  type 'a iter = E | C of key * 'a * 'a implementation * 'a iter

  let cardinal t = Concrete.cardinal (impl_of_t t)
  let enum t = Concrete.enum (impl_of_t t)
  let backwards t = Concrete.backwards (impl_of_t t)
  let keys t = Concrete.keys (impl_of_t t)
  let values t = Concrete.values (impl_of_t t)

  let of_enum e = t_of_impl (Concrete.of_enum Ord.compare e)

  (* In Ocaml 3.11.2, the implementation of stdlib's Map.S.map(i) are
     slightly incorrect in that they don't apply their function
     parameter in increasing key order, as advertised in the
     documentation. This was fixed in 3.12.
     
     http://caml.inria.fr/mantis/view.php?id=4012

     We replace map(i) implementations with the ones derived from
     Concrete, to have the expected evaluation order even with 3.11.
  *)
  let mapi f t = t_of_impl (Concrete.mapi f (impl_of_t t))
  let map f t = t_of_impl (Concrete.map f (impl_of_t t))

  let print ?first ?last ?sep print_k print_v out t =
    Concrete.print ?first ?last ?sep print_k print_v out (impl_of_t t)

  let filter f t =
    t_of_impl (Concrete.filter f Ord.compare (impl_of_t t))
  let filteri f t =
    t_of_impl (Concrete.filteri f Ord.compare (impl_of_t t))
  let filter_map f t =
    t_of_impl (Concrete.filter_map f Ord.compare (impl_of_t t))

  let min_binding t = Concrete.min_binding (impl_of_t t)
  let max_binding t = Concrete.max_binding (impl_of_t t)

  let choose t = Concrete.choose (impl_of_t t)

  let split k t =
    let l, v, r = Concrete.split k Ord.compare (impl_of_t t) in
    (t_of_impl l, v, t_of_impl r)

  let modify x f m = t_of_impl (Concrete.modify x f Ord.compare (impl_of_t m))

  let modify_def v0 x f m =
    t_of_impl (Concrete.modify_def v0 x f Ord.compare (impl_of_t m))

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


(**
 * PMap - Polymorphic maps
 *)

type ('k, 'v) t =
  {
    cmp : 'k -> 'k -> int;
    map : ('k, 'v) Concrete.map;
  }

let create cmp = { cmp = cmp; map = Concrete.empty }
let empty = { cmp = compare; map = Concrete.empty }

let is_empty x = x.map = Concrete.Empty

let add x d m =
  { m with map = Concrete.add x d m.cmp m.map }

let find x m =
  Concrete.find x m.cmp m.map
  
let remove x m =
  { m with map = Concrete.remove x m.cmp m.map }

let mem x m =
  Concrete.mem x m.cmp m.map

let exists = mem

let iter f m =
  Concrete.iter f m.map

let map f m =
  { m with map = Concrete.map f m.map }

let mapi f m =
  { m with map = Concrete.mapi f m.map }

let fold f m acc =
  Concrete.fold f m.map acc

let foldi f m acc =
  Concrete.foldi f m.map acc

let enum t = Concrete.enum t.map
  
let backwards t = Concrete.backwards t.map

let keys    t = BatEnum.map fst (enum t)
let values  t = BatEnum.map snd (enum t)

let of_enum ?(cmp = compare) e =
  { cmp = cmp; map = Concrete.of_enum cmp e }

let print ?first ?last ?sep print_k print_v out t =
  Concrete.print ?first ?last ?sep print_k print_v out t.map

let filter  f t = { t with map = Concrete.filter f t.cmp t.map }
let filteri f t = { t with map = Concrete.filteri f t.cmp t.map }
let filter_map f t = { t with map = Concrete.filter_map f t.cmp t.map }

let choose t = Concrete.choose t.map

let max_binding t = Concrete.max_binding t.map
let min_binding t = Concrete.min_binding t.map

let singleton ?(cmp = compare) k v =
  { cmp = cmp; map = Concrete.singleton k v }

let for_all f m = Concrete.for_all f m.map

let exists_f f m = Concrete.exists f m.map

let partition f m =
  let l, r = Concrete.partition f m.cmp m.map in
  { m with map = l }, { m with map = r }

let cardinal m = Concrete.cardinal m.map

let choose m = Concrete.choose m.map

let add_carry x d m =
  let map', carry = Concrete.add_carry x d m.cmp m.map in
  { m with map = map' }, carry

let modify x f m =
  { m with map = Concrete.modify x f m.cmp m.map }

let modify_def v0 x f m =
  { m with map = Concrete.modify_def v0 x f m.cmp m.map } 

let extract x m =
  let out, map' = Concrete.extract x m.cmp m.map in
  out, { m with map = map' }

let pop m =
  let out, map' = Concrete.pop m.map in
  out, { m with map = map' }

let split k m =
  let (l, v, r) = Concrete.split k m.cmp m.map in
  { m with map = l }, v, { m with map = r }

let union m1 m2 =
  { m2 with map = Concrete.union m1.map m2.cmp m2.map }

let diff m1 m2 =
  { m1 with map = Concrete.diff m1.cmp m1.map m2.map }

let intersect merge m1 m2 =
  (* the use of 'empty' is strange here, but mimicks the older PMap implementation *)
  { empty with map = Concrete.intersect merge m1.cmp m1.map m2.cmp m2.map }

module Exceptionless = 
struct
  let find k m = try Some (find k m) with Not_found -> None
end

module Infix =
struct
  let (-->) map key = find key map
  let (<--) map (key, value) = add key value map
end
