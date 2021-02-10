(*
 * BatMap - Additional map operations
 * Copyright (C) 1996 Xavier Leroy
 *               1996-2003 Nicolas Cannasse, Markus Mottl
 *               2009-2011 David Rajchenbach-Teller, Edgar Friendly, Gabriel Scherer
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

   I tried to keep the interface minimal with respect to ordering
   information : function that do not need the ordering (they do not
   need to find the position of a specific key in the map) do not have
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

  let is_empty m =
    m = Empty

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

  let rec min_binding_opt = function
    | Node (Empty, k, v, _, _) -> Some (k, v)
    | Node (l, _, _, _, _) -> min_binding_opt l
    | Empty -> None

  let get_root = function
    | Empty -> raise Not_found
    | Node (_, k, v, _, _) -> k, v

  let pop_min_binding s =
    let mini = ref (get_root s) in
    let rec loop = function
      | Empty -> assert(false)  (* get_root already raises Not_found on empty map *)
      | Node(Empty, k, v, r, _) -> mini := (k, v); r
      | Node(l, k, v, r, _) -> bal (loop l) k v r
    in
    let others = loop s in
    (!mini, others)

  let rec max_binding = function
    | Node (_, k, v, Empty, _) -> k, v
    | Node (_, _, _, r, _) -> max_binding r
    | Empty -> raise Not_found

  let rec max_binding_opt = function
    | Node (_, k, v, Empty, _) -> Some (k, v)
    | Node (_, _, _, r, _) -> max_binding_opt r
    | Empty -> None

  let pop_max_binding s =
    let maxi = ref (get_root s) in
    let rec loop = function
      | Empty ->  assert(false)  (* get_root already raises Not_found on empty map *)
      | Node (l, k, v, Empty, _) -> maxi := (k, v); l
      | Node (l, k, v, r, _) -> bal l k v (loop r)
    in
    let others = loop s in
    (!maxi, others)

  let rec remove_min_binding = function
    | Node (Empty, _, _, r, _) -> r
    | Node (l, k, v, r, _) -> bal (remove_min_binding l) k v r
    | Empty -> raise Not_found

  let merge t1 t2 =
    match t1, t2 with
    | Empty, _ -> t2
    | _, Empty -> t1
    | _ ->
      let k, v = min_binding t2 in
      bal t1 k v (remove_min_binding t2)

  let add x d cmp map =
    let rec loop = function
      | Node (l, k, v, r, h) as node ->
        let c = cmp x k in
        if c = 0 then
          if d == v then
            node
          else
            Node (l, x, d, r, h)
        else if c < 0 then
          let nl = loop l in
          if nl == l then
            node 
          else
            bal nl k v r
        else
          let nr = loop r in
          if nr == r then
            node
          else
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

  let rec find_first_helper_found k0 v0 f = function
    | Empty -> (k0, v0)
    | Node (l, k, v, r, _) ->
       if f k
       then find_first_helper_found k v f l
       else find_first_helper_found k0 v0 f r

  let rec find_first f m =
    match m with
    | Empty -> raise Not_found
    | Node (l, k, v, r, _) ->
       if f k
       then find_first_helper_found k v f l
       else find_first f r

  let rec find_first_opt f m =
    match m with
    | Empty -> None
    | Node (l, k, v, r, _) ->
       if f k
       then Some (find_first_helper_found k v f l)
       else find_first_opt f r

  let rec find_last_helper_found k0 v0 f = function
    | Empty -> (k0, v0)
    | Node (l, k, v, r, _) ->
       if f k
       then find_last_helper_found k v f r
       else find_last_helper_found k0 v0 f l

  let rec find_last f m =
    match m with 
    | Empty -> raise Not_found
    | Node (l, k, v, r, _) ->
       if f k
       then find_last_helper_found k v f r
       else find_last f l

  let rec find_last_opt f m =
    match m with 
    | Empty -> None
    | Node (l, k, v, r, _) ->
       if f k
       then Some (find_last_helper_found k v f r)
       else find_last_opt f l

  (*$T find_first
              (empty |> add 1 11 |> add 2 12 |> add 3 13 |> find_first (fun x -> x >= 0)) = ((1, 11))
              (empty |> add 1 11 |> add 2 12 |> add 3 13 |> find_first (fun x -> x >= 1)) = ((1, 11))
              (empty |> add 1 11 |> add 2 12 |> add 3 13 |> find_first (fun x -> x >= 2)) = ((2, 12))
              (empty |> add 1 11 |> add 2 12 |> add 3 13 |> find_first (fun x -> x >= 3)) = ((3, 13))
    try ignore(empty |> add 1 11 |> add 2 12 |> add 3 13 |> find_first (fun x -> x >= 4)); false with Not_found -> true
    try ignore(empty |>                                     find_first (fun x -> x >= 3)); false with Not_found -> true
  *)

  (*$T find_first_opt
    (empty |> add 1 11 |> add 2 12 |> add 3 13 |> find_first_opt (fun x -> x >= 0)) = (Some (1, 11))
    (empty |> add 1 11 |> add 2 12 |> add 3 13 |> find_first_opt (fun x -> x >= 1)) = (Some (1, 11))
    (empty |> add 1 11 |> add 2 12 |> add 3 13 |> find_first_opt (fun x -> x >= 2)) = (Some (2, 12))
    (empty |> add 1 11 |> add 2 12 |> add 3 13 |> find_first_opt (fun x -> x >= 3)) = (Some (3, 13))
    (empty |> add 1 11 |> add 2 12 |> add 3 13 |> find_first_opt (fun x -> x >= 4)) = (None)
    (empty |>                                     find_first_opt (fun x -> x >= 3)) = (None)
  *)

  (*$T find_last
              (empty |> add 1 11 |> add 2 12 |> add 3 13 |> find_last (fun x -> x <= 1)) = (1, 11)
              (empty |> add 1 11 |> add 2 12 |> add 3 13 |> find_last (fun x -> x <= 2)) = (2, 12)
              (empty |> add 1 11 |> add 2 12 |> add 3 13 |> find_last (fun x -> x <= 3)) = (3, 13)
              (empty |> add 1 11 |> add 2 12 |> add 3 13 |> find_last (fun x -> x <= 4)) = (3, 13)
    try ignore(empty |> add 1 11 |> add 2 12 |> add 3 13 |> find_last (fun x -> x <= 0)); false with Not_found -> true
    try ignore(empty |>                                     find_last (fun x -> x <= 3)); false with Not_found -> true
  *)

  (*$T find_last_opt
    (empty |> add 1 11 |> add 2 12 |> add 3 13 |> find_last_opt (fun x -> x <= 0)) = None
    (empty |> add 1 11 |> add 2 12 |> add 3 13 |> find_last_opt (fun x -> x <= 1)) = Some (1, 11)
    (empty |> add 1 11 |> add 2 12 |> add 3 13 |> find_last_opt (fun x -> x <= 2)) = Some (2, 12)
    (empty |> add 1 11 |> add 2 12 |> add 3 13 |> find_last_opt (fun x -> x <= 3)) = Some (3, 13)
    (empty |> add 1 11 |> add 2 12 |> add 3 13 |> find_last_opt (fun x -> x <= 4)) = Some (3, 13)
    (empty |>                                     find_last_opt (fun x -> x <= 3)) = None
  *)

  let find_option x cmp map =
    try Some (find x cmp map)
    with Not_found -> None

  let find_default def x cmp map =
    try find x cmp map
    with Not_found -> def

  let remove x cmp map =
    let rec loop = function
      | Node (l, k, v, r, _) as node ->
        let c = cmp x k in
        if c = 0 then
          merge l r
        else if c < 0 then
          let nl = loop l in
          if nl == l then
            node
          else 
            bal nl k v r
        else
          let nr = loop r in
          if nr == r then
            node
          else
            bal l k v nr
      | Empty -> Empty in
    loop map

  (* A variant of [remove] that throws [Not_found] on failure *)
  let remove_exn x cmp map =
    let rec loop = function
      | Empty ->
          raise Not_found
      | Node (l, k, v, r, _) ->
          let c = cmp x k in
          if c = 0 then
            merge l r
          else if c < 0 then
            bal (loop l) k v r
          else
            bal l k v (loop r)
    in
    loop map

  let update k1 k2 v2 cmp map =
    if cmp k1 k2 <> 0 then
      add k2 v2 cmp (remove_exn k1 cmp map)
    else
      let rec loop = function
        | Empty -> raise Not_found
        | Node(l, k, v, r, h) as node ->
           let c = cmp k1 k in
           if c = 0 then
             if v == v2 && k == k2 then
               node
             else 
               Node(l, k2, v2, r, h)
           else if c < 0 then
             let nl = loop l in
             if nl == l then
               node
             else 
               Node(nl, k, v, r, h)
           else
             let nr = loop r in
             if nr == r then
               node
             else 
               Node(l, k, v, nr, h)
      in
      loop map

  let rec update_stdlib x f cmp = function
    |  Empty ->
        begin match f None with
        | None -> Empty
        | Some data -> Node(Empty, x, data, Empty, 1)
        end
    | Node (l, v, d, r, h) as m ->
       let c = cmp x v in
       if c = 0 then
         begin
           match f (Some d) with
           | None -> merge l r
           | Some data ->
              if d == data
              then m
              else Node(l, x, data, r, h)
         end
       else if c < 0 then
         let ll = update_stdlib x f cmp l in
         if l == ll
         then m
         else bal ll v d r
       else
         let rr = update_stdlib x f cmp r in
         if r == rr
         then m
         else bal l v d rr

  let mem x cmp map =
    let rec loop = function
      | Node (l, k, _v, r, _) ->
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
      | Node (l, _k, v, r, _) ->
        loop (f v (loop acc l)) r in
    loop acc map

  let foldi f map acc =
    let rec loop acc = function
      | Empty -> acc
      | Node (l, k, v, r, _) ->
        loop (f k v (loop acc l)) r in
    loop acc map

  exception Found

  let at_rank_exn i m =
    if i < 0 then invalid_arg "Map.at_rank_exn: i < 0";
    let res = ref (get_root m) in (* raises Not_found if empty *)
    try
      let (_: int) =
        foldi (fun k v j ->
            if j <> i then j + 1
            else begin
              res := (k, v);
              raise Found
            end
          ) m 0
      in
      invalid_arg "Map.at_rank_exn: i >= (Map.cardinal s)"
    with Found -> !res

  (*$T at_rank_exn
    (empty |> add 1 true |> at_rank_exn 0) = (1, true)
    (empty |> add 1 true |> add 2 false |> at_rank_exn 1) = (2, false)
    try ignore(at_rank_exn (-1) empty); false with Invalid_argument _ -> true
    try ignore(at_rank_exn 0 empty); false with Not_found -> true
    try ignore(add 1 true empty |> at_rank_exn 1); false with Invalid_argument _ -> true
  *)

  let singleton x d = Node(Empty, x, d, Empty, 1)

  (* beware : those two functions assume that the added k is *strictly*
     smaller (or bigger) than all the present keys in the tree; it
     does not test for equality with the current min (or max) key.

     Indeed, they are only used during the "join" operation which
     respects this precondition.
  *)
  let rec add_min_binding k v = function
    | Empty -> singleton k v
    | Node (l, x, d, r, _h) ->
      bal (add_min_binding k v l) x d r

  let rec add_max_binding k v = function
    | Empty -> singleton k v
    | Node (l, x, d, r, _h) ->
      bal l x d (add_max_binding k v r)

  (* Same as create and bal, but no assumptions are made on the
     relative heights of l and r.

     The stdlib implementation was changed to use the new
     [add_{min,max}_binding] functions instead of the [add] function
     that would require to pass a comparison function.  *)
  let rec join l v d r =
    match (l, r) with
      (Empty, _) -> add_min_binding v d r
    | (_, Empty) -> add_max_binding v d l
    | (Node(ll, lv, ld, lr, lh), Node(rl, rv, rd, rr, rh)) ->
      if lh > rh + 2 then bal ll lv ld (join lr v d r) else
      if rh > lh + 2 then bal (join l v d rl) rv rd rr else
        create l v d r

  (* split also is from stdlib 3.12 *)
  let rec split key cmp = function
    | Empty -> (Empty, None, Empty)
    | Node(l, x, d, r, _) ->
      let c = cmp key x in
      if c = 0 then (l, Some d, r)
      else if c < 0 then
        let (ll, pres, rl) = split key cmp l in (ll, pres, join rl x d r)
      else
        let (lr, pres, rr) = split key cmp r in (join l x d lr, pres, rr)

  type ('key,'a) iter = E | C of 'key * 'a * ('key,'a) map * ('key,'a) iter

  let cardinal map =
    let rec loop acc = function
      | Empty -> acc
      | Node (l, _, _, r, _) ->
        loop (loop (acc+1) r) l
    in
    loop 0 map

  let rec bindings_aux accu = function
    | Empty -> accu
    | Node(l, v, d, r, _) -> bindings_aux ((v, d) :: bindings_aux accu r) l

  let bindings s =
    bindings_aux [] s

  let rec cons_iter s t = match s with
    | Empty -> t
    | Node (l, k, v, r, _) -> cons_iter l (C (k, v, r, t))

  let rec rev_cons_iter s t = match s with
    | Empty -> t
    | Node (l, k, v, r, _) -> rev_cons_iter r (C (k, v, l, t))

  let rec cons_iter_from cmp k2 m e =
    match m with
    | Empty -> e
    | Node (l, k, v, r, _) ->
       if cmp k2 k <= 0
       then cons_iter_from cmp k2 l (C (k, v, r, e))
       else cons_iter_from cmp k2 r e

  let enum_next l () = match !l with
      E -> raise BatEnum.No_more_elements
    | C (k, v, m, t) -> l := cons_iter m t; (k, v)

  let enum_backwards_next l () = match !l with
      E -> raise BatEnum.No_more_elements
    | C (k, v, m, t) -> l := rev_cons_iter m t; (k, v)

  let enum_count l () =
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

  let print ?(first="{\n") ?(last="\n}") ?(sep=",\n") ?(kvsep=": ") print_k print_v out t =
    BatEnum.print ~first ~last ~sep (fun out (k,v) -> BatPrintf.fprintf out "%a%s%a" print_k k kvsep print_v v) out (enum t)

  (*We rely on [fold] rather than on ['a implementation] to
    make future changes of implementation in the base
    library's version of [Map] easier to track, even if the
    result is a tad slower.*)
  (* [filter{,i,_map} f t cmp] do not use [cmp] on [t], but only to
     build the result map. The unusual parameter order was chosen to
     reflect this.  *)
  let filterv f t cmp =
    foldi (fun k a acc -> if f a then acc else remove k cmp acc) t t
  let filter f t cmp =
    foldi (fun k a acc -> if f k a then acc else remove k cmp acc) t t
  let filter_map f t cmp =
    foldi (fun k a acc -> match f k a with
      | None   -> acc
      | Some v -> add k v cmp acc) t empty

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

  let choose = min_binding
  (*$= choose
    (empty |> add 0 1 |> add 1 1 |> choose) (empty |> add 1 1 |> add 0 1 |> choose)
   *)

  let choose_opt m =
    try Some (choose m)
    with Not_found -> None

  let any = function
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

  let modify_opt x f cmp map =
    let rec loop = function
      | Node (l, k, v, r, h) ->
        let c = cmp x k in
        if c = 0 then
          match f (Some v) with
          | None -> merge l r
          | Some v' -> Node (l, x, v', r, h)
        else if c < 0 then
          let nl = loop l in
          bal nl k v r
        else
          let nr = loop r in
          bal l k v nr
      | Empty ->
        match f None with
        | None   -> raise Exit (* fast exit *)
        | Some d -> Node (Empty, x, d, Empty, 1)
    in
    try loop map with Exit -> map

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

  (* Merge two trees l and r into one.
     All elements of l must precede the elements of r.
     No assumption on the heights of l and r. *)
  let concat t1 t2 =
    match (t1, t2) with
      (Empty, t) -> t
    | (t, Empty) -> t
    | (_, _) ->
      let (x, d) = min_binding t2 in
      join t1 x d (remove_min_binding t2)

  let concat_or_join t1 v d t2 =
    match d with
    | Some d -> join t1 v d t2
    | None -> concat t1 t2

  let merge f cmp12 s1 s2 =
    let rec loop s1 s2 =
      match (s1, s2) with
      | (Empty, Empty) -> Empty
      | (Node (l1, v1, d1, r1, h1), _) when h1 >= height s2 ->
        let (l2, d2, r2) = split v1 cmp12 s2 in
        (* TODO force correct evaluation order *)
        concat_or_join (loop l1 l2) v1 (f v1 (Some d1) d2) (loop r1 r2)
      | (_, Node (l2, v2, d2, r2, _h2)) ->
        let (l1, d1, r1) = split v2 cmp12 s1 in
        concat_or_join (loop l1 l2) v2 (f v2 d1 (Some d2)) (loop r1 r2)
      | _ ->
        assert false in
    loop s1 s2

  let merge_diverse f cmp1 s1 cmp2 s2 =
    (* This implementation does not presuppose that the comparison
       function of s1 and s2 are the same. It is necessary in the PMap
       case, were we can't enforce that the same comparison function is
       used on both maps.

       For consistency, we will always return a result built with the
       comparison function of [m1].

       The idea of the algorithm is the following : iterates on keys
       of (s1 union s2), computing the merge result for each
       f k (find_option k s1) (find_option k s2)
       , and adding values to the result s3 accordingly.

       The crucial point is that we need to iterate on both keys of s1
       and s2. There are several possible implementations :

       1. first build the union of the set of keys, then iterate on
       it.

       2. iterate on s1, then reiterate on s2 checking that the key
       wasn't already in s1

       3. iterate on s1, and remove keys from s2 during the traversal,
       then iterate on the remainder of s2.

       Method 1. allocates a temporary map the size of (s1 union s2),
       which I think is too costly. Method 3 may seem better than
       method 2 (as we only have at the end to iterate on the
       remaining keys, instead of dropping almost all keys because
       they were in s1 already), but is actually less efficient : the
       cost of removing is amortized during s1 traversal, but in
       effect we will, for all keys of s2, either remove it (in the
       first phase) or traverse it in the second phase. With method 2,
       we either ignore it or traverse it (both in the second
       phase). As removal induces rebalancing and allocation, it is
       indeed more costly.
       Method 2 only allocations and rebalancing are during the
       building of the final map : s1 and s2 are only looked at, never
       changed. This is optimal memory-wise.

       Those informal justifications ought to be tested with
       a concrete performance measurements, but the current benchmark
       methods, outside the module, don't make it easy to test
       Concrete values directly (as they're hidden by the interface).
       An old benchmark reports than method 2 is sensibly faster than
       method 1 : 2700 op/s vs 951 op/s on the test input.

       This algorithm is still sensibly slower than the 'merge'
       implementation using the same comparison on both maps : a 270%
       performance penalty has been measured (it runs three times
       slower).
    *)
    let first_phase_result =
      foldi (fun k v1 acc ->
        match f k (Some v1) (find_option k cmp2 s2) with
        | None -> acc
        | Some v3 -> add k v3 cmp1 acc)
        s1 empty in
    (* the second phase will return the result *)
    foldi (fun k v2 acc ->
      if mem k cmp1 s1 then acc
      else match f k None (Some v2) with
        | None -> acc
        | Some v3 -> add k v3 cmp1 acc)
      s2 first_phase_result

  (* Checks if a given map is "ordered" wrt. a given comparison
     function. This means that the key are ordered in strictly
     increasing order.

     If [ordered cmp s] holds, [cmp] can be used to search elements in
     the map *even* if it is not the original comparison function that
     was used to build the map; we know that the two comparison
     function "agree" on the present keys. Of course, adding an
     element with one or the other comparison function may break that
     relation.

     The [ordered] function will be useful to choose between different
     implementations having different comparison requirements. For
     example, the implementation of [merge] assuming both maps have
     the same comparison function is much faster than the
     implementation assuming heterogeneous maps. Before calling the
     heterogeneous implementation, one may first check if one of the
     comparison actually orders the other map, and in that case use
     the fast homogeneous implementation instead. This is the
     [heuristic_merge] function.
  *)
  let ordered cmp s =
    if s = Empty then true else
      try
        ignore
          (foldi (fun k _ last_k ->
             if cmp last_k k >= 0 then raise Exit
             else k)
             (remove_min_binding s)
             (fst (min_binding s)));
        true
      with Exit -> false

  (* Maps are considered compatible by their comparison function when either:
     - cmp1 and cmp2 are the *same* function (physical equality)
     - cmp1 is a correct ordering on m2 (see comment in [ordered]) *)
  let compatible_cmp cmp1 _m1 cmp2 m2 =
    cmp1 == cmp2 || ordered cmp1 m2

  (* We first try to see if the comparison functions are compatible.
     If they are, then we use the [merge] function instead of a much
     slower [merge_diverse].

     In the "same comparisons" case, we return a map ordered with
     the given comparison. In the other case, we arbitrarily use the
     comparison function of [m1]. *)
  let heuristic_merge f cmp1 m1 cmp2 m2 =
    if compatible_cmp cmp1 m1 cmp2 m2
    then merge f cmp1 m1 m2
    else merge_diverse f cmp1 m1 cmp2 m2

  (* Binary PMap operations;

     When the comparison function are compatible, we use an efficient
     merge-based implementation.

     Otherwise, we compute the result so that the return comparison
     function is the same as the first map parameter. *)
  let union cmp1 m1 cmp2 m2 =
    if compatible_cmp cmp1 m1 cmp2 m2 then
      let merge_fun _k a b = if a <> None then a else b in
      merge merge_fun cmp2 m2 m1
    else
      foldi (fun k v m -> add k v cmp1 m) m2 m1

  let diff cmp1 m1 cmp2 m2 =
    if compatible_cmp cmp1 m1 cmp2 m2 then
      let merge_fun _k a b = if b <> None then None else a in
      merge merge_fun cmp1 m1 m2
    else
      foldi (fun k _v m -> remove k cmp1 m) m2 m1

  let intersect f cmp1 m1 cmp2 m2 =
    if compatible_cmp cmp1 m1 cmp2 m2 then
      let merge_fun _k a b =
        match a, b with
        | Some v1, Some v2 -> Some (f v1 v2)
        | None, _ | _, None -> None in
      merge merge_fun cmp1 m1 m2
    else
      foldi (fun k v1 m ->
        match find_option k cmp2 m2 with
        | None -> m
        | Some v2 -> add k (f v1 v2) cmp1 m)
        m1 empty

  let add_seq cmp s m =
    BatSeq.fold_left
      (fun m (k, v) -> add k v cmp m)
      m
      s
    
  let of_seq cmp s =
    add_seq cmp s empty

  let rec seq_of_iter m () =
    match m with
    | E -> BatSeq.Nil
    | C(k, v, r, e) ->
       BatSeq.Cons ((k, v), seq_of_iter (cons_iter r e))
      
  let to_seq m =
    seq_of_iter (cons_iter m E)
      
  let to_seq_from cmp k m = 
    seq_of_iter (cons_iter_from cmp k m E)

  let union_stdlib f cmp1 m1 cmp2 m2 =
    let fwrap a b1 b2 =
      match b1, b2 with
      | Some b1, Some b2 -> f a b1 b2
      | x, None
        | None, x -> x in
    heuristic_merge fwrap cmp1 m1 cmp2 m2
    
  let compare ckey cval m1 m2 =
    BatEnum.compare (fun (k1,v1) (k2,v2) -> BatOrd.bin_comp ckey k1 k2 cval v1 v2) (enum m1) (enum m2)
  let equal ckey eq_val m1 m2 =
    BatEnum.equal (fun (k1,v1) (k2,v2) -> ckey k1 k2 = 0 && eq_val v1 v2) (enum m1) (enum m2)
end

module type OrderedType = BatInterfaces.OrderedType

module type S =
sig
  type key
  type (+'a) t
  val empty: 'a t
  val is_empty: 'a t -> bool
  val cardinal: 'a t -> int
  val add: key -> 'a -> 'a t -> 'a t
  val update_stdlib: key -> ('a option -> 'a option) -> 'a t -> 'a t
  val update: key -> key -> 'a -> 'a t -> 'a t
  val find: key -> 'a t -> 'a
  val find_opt: key -> 'a t -> 'a option
  val find_default: 'a -> key -> 'a t -> 'a
  val find_first: (key -> bool) -> 'a t -> key * 'a
  val find_first_opt: (key -> bool) -> 'a t -> (key * 'a) option
  val find_last: (key -> bool) -> 'a t -> key * 'a
  val find_last_opt: (key -> bool) -> 'a t -> (key * 'a) option
  val remove: key -> 'a t -> 'a t
  val remove_exn: key -> 'a t -> 'a t
  val modify: key -> ('a -> 'a) -> 'a t -> 'a t
  val modify_def: 'a -> key -> ('a -> 'a) -> 'a t -> 'a t
  val modify_opt: key -> ('a option -> 'a option) -> 'a t -> 'a t
  val extract : key -> 'a t -> 'a * 'a t
  val pop : 'a t -> (key * 'a) * 'a t
  val mem: key -> 'a t -> bool
  val iter: (key -> 'a -> unit) -> 'a t -> unit
  val map: ('a -> 'b) -> 'a t -> 'b t
  val mapi: (key -> 'a -> 'b) -> 'a t -> 'b t
  val fold: (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val filterv: ('a -> bool) -> 'a t -> 'a t
  val filter: (key -> 'a -> bool) -> 'a t -> 'a t
  val filter_map: (key -> 'a -> 'b option) -> 'a t -> 'b t
  val compare: ('a -> 'a -> int) -> 'a t -> 'a t -> int
  val equal: ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  val keys : _ t -> key BatEnum.t
  val values: 'a t -> 'a BatEnum.t
  val min_binding : 'a t -> (key * 'a)
  val min_binding_opt : 'a t -> (key * 'a) option
  val pop_min_binding: 'a t -> (key * 'a) * 'a t
  val max_binding : 'a t -> (key * 'a)    
  val max_binding_opt : 'a t -> (key * 'a) option
  val pop_max_binding: 'a t -> (key * 'a) * 'a t
  val choose : 'a t -> (key * 'a)
  val choose_opt : 'a t -> (key * 'a) option
  val any : 'a t -> (key * 'a)
  val split : key -> 'a t -> ('a t * 'a option * 'a t)
  val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
  val singleton : key -> 'a -> 'a t
  val bindings : 'a t -> (key * 'a) list
  val enum  : 'a t -> (key * 'a) BatEnum.t
  val backwards  : 'a t -> (key * 'a) BatEnum.t
  val of_enum: (key * 'a) BatEnum.t -> 'a t
  val for_all: (key -> 'a -> bool) -> 'a t -> bool
  val exists: (key -> 'a -> bool) -> 'a t -> bool
  val merge:
    (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t
  val union:
    (key -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
  val to_seq : 'a t -> (key * 'a) BatSeq.t
  val to_seq_from :  key -> 'a t -> (key * 'a) BatSeq.t
  val add_seq : (key * 'a) BatSeq.t -> 'a t -> 'a t
  val of_seq : (key * 'a) BatSeq.t -> 'a t

  (** {7 Printing}*)

  val print :  ?first:string -> ?last:string -> ?sep:string -> ?kvsep:string ->
    ('a BatInnerIO.output -> key -> unit) ->
    ('a BatInnerIO.output -> 'c -> unit) ->
    'a BatInnerIO.output -> 'c t -> unit
  module Exceptionless : sig
    val find: key -> 'a t -> 'a option
    val choose: 'a t -> (key * 'a) option
    val any: 'a t -> (key * 'a) option
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
    val filterv: f:('a -> bool) -> 'a t -> 'a t
    val filter: f:(key -> 'a -> bool) -> 'a t -> 'a t
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
     it's underlying datatype, which is exactly ((key, 'a)
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
  let update k1 k2 v2 t = t_of_impl (Concrete.update k1 k2 v2 Ord.compare (impl_of_t t))
  let update_stdlib k f m = t_of_impl (Concrete.update_stdlib k f Ord.compare (impl_of_t m))
  let find_default d k t = Concrete.find_default d k Ord.compare (impl_of_t t)
  let find_opt k t = Concrete.find_option k Ord.compare (impl_of_t t)
  let find_first     f t = Concrete.find_first     f (impl_of_t t)
  let find_first_opt f t = Concrete.find_first_opt f (impl_of_t t)
  let find_last      f t = Concrete.find_last      f (impl_of_t t)
  let find_last_opt  f t = Concrete.find_last_opt  f (impl_of_t t)

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

  let print ?first ?last ?sep ?kvsep print_k print_v out t =
    Concrete.print ?first ?last ?sep ?kvsep print_k print_v out (impl_of_t t)

  let filterv f t =
    t_of_impl (Concrete.filterv f (impl_of_t t) Ord.compare)
  let filter f t =
    t_of_impl (Concrete.filter f (impl_of_t t) Ord.compare)
  let filter_map f t =
    t_of_impl (Concrete.filter_map f (impl_of_t t) Ord.compare)

  let exists f t = Concrete.exists f (impl_of_t t)
  let for_all f t = Concrete.for_all f (impl_of_t t)

  let min_binding t = Concrete.min_binding (impl_of_t t)
  let pop_min_binding t =
    let mini, rest = Concrete.pop_min_binding (impl_of_t t) in
    (mini, t_of_impl rest)
  let max_binding t = Concrete.max_binding (impl_of_t t)
  let pop_max_binding t =
    let maxi, rest = Concrete.pop_max_binding (impl_of_t t) in
    (maxi, t_of_impl rest)

  let max_binding_opt t = Concrete.max_binding_opt (impl_of_t t)
  let min_binding_opt t = Concrete.min_binding_opt (impl_of_t t)

  let choose t = Concrete.choose (impl_of_t t)
  let choose_opt t = Concrete.choose_opt (impl_of_t t)
  let any t = Concrete.any (impl_of_t t)

  let split k t =
    let l, v, r = Concrete.split k Ord.compare (impl_of_t t) in
    (t_of_impl l, v, t_of_impl r)

  let partition p t =
    let l, r = Concrete.partition p Ord.compare (impl_of_t t) in
    (t_of_impl l, t_of_impl r)

  let remove_exn x m =
    t_of_impl (Concrete.remove_exn x Ord.compare (impl_of_t m))

  let modify x f m = t_of_impl (Concrete.modify x f Ord.compare (impl_of_t m))

  let modify_def v0 x f m =
    t_of_impl (Concrete.modify_def v0 x f Ord.compare (impl_of_t m))

  let modify_opt x f m =
    t_of_impl (Concrete.modify_opt x f Ord.compare (impl_of_t m))

  let extract k t =
    let (v, t') = Concrete.extract k Ord.compare (impl_of_t t) in
    (v, t_of_impl t')

  let pop t =
    let kv, t' = Concrete.pop (impl_of_t t) in
    kv, t_of_impl t'

  let singleton k v = t_of_impl (Concrete.singleton k v)

  let bindings t = Concrete.bindings (impl_of_t t)

  let union f m1 m2 = t_of_impl (Concrete.union_stdlib f Ord.compare (impl_of_t m1) Ord.compare (impl_of_t m2))

  let merge f t1 t2 =
    t_of_impl (Concrete.merge f Ord.compare (impl_of_t t1) (impl_of_t t2))

  let of_seq s = t_of_impl (Concrete.of_seq Ord.compare s)
  let add_seq s m = t_of_impl (Concrete.add_seq Ord.compare s (impl_of_t m))
  let to_seq m = Concrete.to_seq (impl_of_t m)
  let to_seq_from k m = Concrete.to_seq_from Ord.compare k (impl_of_t m)

  module Exceptionless =
  struct
    let find k t = try Some (find k t) with Not_found -> None
    let choose t = try Some (choose t) with Not_found -> None
    let any t = try Some (any t) with Not_found -> None
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
    let filterv ~f = filterv f
    let filter ~f = filter f
  end

end

module Int = Make (BatInt)
module Int32 = Make (BatInt32)
module Int64 = Make (BatInt64)
module Nativeint = Make (BatNativeint)
module Float = Make (BatFloat)
module Char = Make (BatChar)
module String = Make (BatString)

(**
 * PMap - Polymorphic maps
*)

type ('k, 'v) t = ('k, 'v) Concrete.map

let empty = Concrete.empty
let is_empty = Concrete.is_empty

(*$T is_empty
  is_empty empty
  not(is_empty (empty |> add 1 1))
 *)

let add x d m = Concrete.add x d Pervasives.compare m

let update k1 k2 v2 m = Concrete.update k1 k2 v2 Pervasives.compare m

let update_stdlib k f m = Concrete.update_stdlib k f Pervasives.compare m

(*$T update_stdlib
  let of_list l = of_enum (BatList.enum l) and cmp = Pervasives.( = ) in \
  equal cmp (update_stdlib 1 (fun x -> assert(x = Some 1); Some 3) (of_list [1,1; 2,2]))    (of_list [1,3;2,2])
  let of_list l = of_enum (BatList.enum l) and cmp = Pervasives.( = ) in \
  equal cmp (update_stdlib 3 (fun x -> assert(x = None);   Some 3) (of_list [1,1; 2,2]))    (of_list [1,1;2,2;3,3])
  let of_list l = of_enum (BatList.enum l) and cmp = Pervasives.( = ) in \
  equal cmp (update_stdlib 1 (fun x -> assert(x = Some 1); None)   (of_list [1,1; 2,2]))    (of_list [2,2])
  let of_list l = of_enum (BatList.enum l) in \
  let s = (of_list [1,1; 2,2]) in (update_stdlib 3 (fun x -> assert(x = None  ); None  ) s) == s
  let of_list l = of_enum (BatList.enum l) in \
  let s = (of_list [1,1; 2,2]) in (update_stdlib 1 (fun x -> assert(x = Some 1); Some 1) s) == s
 *)

let find x m = Concrete.find x Pervasives.compare m

(*$T add; find
  empty |> add 1 true |> add 2 false |> find 1
  empty |> add 1 true |> add 2 false |> find 2 |> not
  empty |> add 1 true |> add 2 false |> find 1
  empty |> add 1 true |> add 2 false |> find 2 |> not
  empty |> add 2 'y' |> add 1 'x' |> find 1 = 'x'
  empty |> add 2 'y' |> add 1 'x' |> find 2 = 'y'
*)

let find_opt x m = Concrete.find_option x Pervasives.compare m

(*$T find_opt
    find_opt  4 (add 1 2 empty) = None
    find_opt 1 (add 1 2 empty) = Some 2
*)

let find_default def x m =
  Concrete.find_default def x Pervasives.compare m

(*$T find_default
    find_default 3 4 (add 1 2 empty) = 3
    find_default 3 1 (add 1 2 empty) = 2
 *)

let find_first f map = Concrete.find_first f map
let find_first_opt f map = Concrete.find_first_opt f map
let find_last f map = Concrete.find_last f map
let find_last_opt f map = Concrete.find_last_opt f map

(*$Q find ; add
  (Q.list Q.small_int) (fun xs -> \
  let of_list xs y m0 = List.fold_left (fun acc x -> add x y acc) m0 xs in \
  of_list (List.filter ((<>) 100) xs) false (singleton 100 true) |> find 100)
*)


let remove x m = Concrete.remove x Pervasives.compare m

(*$Q add ; remove
  (Q.list Q.small_int) (fun xs -> \
  let of_list xs y m0 = List.fold_left (fun acc x -> add x y acc) m0 xs in \
  List.fold_left (fun acc x -> remove x acc) (of_list xs true empty) xs |> is_empty)
*)

let remove_exn x m = Concrete.remove_exn x Pervasives.compare m

(*$Q add ; remove_exn
  (Q.list Q.small_int) (fun xs -> \
  let xs = List.unique xs in \
  let of_list xs y m0 = List.fold_left (fun acc x -> add x y acc) m0 xs in \
  List.fold_left (fun acc x -> remove_exn x acc) (of_list xs true empty) xs |> is_empty)
*)
(*$T remove_exn
  try remove_exn 1 empty |> ignore ; false with Not_found -> true
*)

let mem x m = Concrete.mem x Pervasives.compare m

let iter = Concrete.iter
let map = Concrete.map
let mapi = Concrete.mapi
let fold = Concrete.fold
let foldi = Concrete.foldi
let at_rank_exn = Concrete.at_rank_exn

(*$Q foldi
  (Q.list Q.small_int) (fun xs -> \
  let m = List.fold_left (fun acc x -> add x true acc) empty xs in \
  foldi (fun x _y acc -> x :: acc) m [] |> List.rev = List.sort_unique BatInt.compare xs)
*)

let enum = Concrete.enum

(*$Q keys
  (Q.list Q.small_int) (fun xs -> \
  List.fold_left (fun acc x -> add x true acc) \
    empty xs |> keys |> List.of_enum \
  = List.sort_unique BatInt.compare xs)
*)

let backwards = Concrete.backwards

let keys    t = BatEnum.map fst (enum t)
let values  t = BatEnum.map snd (enum t)

let of_enum e = Concrete.of_enum Pervasives.compare e

let print = Concrete.print

let filterv  f t = Concrete.filterv f t Pervasives.compare
let filter f t = Concrete.filter f t Pervasives.compare
let filter_map f t = Concrete.filter_map f t Pervasives.compare

let choose = Concrete.choose
let choose_opt = Concrete.choose_opt
let any = Concrete.any
let max_binding = Concrete.max_binding
let min_binding = Concrete.min_binding
let max_binding_opt = Concrete.max_binding_opt
let min_binding_opt = Concrete.min_binding_opt
let pop_min_binding = Concrete.pop_min_binding
let pop_max_binding = Concrete.pop_max_binding
                    
(*$T pop_min_binding
  (empty |> add 1 true |> pop_min_binding) = ((1, true), empty)
  (empty |> add 1 true |> add 2 false |> pop_min_binding) = \
  ((1, true), add 2 false empty)
  try ignore (pop_min_binding empty); false with Not_found -> true
*)

(*$T pop_max_binding
  (empty |> add 1 true |> pop_max_binding) = ((1, true), empty)
  (empty |> add 1 true |> add 2 false |> pop_max_binding) = \
  ((2, false), add 1 true empty)
  try ignore (pop_max_binding empty); false with Not_found -> true
*)

(*$T choose
  let of_list l = of_enum (BatList.enum l) in \
  (1,1) = choose (of_list [1,1])
  try ignore(choose empty); false with Not_found -> true
 *)
                    
(*$T choose_opt
  let of_list l = of_enum (BatList.enum l) in \
  Some (1,1) = choose_opt (of_list [1,1])
  None = choose_opt (empty)
 *)

(*$T max_binding
  let of_list l = of_enum (BatList.enum l) in \
  (3,3) = max_binding (of_list [1,1;2,2;3,3])
  try ignore(max_binding empty); false with Not_found -> true
 *)
                    
(*$T max_binding_opt
  let of_list l = of_enum (BatList.enum l) in \
  Some (3,3) = max_binding_opt (of_list [1,1;2,2;3,3])
  None = max_binding_opt empty
 *)
                    
(*$T min_binding
  let of_list l = of_enum (BatList.enum l) in \
  (1,1) = min_binding (of_list [1,1;2,2;3,3])
  try ignore(min_binding empty); false with Not_found -> true
 *)
                    
(*$T min_binding_opt
  let of_list l = of_enum (BatList.enum l) in \
  Some (1,1) = min_binding_opt (of_list [1,1;2,2;3,3])
  None = min_binding_opt empty
 *)

(*$T add
  let s = empty |> add 1 1 |> add 2 2 in s == (s |> add 2 2)
  *)

(*$T remove
  let s = empty |> add 1 1 |> add 2 2 in s == (s |> remove 4)
  *)

(*$T update
  let s = empty |> add 1 1 |> add 2 2 in \
  s == (s |> update 2 2 2)
  *)

(*$T update_stdlib
  let s = empty |> add 1 1 |> add 2 2 in \
  s == (s |> update_stdlib 2 (fun _ -> Some 2))
  *)

(*$T filter
  let s = empty |> add 1 1 |> add 2 2 in \
  s == (filter (fun _ _ -> true) s)
  *)

                    
let of_seq s =
  Concrete.of_seq Pervasives.compare s

let add_seq s m =
  Concrete.add_seq Pervasives.compare s m

let to_seq = Concrete.to_seq

let to_seq_from x m =
  Concrete.to_seq_from Pervasives.compare x m

let union_stdlib f m1 m2 = Concrete.union_stdlib f Pervasives.compare m1 Pervasives.compare m2
(*$T union_stdlib
  let cmp = Pervasives.( = ) in \
  equal cmp (union_stdlib (fun _ -> failwith "must not be called") empty empty) empty
  let of_list l = of_enum (BatList.enum l) and cmp = Pervasives.( = ) in \
  equal cmp (union_stdlib (fun _ -> failwith "must not be called") (of_list [1,1;2,2]) empty) (of_list [1,1;2,2])
  let of_list l = of_enum (BatList.enum l) and cmp = Pervasives.( = ) in \
  equal cmp (union_stdlib (fun _ -> failwith "must not be called") empty (of_list [1,1;2,2])) (of_list [1,1;2,2])
  let of_list l = of_enum (BatList.enum l) and cmp = Pervasives.( = ) in \
  equal cmp (union_stdlib (fun _ -> failwith "must not be called") (of_list [3,3;4,4]) (of_list [1,1;2,2])) (of_list [1,1;2,2;3,3;4,4])
 *)

let singleton k v = Concrete.singleton k v

let for_all = Concrete.for_all
let exists = Concrete.exists

let partition f m = Concrete.partition f Pervasives.compare m
let cardinal = Concrete.cardinal

let split k m = Concrete.split k Pervasives.compare m

let add_carry x d m = Concrete.add_carry x d Pervasives.compare m
let modify x f m = Concrete.modify x f Pervasives.compare m

let modify_def v0 x f m = Concrete.modify_def v0 x f Pervasives.compare m

let modify_opt x f m = Concrete.modify_opt x f Pervasives.compare m
(*$T modify_opt
  empty |> add 1 false |> \
  modify_opt 1 (function Some false -> Some true | _ -> assert false) |> \
  find 1
  empty |> add 1 true |> \
  modify_opt 1 (function Some true -> None | _ -> assert false) |> \
  mem 1 |> not
*)

let extract x m = Concrete.extract x Pervasives.compare m

let pop = Concrete.pop

let split k m = Concrete.split k Pervasives.compare m

(* We can't compare external primitives directly using the physical equality
   operator, since two different occurrences of an external primitive are two
   different closures. So we first make a local binding of [Pervasives.compare]
   and only then pass it to corresponding functions from Concrete. This way the
   physical equality check in [compatible_cmp] will work as needed *)

let union m1 m2 =
  let comp = Pervasives.compare in
  Concrete.union comp m1 comp m2

(*$T union
  let m1 = empty |> add 1 1 |> add 2 2 in \
  let m2 = empty |> add 2 20 |> add 3 30 in \
  (union m1 m2 |> find 2 = 20) && (union m2 m1 |> find 2 = 2)
 *)

let union_stdlib f m1 m2 =
  Concrete.union_stdlib f Pervasives.compare m1 Pervasives.compare m2

let diff m1 m2 =
  let comp = Pervasives.compare in
  Concrete.diff comp m1 comp m2

let intersect merge m1 m2 =
  let comp = Pervasives.compare in
  Concrete.intersect merge comp m1 comp m2


let merge f m1 m2 = Concrete.merge f Pervasives.compare m1 m2

let bindings = Concrete.bindings

let compare cmp_val m1 m2 =
  Concrete.compare Pervasives.compare Pervasives.compare m1 m2

let equal eq_val m1 m2 = Concrete.equal Pervasives.compare eq_val m1 m2

module Exceptionless =
struct
  let find k m = try Some (find k m) with Not_found -> None
  let choose m = try Some (choose m) with Not_found -> None
  let any m = try Some (any m) with Not_found -> None
end

module Infix =
struct
  let (-->) map key = find key map
  let (<--) map (key, value) = add key value map
end

include Infix

module PMap = struct (*$< PMap *)
  (**
     * PMap - Polymorphic maps
  *)

  type ('k, 'v) t =
    {
      cmp : 'k -> 'k -> int;
      map : ('k, 'v) Concrete.map;
    }

  let create cmp = { cmp = cmp; map = Concrete.empty }
  let get_cmp {cmp; _} = cmp

  (*$T get_cmp
    get_cmp (create BatInt.compare) == BatInt.compare
  *)

  let empty = { cmp = Pervasives.compare; map = Concrete.empty }
  let get_cmp {cmp; _} = cmp
  let is_empty x = x.map = Concrete.Empty

  let add x d m =
    let newmap = Concrete.add x d m.cmp m.map in
    if newmap == m.map
    then m
    else { m with map = newmap }

  let update k1 k2 v2 m =
    let newmap = Concrete.update k1 k2 v2 m.cmp m.map in
    if newmap == m.map
    then m
    else { m with map = newmap }

  let update_stdlib k f m =
    let newmap = Concrete.update_stdlib k f m.cmp m.map in
    if newmap == m.map
    then m
    else { m with map = newmap }

  let find x m =
    Concrete.find x m.cmp m.map

  let find_opt x m =
    Concrete.find_option x m.cmp m.map

  let find_default def x m =
    Concrete.find_default def x m.cmp m.map

  (*$T add; find
    empty |> add 1 true |> add 2 false |> find 1
    empty |> add 1 true |> add 2 false |> find 2 |> not
    create BatInt.compare |> add 1 true |> add 2 false |> find 1
    create BatInt.compare |> add 1 true |> add 2 false |> find 2 |> not
    empty |> add 2 'y' |> add 1 'x' |> find 1 = 'x'
    empty |> add 2 'y' |> add 1 'x' |> find 2 = 'y'
  *)

  (*$T find_default
    find_default 3 4 (add 1 2 empty) = 3
    find_default 3 1 (add 1 2 empty) = 2
  *)

  let find_first f map = Concrete.find_first f map.map
  let find_first_opt f map = Concrete.find_first_opt f map.map
  let find_last f map = Concrete.find_last f map.map
  let find_last_opt f map = Concrete.find_last_opt f map.map

    
  (*$T update
    add 1 false empty |> update 1 1 true |> find 1
    add 1 false empty |> update 1 2 true |> find 2
    try ignore (update 1 1 false empty); false with Not_found -> true
    empty |> add 1 11 |> add 2 22 |> update 2 2 222 |> find 2 = 222
    let m = empty |> add 1 11 |> add 2 22 in \
    try ignore (m |> update 3 4 555); false with Not_found -> true
  *)

  (*$Q find ; add
    (Q.list Q.small_int) (fun xs -> \
    let of_list xs y m0 = List.fold_left (fun acc x -> add x y acc) m0 xs in \
    of_list (List.filter ((<>) 100) xs) false (singleton 100 true) |> find 100)
  *)


  let remove x m =
    { m with map = Concrete.remove x m.cmp m.map }

  (*$Q add ; remove
    (Q.list Q.small_int) (fun xs -> \
    let of_list xs y m0 = List.fold_left (fun acc x -> add x y acc) m0 xs in \
    List.fold_left (fun acc x -> remove x acc) (of_list xs true empty) xs |> is_empty)
  *)
  let remove_exn x m =
    { m with map = Concrete.remove_exn x m.cmp m.map }

  (*$Q add ; remove_exn
    (Q.list Q.small_int) (fun xs -> \
    let xs = List.unique xs in \
    let of_list xs y m0 = List.fold_left (fun acc x -> add x y acc) m0 xs in \
    List.fold_left (fun acc x -> remove_exn x acc) (of_list xs true empty) xs |> is_empty)
  *)
  (*$T remove_exn
    add 1 false empty |> remove_exn 1 |> mem 1 |> not
    try remove_exn 1 empty |> ignore ; false with Not_found -> true
  *)

  let mem x m =
    Concrete.mem x m.cmp m.map

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

  (*$Q foldi
    (Q.list Q.small_int) (fun xs -> \
    let m = List.fold_left (fun acc x -> add x true acc) (create BatInt.compare) xs in \
    foldi (fun x _y acc -> x :: acc) m [] |> List.rev = List.sort_unique BatInt.compare xs)
  *)

  let at_rank_exn i m =
    Concrete.at_rank_exn i m.map

  let enum t = Concrete.enum t.map

  (*$Q keys
    (Q.list Q.small_int) (fun xs -> \
    List.fold_left (fun acc x -> add x true acc) \
    (create BatInt.compare) xs |> keys |> List.of_enum \
    = List.sort_unique BatInt.compare xs)
  *)

  let backwards t = Concrete.backwards t.map

  let keys    t = BatEnum.map fst (enum t)
  let values  t = BatEnum.map snd (enum t)

  let of_enum ?(cmp = Pervasives.compare) e =
    { cmp = cmp; map = Concrete.of_enum cmp e }

  let print ?first ?last ?sep ?kvsep print_k print_v out t =
    Concrete.print ?first ?last ?sep ?kvsep print_k print_v out t.map

  let filterv  f t = { t with map = Concrete.filterv f t.map t.cmp }
  let filter_map f t = { t with map = Concrete.filter_map f t.map t.cmp }
  let filter f t =
    let newmap =  Concrete.filter f t.map t.cmp in
    if newmap == t.map
    then t
    else { t with map = newmap }

  let max_binding t = Concrete.max_binding t.map
  let min_binding t = Concrete.min_binding t.map
  let max_binding_opt t = Concrete.max_binding_opt t.map
  let min_binding_opt t = Concrete.min_binding_opt t.map
  let pop_min_binding m =
    let mini, rest = Concrete.pop_min_binding m.map in
    (mini, { m with map = rest })
  let pop_max_binding m =
    let maxi, rest = Concrete.pop_max_binding m.map in
    (maxi, { m with map = rest })

  let singleton ?(cmp = Pervasives.compare) k v =
    { cmp = cmp; map = Concrete.singleton k v }

  let for_all f m = Concrete.for_all f m.map

  let exists f m = Concrete.exists f m.map

  let partition f m =
    let l, r = Concrete.partition f m.cmp m.map in
    { m with map = l }, { m with map = r }

  let cardinal m = Concrete.cardinal m.map

  let choose m = Concrete.choose m.map
  let choose_opt m = Concrete.choose_opt m.map
  let any m = Concrete.any m.map

  let split k m =
    let (l, v, r) = Concrete.split k m.cmp m.map in
    { m with map = l }, v, { m with map = r }

  let add_carry x d m =
    let map', carry = Concrete.add_carry x d m.cmp m.map in
    { m with map = map' }, carry

  let modify x f m =
    { m with map = Concrete.modify x f m.cmp m.map }

  let modify_def v0 x f m =
    { m with map = Concrete.modify_def v0 x f m.cmp m.map }

  let modify_opt x f m =
    { m with map = Concrete.modify_opt x f m.cmp m.map }

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
    { m1 with map = Concrete.union m1.cmp m1.map m2.cmp m2.map }

  let diff m1 m2 =
    { m1 with map = Concrete.diff m1.cmp m1.map m2.cmp m2.map }

  let intersect merge m1 m2 =
    { m1 with map = Concrete.intersect merge m1.cmp m1.map m2.cmp m2.map }

  let merge f m1 m2 =
    { m1 with map = Concrete.heuristic_merge f m1.cmp m1.map m2.cmp m2.map }

  let merge_unsafe f m1 m2 =
    { m1 with map = Concrete.merge f m1.cmp m1.map m2.map }

  let of_seq ?(cmp = Pervasives.compare) s =
    { map = Concrete.of_seq cmp s; cmp = cmp }
    
  let to_seq m = Concrete.to_seq m.map
             
  let to_seq_from k m =
    Concrete.to_seq_from m.cmp k m.map
                      
  let add_seq s m =
    { m with map = Concrete.add_seq m.cmp s m.map }

  let union_stdlib f m1 m2 =
    { m1 with map = Concrete.union_stdlib f m1.cmp m1.map m2.cmp m2.map }

  let bindings m =
    Concrete.bindings m.map

  let compare cmp_val m1 m2 = Concrete.compare m1.cmp cmp_val m1.map m2.map
  let equal eq_val m1 m2 = Concrete.equal m1.cmp eq_val m1.map m2.map

  module Exceptionless =
  struct
    let find k m = try Some (find k m) with Not_found -> None
    let choose m = try Some (choose m) with Not_found -> None
    let any m = try Some (any m) with Not_found -> None
  end

  module Infix =
  struct
    let (-->) map key = find key map
    let (<--) map (key, value) = add key value map
  end

  include Infix
end (*$>*)
