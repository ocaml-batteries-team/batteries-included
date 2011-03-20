(*
 * Splay -- splay trees
 * Copyright (C) 2011  Batteries Included Development Team
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

module List = struct include List include BatList end
module Enum = BatEnum

type 'a bst = Empty | Node of 'a bst * 'a * 'a bst

let size =
  let rec count tr k = match tr with
    | Empty -> k 0
    | Node (l, _, r) ->
        count l (fun m -> count r (fun n -> k (1 + m + n)))
  in
  fun tr -> count tr (fun n -> n)

let bst_append l r =
  let rec cat = function
    | Empty -> r
    | Node (l, x, r) -> Node (l, x, cat r)
  in
  cat l

type 'a step =
  | Left  of 'a * 'a bst
  | Right of 'a bst * 'a

type 'a cursor = C of 'a step list * 'a bst

let rec top' cx t = match cx with
  | [] -> t
  | (Left (p, pr) :: cx) ->
      top' cx (Node (t, p, pr))
  | (Right (pl, p) :: cx) ->
      top' cx (Node (pl, p, t))

let top (C (cx, t)) = top' cx t

let rec csplay' cx l x r = match cx with
  | [] ->
      Node (l, x, r)
  | [Left (p, pr)] ->
      Node (l, x, Node (r, p, pr))
  | [Right (pl, px)] ->
      Node (Node (pl, px, l), x, r)
  | (Left (px, pr) :: Left (ppx, ppr) :: cx) ->
      (* zig zig *)
      let r = Node (r, px, Node (pr, ppx, ppr)) in
      csplay' cx l x r
  | (Left (px, pr) :: Right (ppl, ppx) :: cx) ->
      (* zig zag *)
      let l = Node (ppl, ppx, l) in
      let r = Node (r, px, pr) in
      csplay' cx l x r
  | (Right (pl, px) :: Right (ppl, ppx) :: cx) ->
      (* zig zig *)
      let l = Node (Node (ppl, ppx, pl), px, l) in
      csplay' cx l x r
  | (Right (pl, px) :: Left (ppx, ppr) :: cx) ->
      (* zig zag *)
      let l = Node (pl, px, l) in
      let r = Node (r, ppx, ppr) in
      csplay' cx l x r

let csplay = function
  | C (cx, Node (l, x, r)) -> csplay' cx l x r
  | _ -> raise Not_found

let rec cfind ?(cx=[]) ~sel = function
  | Empty -> C (cx, Empty)
  | Node (l, x, r) ->
      let sx = sel x in
      if sx = 0 then C (cx, Node (l, x, r))
      else if sx < 0 then cfind ~cx:(Left (x, r) :: cx) ~sel l
      else cfind ~cx:(Right (l, x) :: cx) ~sel r

module Map (Ord : BatInterfaces.OrderedType)
  : BatMap.S with type key = Ord.t =
struct

  type key = Ord.t

  type 'a t = Map of (key * 'a) bst

  let empty = Map Empty

  let is_empty (Map tr) = tr = Empty

  let kcmp (j, _) (k, _) = Ord.compare j k
  let ksel j (k, _) = Ord.compare j k

  let singleton' k v = Node (Empty, (k, v), Empty)
  let singleton k v = Map (singleton' k v)

  let add k v (Map tr) = Map begin
    csplay begin
      match cfind ~sel:(ksel k) tr with
        | C (cx, Node (l, (k, _), r)) -> C (cx, Node (l, (k, v), r))
        | C (cx, Empty) -> C (cx, singleton' k v)
    end
  end

  let modify k fn (Map tr) = Map begin
    csplay begin
      match cfind ~sel:(ksel k) tr with
        | C (cx, Node (l, (k, v), r)) -> C (cx, Node (l, (k, fn v), r))
        | C (cx, Empty) -> raise Not_found
    end
  end

  let modify_def def k fn (Map tr) = Map begin
    csplay begin
      match cfind ~sel:(ksel k) tr with
        | C (cx, Node (l, (k, v), r)) -> C (cx, Node (l, (k, fn v), r))
        | C (cx, Empty) -> C (cx, singleton' k (fn def))
    end
  end

  let find k (Map tr as m) =
    let tr = csplay (cfind ~sel:(ksel k) tr) in
    match tr with
      | Node (_, (_, v), _) ->
          Obj.set_field (Obj.repr m) 0 (Obj.repr tr) ;
          v
      | _ -> raise Not_found

  let cchange fn (C (cx, t)) = C (cx, fn t)

  let remove k (Map tr) =
    let replace = function
      | Empty -> Empty
      | Node (l, _, r) -> bst_append l r
    in
    let tr = top (cchange replace (cfind ~sel:(ksel k) tr)) in
    Map tr

  let mem k m =
    try ignore (find k m) ; true with Not_found -> false

  let iter fn (Map tr) =
    let rec visit = function
      | Empty -> ()
      | Node (l, (k, v), r) ->
          visit l ;
          fn k v ;
          visit r
    in
    visit tr

  let fold fn (Map tr) acc =
    let rec visit acc = function
      | Empty -> acc
      | Node (l, (k, v), r) ->
          let acc = visit acc l in
          let acc = fn k v acc in
          visit acc r
    in
    visit acc tr

  let choose (Map tr) = match tr with
    | Empty -> raise Not_found
    | Node (_, kv, _) -> kv

  let min_binding (Map tr) =
    let rec bfind = function
      | Node (Empty, kv, _) -> kv
      | Node (l, _, _) -> bfind l
      | Empty -> raise Not_found
    in
    bfind tr

  let max_binding (Map tr) =
    let rec bfind = function
      | Node (_, kv, Empty) -> kv
      | Node (_, _, r) -> bfind r
      | Empty -> raise Not_found
    in
    bfind tr

  let filter_map (f : key -> 'a -> 'b option) : 'a t -> 'b t =
    let rec visit t cont = match t with
      | Empty -> cont Empty
      | Node (l, (k, v), r) ->
        visit l begin fun l ->
          let w = f k v in
          visit r begin fun r ->
            match w with
              | None -> cont (bst_append l r)
              | Some w ->
                cont (Node (l, (k, w), r))
          end
        end
    in
    fun (Map tr) -> visit tr (fun tr -> Map tr)

  let filter f t =
    filter_map (fun _ v -> if f v then Some v else None) t

  let filteri f t =
    filter_map (fun k v -> if f k v then Some v else None) t

  let map f t = filter_map (fun _ v -> Some (f v)) t

  let mapi f t = filter_map (fun k v -> Some (f k v)) t

  type 'a enumeration =
    | End
    | More of key * 'a * (key * 'a) bst * 'a enumeration

  let count_enum =
    let rec count k = function
      | End -> k
      | More (_, _, tr, en) ->
        count (1 + k + size tr) en
    in
    fun en -> count 0 en

  let rec cons_enum m e = match m with
    | Empty -> e
    | Node (l, (k, v), r) ->
      cons_enum l (More (k, v, r, e))

  let rec rev_cons_enum m e = match m with
    | Empty -> e
    | Node (l, (k, v), r) ->
      rev_cons_enum r (More (k, v, l, e))

  let compare cmp (Map tr1) (Map tr2) =
    let rec aux e1 e2 = match (e1, e2) with
      | (End, End) -> 0
      | (End, _)  -> -1
      | (_, End) -> 1
      | (More (v1, d1, r1, e1), More (v2, d2, r2, e2)) ->
        let c = Ord.compare v1 v2 in
        if c <> 0 then c else
          let c = cmp d1 d2 in
          if c <> 0 then c else
            aux (cons_enum r1 e1) (cons_enum r2 e2)
    in aux (cons_enum tr1 End) (cons_enum tr2 End)

  let equal cmp (Map tr1) (Map tr2) =
    let rec aux e1 e2 =
      match (e1, e2) with
          (End, End) -> true
        | (End, _)  -> false
        | (_, End) -> false
        | (More (v1, d1, r1, e1), More (v2, d2, r2, e2)) ->
          Ord.compare v1 v2 = 0 && cmp d1 d2 &&
      aux (cons_enum r1 e1) (cons_enum r2 e2)
    in aux (cons_enum tr1 End) (cons_enum tr2 End)

  let rec enum_bst cfn en =
    let cur = ref en in
    let next () = match !cur with
      | End -> raise Enum.No_more_elements
      | More (k, v, r, e) ->
        cur := cfn r e ;
        (k, v)
    in
    let count () = count_enum !cur in
    let clone () = enum_bst cfn !cur in
    Enum.make ~next ~count ~clone

  let enum (Map tr) = enum_bst cons_enum (cons_enum tr End)
  let backwards (Map tr) = enum_bst rev_cons_enum (rev_cons_enum tr End)

  let keys m = Enum.map fst (enum m)
  let values m = Enum.map snd (enum m)

  let of_enum e = Enum.fold begin
    fun acc (k, v) -> add k v acc
  end empty e

  let to_list m = List.of_enum (enum m)
  let of_list l = of_enum (List.enum l)

  let print ?(first="of_list [") ?(last="]") ?(sep="; ")
      kpr vpr out m =
    Enum.print ~first ~last ~sep begin
      fun out (k, v) ->
        kpr out k ;
        BatInnerIO.nwrite out ", " ;
        vpr out v
    end out (enum m)

  module Labels = struct
    let add ~key ~data t = add key data t
    let iter ~f t = iter (fun key data -> f ~key ~data) t
    let map ~f t = map f t
    let mapi ~f t = mapi (fun key data -> f ~key ~data) t
    let fold ~f t ~init =
      fold (fun key data acc -> f ~key ~data acc) t init
    let compare ~cmp a b = compare cmp a b
    let equal ~cmp a b = equal cmp a b
    let filter ~f = filter f
    let filteri ~f = filteri f
  end

  module Exceptionless = struct
    let find k m =
      try Some (find k m) with Not_found -> None
  end

  module Infix = struct
    let ( --> ) m k = find k m
    let ( <-- ) m (k, v) = add k v m
  end

  let bindings m = List.of_enum (enum m)

  let exist_bool b f m =
    try
      iter (fun k v -> if f k v = b then raise Exit) m;
      false
    with Exit -> true
  let exists f m = exist_bool true f m
  let for_all f m = not (exist_bool false f m)

  let cardinal m = fold (fun _k _v -> succ) m 0

  (*TODO : add an implementation for these functions

    The following functions are required to conform to the Map
    interface since ocaml 3.12. They have not yet been implemented in
    BatSplay, but that must come soon.
  *)
  let split _ =
    failwith "split not yet implemented in batSplay"

  let merge _ =
    failwith "merge not yet implemented in batSplay"

  let pop _ =
    failwith "pop not yet implemented in batSplay"
  let extract _ =
    failwith "extract not yet implemented in batSplay"
end
