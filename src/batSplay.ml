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

let rec csplay' cx l r = match cx with
  | [] ->
    (l, r)
  | [Left (p, pr)] ->
    (l, Node (r, p, pr))
  | [Right (pl, px)] ->
    (Node (pl, px, l), r)
  | (Left (px, pr) :: Left (ppx, ppr) :: cx) ->
    (* zig zig *)
    let r = Node (r, px, Node (pr, ppx, ppr)) in
    csplay' cx l r
  | (Left (px, pr) :: Right (ppl, ppx) :: cx) ->
    (* zig zag *)
    let l = Node (ppl, ppx, l) in
    let r = Node (r, px, pr) in
    csplay' cx l r
  | (Right (pl, px) :: Right (ppl, ppx) :: cx) ->
    (* zig zig *)
    let l = Node (Node (ppl, ppx, pl), px, l) in
    csplay' cx l r
  | (Right (pl, px) :: Left (ppx, ppr) :: cx) ->
    (* zig zag *)
    let l = Node (pl, px, l) in
    let r = Node (r, ppx, ppr) in
    csplay' cx l r

let csplay = function
  | C (cx, Node (l, x, r)) ->
    let l', r' = csplay' cx l r in
    Node (l', x, r')
  | _ -> raise Not_found

let rec cfind ?(cx=[]) ~sel = function
  | Empty -> C (cx, Empty)
  | Node (l, x, r) as node ->
    let sx = sel x in
    if sx = 0 then C (cx, node)
    else if sx < 0 then cfind ~cx:(Left (x, r) :: cx) ~sel l
    else cfind ~cx:(Right (l, x) :: cx) ~sel r

(* A splay tree is a binary tree that is dynamically balanced: when
   a key is accessed, the tree is rebalanced (by an internal mutation) so
   that the next accesses to the same or neighbouring keys are very fast.

   Despite the use of a mutation for rebalancing, the structure is
   observably pure/persistent, as the mutation does not change the set
   of elements.
 *)

module StrongRef : sig
  type +
##V>=4.12## !
  'a t

  val ref : 'a -> 'a t
  val get : 'a t -> 'a
  val set : 'a t -> 'a -> unit
end = struct
  (* Didactic implementation note : why that ugly Obj.magic below?
     What does StrongRef bring compared to the usual ('a ref) type?

     We want splay tree to respect the Map interface, which whose map
     type is covariant (type (+'a) t). OCaml checks the internal
     definition to verify that the internal datatype is consistent
     with the variance annotation. Using a reference in the
     implementation of BatSplay would make the compiler reject the
     implementation, because reference types must be invariant.

     Following is an explanation of covariance and reference
     invariance, feel free to skip it if you already know.

         The idea of covariance for data structure is the following : if
         you have an ('a list), and a type 'b which is less specific than
         'a (a subtype, eg. with OCaml polymorphic variants or object
         types), you can at any type pretend that your list is
         a ('b list): if all 'a can be used as 'b, then all ('a list) can
         be used as ('b list).

           # type a = < f1 : int; f2 : float >;;
           # type b = < f1 : int >;;
           # let t : a = object method f1 = 1 method f2 = 2. end;;
           val t : a = <obj>
           # (t :> b);;
           - : b = <obj>
           # ([t] :> b list);;
           - : b list = [<obj>]

         But this is not true for ('a list ref), or else I may locally
         consider it a ('b list) and mutate it to add an element of type
         'b in it, then observe it at type ('a list ref) again. This is
         unsound because the added 'b element won't behave correctly as
         a 'a.

           # let tref = ref [t];;
           # (tref :> b list ref);;
           Error: Type a list ref is not a subtype of b list ref
           Type a = < f1 : int; f2 : float > is not compatible with type
             b = < f1 : int >
           The second object type has no method f2

         Imagine I think I know better, and break the type safety.

           # let forced_tref = (Obj.magic tref : b list ref);;

         Then I can add a element of type b to the list :

           # forced_tref := object method f1 = 1 end :: !forced_tref;;

         But this is unsound as I can now look at tref again, at type
         (a list ref).

           # !tref;;
           - : a list = [<obj>; <obj>]
           # (List.hd !tref)#f2;;
           Segmentation fault

         So in general, reference types cannot be safely subtyped (note
         that Java has had a blatant flaw in its type system for years, as
         mutable Arrays were covariant). If we used a `ref` in the
         internal definition of BatSplay.t, the typer would reject the
         module (the interface claims its covariant, while it's
         invariant).

     Said otherwise, covariance of a type (+'a t) allows situations
     where a single value may have several distinct types
     simultaneously:
       - the empty list [] is both an (int list) and a (float list)
         (distinct types here come from instantiations of the polymorphic
          'a list, generalized by the (relaxed) value restrict)
       - if a is a subtype of b, then all (a list) (even non-empty)
         are simultaneously of type (b list)

     Mutating such values is unsound in the general case, if the
     result of the mutation is a value that is not valid for some of
     those simultaneous types (adding a float in a ('a list ref) makes
     it invalid as an (int list ref)).

     In our case however, the mutations that actually happen (that are
     confined in the internal implementation of BatSplay) are soundly
     compatible with subtyping or polymorphic instantiation. Indeed,
     rebalancing never adds any element to the splay tree, it only
     reorders the element that were already there. In particular,
     sharing values between two different types (either through
     subtyping (cast) or polymorphic instantiation
     (relaxed value restriction)) is correct even if mutations happens
     on those shared value.. However, we must be careful to ensure
     that all rebalancings keep the set of elements of the splay tree
     unchanged (dropping elements would be ok-ish, but adding new
     elements would be unsound).

     We use the dirty Obj magic to create a type of "strong
     references" that are mutable yet covariant. Note that the
     mutations are confined to the "top" of the structure, the
     balanced tree itself is purely functional. Note that we must be
     careful (in the internal implementation) to allocate a new strong
     reference (with StrongRef.ref) each time we want to build a tree
     with a different set of elements than the one we started with.

     PS : No list reference were harmed during the implementation of
     this module.
   *)

  type 'a t = { ref : 'a }
  type 'a mut = { mutable mut_ref : 'a }

  let ref (x : 'a) = (Obj.magic { mut_ref = x } : 'a t)
  let get r = r.ref
  let set (r : 'a t) v = (Obj.magic r : 'a mut).mut_ref <- v
end

module Map (Ord : BatInterfaces.OrderedType) =
struct
  (*$inject
    module TestMap = Splay.Map (Int)
  *)
  (*$< TestMap *)

  type key = Ord.t

  type 'a map = (key * 'a) bst
  type 'a t = 'a map StrongRef.t

  let sget = StrongRef.get
  let sref = StrongRef.ref

  let empty = sref Empty

  let is_empty m =
    let tr = sget m in
    tr = Empty

  (*  let kcmp (j, _) (k, _) = Ord.compare j k*)
  let ksel j (k, _) = Ord.compare j k

  let singleton' k v = Node (Empty, (k, v), Empty)
  let singleton k v = sref (singleton' k v)

  let add k v tr =
    let tr = sget tr in
    sref begin
      csplay begin
        match cfind ~sel:(ksel k) tr with
        | C (cx, Node (l, (k, _), r)) -> C (cx, Node (l, (k, v), r))
        | C (cx, Empty) -> C (cx, singleton' k v)
      end
    end

  let modify k fn tr =
    let tr = sget tr in
    sref begin
      csplay begin
        match cfind ~sel:(ksel k) tr with
        | C (cx, Node (l, (k, v), r)) -> C (cx, Node (l, (k, fn v), r))
        | C (_cx, Empty) -> raise Not_found
      end
    end

  let modify_def def k fn tr =
    let tr = sget tr in
    sref begin
      csplay begin
        match cfind ~sel:(ksel k) tr with
        | C (cx, Node (l, (k, v), r)) -> C (cx, Node (l, (k, fn v), r))
        | C (cx, Empty) -> C (cx, singleton' k (fn def))
      end
    end

  let modify_opt k fn tr =
    let tr = sget tr in
    sref begin
      try
        match cfind ~sel:(ksel k) tr with
        | C (cx, Node (l, (k, v), r)) -> begin
            match fn (Some v) with
            | Some v' -> csplay (C (cx, Node (l, (k, v'), r)))
            | None    -> bst_append l r
          end
        | C (cx, Empty) ->
          match fn None with
          | Some v -> csplay (C (cx, singleton' k v))
          | None   -> raise Exit
      with Exit -> tr
    end

  let rebalance m tr =
    StrongRef.set m tr

  let find k m =
    let tr = sget m in
    let tr = csplay (cfind ~sel:(ksel k) tr) in
    match tr with
    | Node (_, (_, v), _) ->
      rebalance m tr;
      v
    | _ -> raise Not_found

  let find_opt k m =
    try Some (find k m)
    with Not_found -> None

  let find_default def k m =
    try find k m
    with Not_found -> def

  let rec find_first_helper_found f kv map = function
    | Node (l, (k, v), r) ->
       if f k
       then find_first_helper_found f (k, v) map l
       else find_first_helper_found f kv map r
    | Empty -> 
       (* dummy find to rebalance the tree *)
       ignore(find (fst kv) map);
       kv
             
  let find_first f (map : 'a t) =
    let rec loop_notfound f = function
      | Node(l, (k, v), r) ->
         if f k
         then find_first_helper_found f (k, v) map l
         else loop_notfound f r
      | Empty -> raise Not_found in
    loop_notfound f (sget map)

  let find_first_opt f map =
    let rec loop_notfound f = function
      | Node(l, (k, v), r) ->
         if f k
         then Some (find_first_helper_found f (k, v) map l)
         else loop_notfound f r
      | Empty -> None in
    loop_notfound f (sget map)

  let rec find_last_helper_found f kv map = function
    | Node (l, (k, v), r) ->
       if f k
       then find_last_helper_found f (k, v) map r
       else find_last_helper_found f kv map l
    | Empty ->
       (* dummy find to rebalance the tree *)
       ignore(find (fst kv) map);
       kv
    
  let find_last f (map : 'a t) =
    let rec loop_notfound f = function
      | Node(l, (k, v), r) ->
         if f k
         then find_last_helper_found f (k, v) map r
         else loop_notfound f l
      | Empty -> raise Not_found in
    loop_notfound f (sget map)

  let find_last_opt f map =
    let rec loop_notfound f = function
      | Node(l, (k, v), r) ->
         if f k
         then Some (find_last_helper_found f (k, v) map r)
         else loop_notfound f l
      | Empty -> None in
    loop_notfound f (sget map)
                    
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

  let cchange fn (C (cx, t)) = C (cx, fn t)

  let remove k tr =
    let tr = sget tr in
    let replace = function
      | Empty -> Empty
      | Node (l, _, r) -> bst_append l r
    in
    let tr = top (cchange replace (cfind ~sel:(ksel k) tr)) in
    sref tr

  let remove_exn k tr =
    let tr = sget tr in
    let replace = function
      | Empty -> raise Not_found
      | Node (l, _, r) -> bst_append l r
    in
    let tr = top (cchange replace (cfind ~sel:(ksel k) tr)) in
    sref tr

  (*$T remove_exn
    try remove_exn 1 empty |> ignore ; false with Not_found -> true
  *)

  let update k1 k2 v2 tr =
    if Ord.compare k1 k2 <> 0 then
      add k2 v2 (remove k1 tr)
    else
      let tr = sget tr in
      sref begin
        csplay begin
          match cfind ~sel:(ksel k1) tr with
          | C (cx, Node (l, _kv, r)) -> C (cx, Node (l, (k2, v2), r))
          | C (_cx, Empty) -> raise Not_found
        end
      end

  let update_stdlib k f m =
    match f (find_opt k m) with
    | Some x -> add k x m
    | None -> remove k m

  let mem k m =
    try ignore (find k m) ; true with Not_found -> false

  let iter fn tr =
    let tr = sget tr in
    let rec visit = function
      | Empty -> ()
      | Node (l, (k, v), r) ->
        visit l ;
        fn k v ;
        visit r
    in
    visit tr

  let fold fn tr acc =
    let tr = sget tr in
    let rec visit acc = function
      | Empty -> acc
      | Node (l, (k, v), r) ->
        let acc = visit acc l in
        let acc = fn k v acc in
        visit acc r
    in
    visit acc tr

  let min_binding tr =
    let tr = sget tr in
    let rec bfind = function
      | Node (Empty, kv, _) -> kv
      | Node (l, _, _) -> bfind l
      | Empty -> raise Not_found
    in
    bfind tr

  let min_binding_opt tr =
    let tr = sget tr in
    let rec bfind = function
      | Node (Empty, kv, _) -> Some kv
      | Node (l, _, _) -> bfind l
      | Empty -> None
    in
    bfind tr
        

  let choose = min_binding
  (*$= choose
    (empty |> add 0 1 |> add 1 1 |> choose) \
      (empty |> add 1 1 |> add 0 1 |> choose)
  *)
  (*$T choose
    try ignore (choose empty) ; false with Not_found -> true
  *)
  let choose_opt  = min_binding_opt
                  
  let any tr = match sget tr with
    | Empty -> raise Not_found
    | Node (_, kv, _) -> kv
  (*$T any
    try ignore (any empty) ; false with Not_found -> true
  *)

  let pop_min_binding tr =
    let mini = ref (choose tr) in
    let rec bfind = function
      | Node (Empty, kv, r) -> mini := kv; r
      | Node (l, kv, r) -> Node (bfind l, kv, r)
      | Empty ->  assert(false)  (* choose already raises Not_found on empty map *)
    in
    (!mini, sref (bfind (sget tr)))

  let max_binding tr =
    let tr = sget tr in
    let rec bfind = function
      | Node (_, kv, Empty) -> kv
      | Node (_, _, r) -> bfind r
      | Empty -> raise Not_found
    in
    bfind tr

  let max_binding_opt tr =
    let tr = sget tr in
    let rec bfind = function
      | Node (_, kv, Empty) -> Some kv
      | Node (_, _, r) -> bfind r
      | Empty -> None
    in
    bfind tr
        
  let pop_max_binding tr =
    let maxi = ref (choose tr) in
    let rec bfind = function
      | Node (l, kv, Empty) -> maxi := kv; l
      | Node (l, kv, r) -> Node (l, kv, bfind r)
      | Empty ->  assert(false)  (* choose already raises Not_found on empty map *)
    in
    (!maxi, sref (bfind (sget tr)))

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
    fun m -> visit (sget m) sref

  let filterv f t =
    filter_map (fun _ v -> if f v then Some v else None) t

  let filter f t =
    filter_map (fun k v -> if f k v then Some v else None) t

  let map f t = filter_map (fun _ v -> Some (f v)) t

  let mapi f t = filter_map (fun k v -> Some (f k v)) t

  let partition (p : key -> 'a -> bool) : 'a t -> 'a t * 'a t =
    let rec visit t cont = match t with
      | Empty -> cont Empty Empty
      | Node (l, ((k, v) as kv), r) ->
        visit l begin fun l1 l2 ->
          let b = p k v in
          visit r begin fun r1 r2 ->
            if b
            then cont (Node (l1, kv, r1)) (bst_append l2 r2)
            else cont (bst_append l1 r1)  (Node (l2, kv, r2))
          end
        end
    in
    fun m ->
      visit (sget m) (fun t1 t2 -> sref t1, sref t2)

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

  let rec cons_enum_from k2 m e =
    match m with
    | Empty -> e
    | Node (l, (k, v), r) ->
       if Ord.compare k2 k <= 0
       then cons_enum_from k2 l (More (k, v, r, e))
       else cons_enum_from k2 r e

  let compare cmp tr1 tr2 =
    let tr1, tr2 = sget tr1, sget tr2 in
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

  let equal cmp tr1 tr2 =
    let tr1, tr2 = sget tr1, sget tr2 in
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

  let enum tr = enum_bst cons_enum (cons_enum (sget tr) End)
  let backwards tr = enum_bst rev_cons_enum (rev_cons_enum (sget tr) End)

  let keys m = Enum.map fst (enum m)
  let values m = Enum.map snd (enum m)

  let of_enum e = Enum.fold begin
      fun acc (k, v) -> add k v acc
    end empty e

  let to_list m = List.of_enum (enum m)
  let of_list l = of_enum (List.enum l)
  let add_to_list x data m =
    let add = function None -> Some [data] | Some l -> Some (data :: l) in
    update_stdlib x add m

  let custom_print ~first ~last ~sep kvpr out m =
    Enum.print ~first ~last ~sep
      (fun out (k, v) -> kvpr out k v)
      out (enum m)

  let print ?(first="{\n") ?(last="}\n") ?(sep=",\n") ?(kvsep=": ") kpr vpr out m =
    custom_print ~first ~last ~sep
      (fun out k v -> BatPrintf.fprintf out "%a%s%a" kpr k kvsep vpr v)
      out m

  let print_as_list kpr vpr out m =
    print ~first:"[" ~last:"]" ~sep:"; " ~kvsep:", " kpr vpr out m

  module Labels = struct
    let add ~key ~data t = add key data t
    let iter ~f t = iter (fun key data -> f ~key ~data) t
    let map ~f t = map f t
    let mapi ~f t = mapi (fun key data -> f ~key ~data) t
    let fold ~f t ~init =
      fold (fun key data acc -> f ~key ~data acc) t init
    let compare ~cmp a b = compare cmp a b
    let equal ~cmp a b = equal cmp a b
    let filterv ~f = filterv f
    let filter ~f = filter f
  end

  module Exceptionless = struct
    let find k m = find_opt k m
    let choose m = try Some (choose m) with Not_found -> None
    let any m = try Some (any m) with Not_found -> None
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

  let split k m =
    let tr = sget m in
    let C (cx, center) = cfind ~sel:(ksel k) tr in
    match center with
    | Empty ->
      let l, r = csplay' cx Empty Empty in
      (sref l, None, sref r)
    | Node (l, x, r) ->
      let l', r' = csplay' cx l r in
      (* we rebalance as in 'find' *)
      rebalance m (Node (l', x, r'));
      (sref l', Some (snd x), sref r')

  let merge f m1 m2 =
    (* The implementation is a bit long, but has the important
       property of applying `f` in increasing key order. *)
    (* we will iterate on both enumerations in increasing order simultaneously *)
    let e1 = enum m1 in
    let e2 = enum m2 in
    (* we will push the results in increasing order from left to
       right; the result will be very unbalanced, but this will be
       corrected by the rebalancing at the first lookup in the splay
       tree. *)
    let maybe_push acc k maybe_v1 maybe_v2 =
      match f k maybe_v1 maybe_v2 with
      | None -> acc
      | Some v -> Node (acc, (k, v), Empty) in
    let push1 acc (k, v1) = maybe_push acc k (Some v1) None in
    let push2 acc (k, v2) = maybe_push acc k None (Some v2) in
    (* we iterate simultaneously on both inputs, in increasing
       order of keys. There are four different "states" to consider :
       - we have no idea of the inputs :
         none_known
       - we know the next (key, value) pair of e1, and that e2 is empty :
         only_e1 (k1, v1)
       - we know the next (key, value) pair of e2, and that e1 is empty :
         only_e2 (k2, v2)
       - we know the next (key, value) pair of both e1 and e2 :
         both_known (k1, v1) (k2, v2)
    *)
    let rec none_known acc =
      match Enum.peek e1, Enum.peek e2 with
      | None, None -> acc
      | None, Some kv2 ->
        Enum.junk e2;
        only_e2 acc kv2
      | Some kv1, None ->
        Enum.junk e1;
        only_e1 acc kv1
      | Some kv1, Some kv2 ->
        Enum.junk e1; Enum.junk e2;
        both_known acc kv1 kv2
    and only_e1 acc kv1 =
      Enum.fold push1 (push1 acc kv1) e1
    and only_e2 acc kv2 =
      Enum.fold push2 (push2 acc kv2) e2
    and both_known acc ((k1, v1) as kv1) ((k2, v2) as kv2) =
      let cmp = Ord.compare k1 k2 in
      if cmp < 0 then begin
        let acc = push1 acc kv1 in
        match Enum.peek e1 with
        | None -> only_e2 acc kv2
        | Some kv1' ->
          Enum.junk e1;
          both_known acc kv1' kv2
      end
      else if cmp > 0 then begin
        let acc = push2 acc kv2 in
        match Enum.peek e2 with
        | None -> only_e1 acc kv1
        | Some kv2' ->
          Enum.junk e2;
          both_known acc kv1 kv2'
      end
      else begin
        let acc = maybe_push acc k1 (Some v1) (Some v2) in
        none_known acc
      end
    in
    sref (none_known Empty)

  let pop m = match sget m with
    | Empty -> raise Not_found
    | Node (l, kv, r) -> kv, sref (bst_append l r)


  let add_seq s m =
    BatSeq.fold_left
      (fun m (k, v) -> add k v m)
      m
      s
    
  let of_seq s =
    add_seq s empty

  let rec seq_of_iter m () =
    match m with
    | End -> BatSeq.Nil
    | More(k, v, r, e) ->
       BatSeq.Cons ((k, v), seq_of_iter (cons_enum r e))
      
  let to_seq m =
    seq_of_iter (cons_enum (sget m) End)

  let to_rev_seq m =
    seq_of_iter (rev_cons_enum (sget m) End)

  let to_seq_from k m =
    seq_of_iter (cons_enum_from k (sget m) End)
   
  let union f m1 m2 =
    fold
     (fun k v m ->
        match find_opt k m with
        | Some v1 ->
           (match f k v v1 with
            | Some vmerged -> add k vmerged m
            | None -> remove k m)
        | None -> add k v m)
      m1
      m2
                         
  let extract k tr =
    let tr = sget tr in
    (* the reference here is a tad ugly but allows to reuse `cfind`
       without fuss *)
    let maybe_v = ref None in
    let replace = function
      | Empty -> Empty
      | Node (l, (_, v), r) ->
        maybe_v := Some v;
        bst_append l r
    in
    let tr = top (cchange replace (cfind ~sel:(ksel k) tr)) in
    (* like in the `remove` case, we don't bother rebalancing *)
    match !maybe_v with
    | None -> raise Not_found
    | Some v -> v, sref tr
  (*$>*)
end
  

