(*
 * Heap -- binomial heaps
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

let min x y = if Pervasives.compare x y <= 0 then x else y

(** binomial trees *)
type 'a bt = {
  rank : int ;
  root : 'a ;
  kids : 'a bt list ;
}

type 'a t = {
  size : int ;
  data : 'a bt list ;
  mind : 'a option ; (** cached minimal element *)
}

let empty = { size = 0 ; data = [] ; mind = None }

let size bh = bh.size

let link bt1 bt2 =
  assert (bt1.rank = bt2.rank) ;
  let rank = bt1.rank + 1 in
  let leq = Pervasives.compare bt1.root bt2.root <= 0 in
  let root = if leq then bt1.root else bt2.root in
  let kids = if leq then bt2 :: bt1.kids else bt1 :: bt2.kids in
  { rank = rank ; root = root ; kids = kids }

let rec add_tree t = function
  | [] -> [t]
  | (ut :: uts) as ts ->
    assert (t.rank <= ut.rank) ;
    if t.rank < ut.rank then t :: ts
    else add_tree (link t ut) uts

let insert bh x =
  let size = bh.size + 1 in
  let data = add_tree { rank = 0 ; root = x ; kids = [] } bh.data in
  let mind = match bh.mind with
    | None -> Some x
    | Some mind -> Some (min x mind)
  in {
    size = size ; data = data ; mind = mind
  }

(*$T size ; empty
  size (insert empty 3) = 1
  size empty = 0
*)

let add x bh = insert bh x

let rec merge_data ts1 ts2 = match ts1, ts2 with
  | _, [] -> ts1
  | [], _ -> ts2
  | t1 :: tss1, t2 :: tss2 ->
    if t1.rank < t2.rank then
      t1 :: merge_data tss1 ts2
    else if t1.rank > t2.rank then
      t2 :: merge_data ts1 tss2
    else
      add_tree (link t1 t2) (merge_data tss1 tss2)

let merge bh1 bh2 =
  let size = bh1.size + bh2.size in
  let data = merge_data bh1.data bh2.data in
  let mind = match bh1.mind, bh2.mind with
    | Some m1, Some m2 -> Some (min m1 m2)
    | m, None | None, m -> m
  in
  { size = size ; data = data ; mind = mind }

let find_min bh = match bh.mind with
  | None -> invalid_arg "find_min"
  | Some d -> d

(*$T find_min ; insert ; empty
   find_min (insert (insert empty 3) 5) = 3
   find_min (insert (insert empty 5) 3) = 3
*)


let rec find_min_tree ts k = match ts with
  | [] -> failwith "find_min_tree"
  | [t] -> k t
  | t :: ts ->
    find_min_tree ts begin
      fun u ->
        if Pervasives.compare t.root u.root <= 0
        then k t else k u
    end

let rec del_min_tree bts k = match bts with
  | [] -> invalid_arg "del_min"
  | [t] -> k t []
  | t :: ts ->
    del_min_tree ts begin
      fun u uts ->
        if Pervasives.compare t.root u.root <= 0
        then k t ts
        else k u (t :: uts)
    end

let del_min bh =
  del_min_tree bh.data begin
    fun bt data ->
      let size = bh.size - 1 in
      let data = merge_data (List.rev bt.kids) data in
      let mind = if size = 0 then None else Some (find_min_tree data (fun t -> t)).root in
      { size = size ; data = data ; mind = mind }
  end

let of_list l = List.fold_left insert empty l

let to_list bh =
  let rec aux acc bh =
    if size bh = 0 then acc else
      let m = find_min bh in
      let bh = del_min bh in
      aux (m :: acc) bh
  in
  List.rev (aux [] bh)

(*$T to_list ; empty
   to_list (insert (insert empty 4) 6) = [4; 6]
   to_list (insert (insert empty 6) 4) = [4; 6]
   to_list empty = []
*)

(*$Q to_list ; insert ; empty
   (Q.list Q.int) ~count:10 (fun l -> to_list (List.fold_left insert empty l) = List.sort Pervasives.compare l)
*)

let elems = to_list

let print ?(first="[") ?(last="]") ?(sep="; ") elepr out bh =
  let rec spin bh =
    if size bh = 0 then ()
    else if size bh = 1 then elepr out (find_min bh)
    else begin
      elepr out (find_min bh) ;
      BatInnerIO.nwrite out sep ;
      spin (del_min bh)
    end
  in
  BatInnerIO.nwrite out first ;
  spin bh ;
  BatInnerIO.nwrite out last

let rec enum bh =
  let cur = ref bh in
  let next () =
    let bh = !cur in
    if size bh = 0 then raise BatEnum.No_more_elements ;
    cur := (del_min bh) ; find_min bh
  in
  let count () = size !cur in
  let clone () = enum !cur in
  BatEnum.make ~next ~count ~clone

let of_enum e = BatEnum.fold insert empty e

module type H = sig
  type elem
  type t
  val empty     : t
  val size      : t -> int
  val insert    : t -> elem -> t
  val add       : elem -> t -> t
  val merge     : t -> t -> t
  val find_min  : t -> elem
  val del_min   : t -> t
  val of_list   : elem list -> t
  val to_list   : t -> elem list
  val elems     : t -> elem list
  val of_enum   : elem BatEnum.t -> t
  val enum      : t -> elem BatEnum.t
  val print     :  ?first:string -> ?last:string -> ?sep:string
    -> ('a BatInnerIO.output -> elem -> unit)
    -> 'a BatInnerIO.output -> t -> unit
end

module Make (Ord : BatInterfaces.OrderedType) = struct
  type elem = Ord.t

  let ord_min x y =
    if Ord.compare x y <= 0 then x else y

  type bt = {
    rank : int ;
    root : Ord.t ;
    kids : bt list ;
  }

  type t = {
    size : int ;
    data : bt list ;
    mind : Ord.t option ;
  }

  let empty = { size = 0 ; data = [] ; mind = None }

  let size bh = bh.size

  let link bt1 bt2 =
    assert (bt1.rank = bt2.rank) ;
    let rank = bt1.rank + 1 in
    let leq = Ord.compare bt1.root bt2.root <= 0 in
    let root = if leq then bt1.root else bt2.root in
    let kids = if leq then bt2 :: bt1.kids else bt1 :: bt2.kids in
    { rank = rank ; root = root ; kids = kids }

  let rec add_tree t = function
    | [] -> [t]
    | (ut :: uts) as ts ->
      assert (t.rank <= ut.rank) ;
      if t.rank < ut.rank then t :: ts
      else add_tree (link t ut) uts

  let insert bh x =
    let data = add_tree { rank = 0 ; root = x ; kids = [] } bh.data in
    let mind = match bh.mind with
      | None -> Some x
      | Some mind -> Some (ord_min x mind)
    in {
      size = bh.size + 1 ; data = data ; mind = mind
    }

  let add x bh = insert bh x

  let rec merge_data ts1 ts2 = match ts1, ts2 with
    | _, [] -> ts1
    | [], _ -> ts2
    | t1 :: tss1, t2 :: tss2 ->
      if t1.rank < t2.rank then
        t1 :: merge_data tss1 ts2
      else if t1.rank > t2.rank then
        t2 :: merge_data ts1 tss2
      else
        add_tree (link t1 t2) (merge_data tss1 tss2)

  let merge bh1 bh2 =
    let size = bh1.size + bh2.size in
    let data = merge_data bh1.data bh2.data in
    let mind = match bh1.mind, bh2.mind with
      | Some m1, Some m2 -> Some (ord_min m1 m2)
      | m, None | None, m -> m
    in
    { size = size ; data = data ; mind = mind }

  let find_min bh = match bh.mind with
    | None -> invalid_arg "find_min"
    | Some d -> d

  let rec find_min_tree ts k = match ts with
    | [] -> failwith "find_min_tree"
    | [t] -> k t
    | t :: ts ->
      find_min_tree ts begin
        fun u ->
          if Ord.compare t.root u.root <= 0
          then k t else k u
      end

  let rec del_min_tree bts k = match bts with
    | [] -> invalid_arg "del_min"
    | [t] -> k t []
    | t :: ts ->
      del_min_tree ts begin
        fun u uts ->
          if Ord.compare t.root u.root <= 0
          then k t ts
          else k u (t :: uts)
      end

  let del_min bh =
    del_min_tree bh.data begin
      fun bt data ->
        let size = bh.size - 1 in
        let data = merge_data (List.rev bt.kids) data in
        let mind = if size = 0 then None else Some (find_min_tree data (fun t -> t)).root in
        { size = size ; data = data ; mind = mind }
    end

  let to_list bh =
    let rec aux acc bh =
      if size bh = 0 then acc else
        let m = find_min bh in
        let bh = del_min bh in
        aux (m :: acc) bh
    in
    List.rev (aux [] bh)

  let elems = to_list

  let of_list l = List.fold_left insert empty l

  let rec enum bh =
    let cur = ref bh in
    let next () =
      let bh = !cur in
      if size bh = 0 then raise BatEnum.No_more_elements ;
      cur := (del_min bh) ; find_min bh
    in
    let count () = size !cur in
    let clone () = enum !cur in
    BatEnum.make ~next ~count ~clone

  let of_enum e = BatEnum.fold insert empty e

  let print ?(first="[") ?(last="]") ?(sep="; ") elepr out bh =
    let rec spin bh =
      if size bh = 0 then ()
      else if size bh = 1 then elepr out (find_min bh)
      else begin
        elepr out (find_min bh) ;
        BatInnerIO.nwrite out sep ;
        spin (del_min bh)
      end
    in
    BatInnerIO.nwrite out first ;
    spin bh ;
    BatInnerIO.nwrite out last

end
