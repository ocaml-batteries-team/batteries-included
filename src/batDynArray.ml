(*
 * DynArray - Resizeable Ocaml arrays
 * Copyright (C) 2003 Brian Hurt
 * Copyright (C) 2003 Nicolas Cannasse
 * Copyright (C) 2008 David Teller
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


type resizer_t = currslots:int -> oldlength:int -> newlength:int -> int

type 'a intern

external ilen : 'a intern -> int = "%obj_size"
let idup (x : 'a intern) : 'a intern = Obj.magic (Obj.dup (Obj.repr x))
let imake len = (Obj.magic (Obj.new_block 0 len) : 'a intern)
external iget : 'a intern -> int -> 'a = "%obj_field"
external iset : 'a intern -> int -> 'a -> unit = "%obj_set_field"

type 'a t = {
  mutable arr : 'a intern;
  mutable len : int;
  mutable resize: resizer_t;
}

let dummy_for_gc = Obj.magic 0
let bool_invariants t =
  t.len >= 0 &&
  t.len <= ilen t.arr &&
  (* check that elements beyond t.len are free'd, no memory leak *)
  let rec check i =
    if i >= ilen t.arr - 1 then true
    else iget t.arr i == dummy_for_gc && check (i+1)
  in check t.len
let invariants t = assert (bool_invariants t)

type 'a mappable = 'a t
type 'a enumerable = 'a t

exception Invalid_arg of int * string * string

let invalid_arg n f p = raise (Invalid_arg (n,f,p))

let length d = d.len

let exponential_resizer ~currslots ~oldlength:_ ~newlength =
  let rec doubler x = if x >= newlength then x else doubler (x * 2) in
  let rec halfer x = if x / 2 < newlength then x else halfer (x / 2) in
  if newlength = 1 then
    1
  else if currslots = 0 then
    doubler 1
  else if currslots < newlength then
    doubler currslots
  else
    halfer currslots

let step_resizer step =
  if step <= 0 then invalid_arg step "step_resizer" "step";
  (fun ~currslots ~oldlength:_ ~newlength ->
    if currslots < newlength || newlength < (currslots - step)
    then
      (newlength + step - (newlength mod step))
    else
      currslots)

let conservative_exponential_resizer ~currslots ~oldlength ~newlength =
  let rec doubler x = if x >= newlength then x else doubler (x * 2) in
  let rec halfer x = if x / 2 < newlength then x else halfer (x / 2) in
  if currslots < newlength then begin
    if newlength = 1 then
      1
    else if currslots = 0 then
      doubler 1
    else
      doubler currslots
  end else if oldlength < newlength then
    halfer currslots
  else
    currslots

let default_resizer = conservative_exponential_resizer

let changelen (d : 'a t) newlen =
  let oldsize = ilen d.arr in
  let r = d.resize
      ~currslots:oldsize
      ~oldlength:d.len
      ~newlength:newlen
  in
  (* We require the size to be at least large enough to hold the number
   * of elements we know we need!
  *)
  let newsize = if r < newlen then newlen else r in
  if newsize <> oldsize then begin
    let newarr = imake newsize in
    let cpylen = (if newlen < d.len then newlen else d.len) in
    for i = 0 to cpylen - 1 do
      iset newarr i (iget d.arr i);
    done;
    d.arr <- newarr;
  end;
  d.len <- newlen

let compact d =
  if d.len <> ilen d.arr then begin
    let newarr = imake d.len in
    for i = 0 to d.len - 1 do
      iset newarr i (iget d.arr i)
    done;
    d.arr <- newarr;
  end

let create_with resize=
  { resize;
    len=0;
    arr=imake 0;
  }

(*$Q
  (Q.list Q.small_int) (fun l -> \
    let v = create_with exponential_resizer in List.iter (add v) l; \
    bool_invariants v)
  (Q.list Q.small_int) (fun l -> \
    let v = create_with conservative_exponential_resizer in List.iter (add v) l; \
    bool_invariants v)
  (Q.list Q.small_int) (fun l -> \
    let v = create_with (step_resizer 5) in List.iter (add v) l; \
    bool_invariants v)
  *)

let create() =
  {
    resize = default_resizer;
    len = 0;
    arr = imake 0;
  }

(*$Q
  (Q.list Q.small_int) (fun l -> \
    let v = create() in List.iter (add v) l; \
    bool_invariants v)
  *)

let singleton x = 
  let a = {
    resize = default_resizer;
    len = 1;
    arr = imake 1;
  } in
  iset a.arr 0 x;
  a

(*$T
  to_list @@ singleton 42 = [42]
*)

let make initlen =
  if initlen < 0 then invalid_arg initlen "make" "size";
  {
    resize = default_resizer;
    len = 0;
    arr = imake initlen;
  }

let init initlen f =
  if initlen < 0 then invalid_arg initlen "init" "len";
  let arr = imake initlen in
  for i = 0 to initlen-1 do
    iset arr i (f i)
  done;
  {
    resize = default_resizer;
    len = initlen;
    arr = arr;
  }

(*$T
  init 5 identity |> to_list = [0;1;2;3;4]
*)

let set_resizer d resizer =
  d.resize <- resizer

let get_resizer d =
  d.resize

let empty d =
  d.len = 0

let get d idx =
  if idx < 0 || idx >= d.len then invalid_arg idx "get" "index";
  iget d.arr idx

let set d idx v =
  if idx < 0 || idx >= d.len then invalid_arg idx "set" "index";
  iset d.arr idx v

(* upd a i f = set a i (f @@ get a i) 
   Faster (avoids duplication of bounds checks) and more convenient. *)
let upd d idx f =
  if idx < 0 || idx >= d.len then invalid_arg idx "set" "index";
  iset d.arr idx (f @@ iget d.arr idx)

let first d =
  if d.len = 0 then invalid_arg 0 "first" "<array len is 0>";
  iget d.arr 0

let last d =
  if d.len = 0 then invalid_arg 0 "last" "<array len is 0>";
  iget d.arr (d.len - 1)

(*$T
  let v = of_list [1;2;3;4] in set v 1 42; get v 1 = 42
  let v = of_list [1;2;3;4] in set v 1 42; last v = 4
  let v = of_list [1;2;3;4] in set v 1 42; first v = 1
  let v = of_list [1;2;3;4] in upd v 1 succ; get v 1 = 3
*)

let left a n =
  let arr = imake n in
  for i = 0 to n - 1 do
    iset arr i (iget a.arr i)
  done;
  {
    resize = a.resize;
    len = n;
    arr = arr;
  }

let right a n =
  let arr = imake n in
  (* for i = a.len - n to a.len - 1 do *)
  let i = ref 0 in
  let j = ref (a.len - n) in
  while !i < n do
    iset arr !i (iget a.arr !j);
    incr i; incr j;
  done;
  {
    resize = a.resize;
    len = n;
    arr = arr;
  }

(*$T
  let v = left  (of_list [1;2;3;4;5;6;7;8]) 3 in to_list v = [1;2;3]
  let v = right (of_list [1;2;3;4;5;6;7;8]) 3 in to_list v = [6;7;8]
*)

let head = left

let tail a n =
  let len = a.len - n in
  let arr = imake len in
  (* for i = n to a.len - 1 do *)
  let i = ref 0 in
  let j = ref n in
  while !j < a.len do
    iset arr !i (iget a.arr !j);
    incr i; incr j;
  done;
  {
    resize = a.resize;
    len = len;
    arr = arr;
  }

(*$T
  let v = head (of_list [1;2;3;4;5;6;7;8]) 3 in to_list v = [1;2;3]
  let v = tail (of_list [1;2;3;4;5;6;7;8]) 3 in to_list v = [4;5;6;7;8]
*)

let insert d idx v =
  if idx < 0 || idx > d.len then invalid_arg idx "insert" "index";
  if d.len = ilen d.arr then changelen d (d.len + 1) else d.len <- d.len + 1;
  if idx < d.len - 1 then begin
    for i = d.len - 2 downto idx do
      iset d.arr (i+1) (iget d.arr i)
    done;
  end;
  iset d.arr idx v

(*$T
  let v = of_list [1;2;3;4] in insert v 2 10; to_list v = [1;2;10;3;4]
*)

let add d v =
  if d.len = ilen d.arr then changelen d (d.len + 1) else d.len <- d.len + 1;
  iset d.arr (d.len - 1) v

let delete d idx =
  if idx < 0 || idx >= d.len then invalid_arg idx "delete" "index";
  let oldsize = ilen d.arr in
  (* we don't call changelen because we want to blit *)
  let r = d.resize
      ~currslots:oldsize
      ~oldlength:d.len
      ~newlength:(d.len - 1)
  in
  let newsize = (if r < d.len - 1 then d.len - 1 else r) in
  if oldsize <> newsize then begin
    let newarr = imake newsize in
    for i = 0 to idx - 1 do
      iset newarr i (iget d.arr i);
    done;
    for i = idx to d.len - 2 do
      iset newarr i (iget d.arr (i+1));
    done;
    d.arr <- newarr;
  end else begin
    for i = idx to d.len - 2 do
      iset d.arr i (iget d.arr (i+1));
    done;
    iset d.arr (d.len - 1) dummy_for_gc
  end;
  d.len <- d.len - 1

(*$T
  let v = of_list [1;2;3;4] in delete v 1; to_list v = [1;3;4]
*)

let delete_range d idx len =
  if len < 0 then invalid_arg len "delete_range" "length";
  if idx < 0 || idx + len > d.len then invalid_arg idx "delete_range" "index";
  let oldsize = ilen d.arr in
  (* we don't call changelen because we want to blit *)
  let r = d.resize
      ~currslots:oldsize
      ~oldlength:d.len
      ~newlength:(d.len - len)
  in
  let newsize = (if r < d.len - len then d.len - len else r) in
  if oldsize <> newsize then begin
    let newarr = imake newsize in
    for i = 0 to idx - 1 do
      iset newarr i (iget d.arr i);
    done;
    for i = idx to d.len - len - 1 do
      iset newarr i (iget d.arr (i+len));
    done;
    d.arr <- newarr;
  end else begin
    for i = idx to d.len - len - 1 do
      iset d.arr i (iget d.arr (i+len));
    done;
    for i = d.len - len to d.len - 1 do
      iset d.arr i dummy_for_gc
    done;
  end;
  d.len <- d.len - len

(*$T
  let v = of_list [1;2;3;4] in delete_range v 1 2; to_list v = [1;4]
  let v = of_list [1;2;3;4] in delete_range v 1 0; to_list v = [1;2;3;4]
  let v = of_list [1;2;3;4] in try delete_range v 4 2; false \
    with Invalid_arg _ -> true
*)

let clear d =
  d.len <- 0;
  d.arr <- imake 0

(*$T
  let v = of_list [1;2;3;4;5] in clear v; to_list v = []
*)

let delete_last d =
  if d.len <= 0 then invalid_arg 0 "delete_last" "<array len is 0>";
  (* erase for GC, in case changelen don't resize our array *)
  iset d.arr (d.len - 1) dummy_for_gc;
  changelen d (d.len - 1)

(*$T
  let v = of_list [1;2;3;4;5] in delete_last v; to_list v = [1;2;3;4]
*)

let blit src srcidx dst dstidx len =
  if len < 0 then invalid_arg len "blit" "len";
  if srcidx < 0 || srcidx + len > src.len then invalid_arg srcidx "blit" "source index";
  if dstidx < 0 || dstidx > dst.len then invalid_arg dstidx "blit" "dest index";
  let newlen = dstidx + len in
  if newlen > ilen dst.arr then begin
    (* this case could be inlined so we don't blit on just-copied elements *)
    changelen dst newlen
  end else begin
    if newlen > dst.len then dst.len <- newlen;
  end;
  (* same array ! we need to copy in reverse order *)
  if src.arr == dst.arr && dstidx > srcidx then
    for i = len - 1 downto 0 do
      iset dst.arr (dstidx+i) (iget src.arr (srcidx+i));
    done
  else
    for i = 0 to len - 1 do
      iset dst.arr (dstidx+i) (iget src.arr (srcidx+i));
    done

(*$T
  let v = of_list [1;2;3;4;5] and v2 = of_list [10;11] in \
    blit v2 0 v 1 2; to_list v = [1;10;11;4;5]
*)

let append src dst =
  blit src 0 dst dst.len src.len

(*$T
  let v = of_list [1;2;3;4;5] and v2 = of_list [10;11] in \
    append v2 v; to_list v = [1;2;3;4;5;10;11]
*)

let to_list d =
  let rec loop idx accum =
    if idx < 0 then accum else loop (idx - 1) (iget d.arr idx :: accum)
  in
  loop (d.len - 1) []

let to_array d =
  if d.len = 0 then begin
    (* since the empty array is an atom, we don't care if float or not *)
    [||]
  end else begin
    let arr = Array.make d.len (iget d.arr 0) in
    for i = 1 to d.len - 1 do
      Array.unsafe_set arr i (iget d.arr i)
    done;
    arr;
  end

let of_list lst =
  let size = List.length lst in
  let arr = imake size in
  let rec loop idx =  function
    | h :: t -> iset arr idx h; loop (idx + 1) t
    | [] -> ()
  in
  loop 0 lst;
  {
    resize = default_resizer;
    len = size;
    arr = arr;
  }

let of_array src =
  let size = Array.length src in
  let is_float = Obj.tag (Obj.repr src) = Obj.double_array_tag in
  let arr = (if is_float then begin
      let arr = imake size in
      for i = 0 to size - 1 do
        iset arr i (Array.unsafe_get src i);
      done;
      arr
    end else
      (* copy the fields *)
      idup (Obj.magic src : 'a intern))
  in
  {
    resize = default_resizer;
    len = size;
    arr = arr;
  }

let copy src =
  {
    resize = src.resize;
    len = src.len;
    arr = idup src.arr;
  }

(*$T
  let v = of_list [1;2;3] in let v2 = copy v in \
    set v 0 42; get v2 0 = 1
*)

let sub src start len =
  if len < 0 then invalid_arg len "sub" "len";
  if start < 0 || start + len > src.len then invalid_arg start "sub" "start";
  let arr = imake len in
  for i = 0 to len - 1 do
    iset arr i (iget src.arr (i+start));
  done;
  {
    resize = src.resize;
    len = len;
    arr = arr;
  }

(*$T
  let v = of_list [1;2;3;4;5] in \
    let v2 = sub v 1 3 in to_list v2 = [2;3;4]
  let v = of_list [1;2;3;4;5] in \
    let v2 = sub v 0 1 in to_list v2 = [1]
  let v = of_list [1;2;3;4;5] in \
    let v2 = sub v 4 1 in to_list v2 = [5]
  let v = of_list [1;2;3;4;5] in \
    let v2 = sub v 2 0 in to_list v2 = []
  let v = of_list [1;2;3;4;5] in \
    try ignore @@ sub v (-1) 2; false with Invalid_arg _ -> true
  let v = of_list [1;2;3;4;5] in \
    try ignore @@ sub v 5 2; false with Invalid_arg _ -> true
  let v = of_list [1;2;3;4;5] in \
    try ignore @@ sub v 3 3; false with Invalid_arg _ -> true
  let v = of_list [1;2;3;4;5] in \
    try ignore @@ sub v 3 (-1); false with Invalid_arg _ -> true
*)

let fill a start len x = 
  if len < 0 then invalid_arg len "fill" "len";
  if start < 0 || start+len > a.len then invalid_arg start "fill" "start";
  for i = start to start + len - 1 do
    iset a.arr i x
  done

(*$T
  let v = of_list [1;2;3;4;5] in \
    fill v 1 3 0; to_list v = [1;0;0;0;5]
  let v = of_list [1;2;3;4;5] in \
    fill v 0 1 0; to_list v = [0;2;3;4;5]
  let v = of_list [1;2;3;4;5] in \
    fill v 4 1 0; to_list v = [1;2;3;4;0]
  let v = of_list [1;2;3;4;5] in \
    fill v 2 0 0; to_list v = [1;2;3;4;5]
  let v = of_list [1;2;3;4;5] in \
    try fill v (-1) 2 0; false with Invalid_arg _ -> true
  let v = of_list [1;2;3;4;5] in \
    try fill v 5 2 0; false with Invalid_arg _ -> true
  let v = of_list [1;2;3;4;5] in \
    try fill v 3 3 0; false with Invalid_arg _ -> true
  let v = of_list [1;2;3;4;5] in \
    try fill v 3 (-1) 0; false with Invalid_arg _ -> true
  let v = of_list [1;2;3;4;5] in \
    try fill v (-1) 2 0; false with Invalid_arg _ -> true
  let v = of_list [1;2;3;4;5] in \
    try fill v 5 2 0; false with Invalid_arg _ -> true
  let v = of_list [1;2;3;4;5] in \
    try fill v 3 3 0; false with Invalid_arg _ -> true
  let v = of_list [1;2;3;4;5] in \
    try fill v 3 (-1) 0; false with Invalid_arg _ -> true
*)

let split a =
  let n = a.len in
  let left = make n in
  let right = make n in
  for i = 0 to n-1 do
    let a,b = iget a.arr i in
    iset left.arr i a;
    iset right.arr i b
  done;
  left.len <- n;
  right.len <- n;
  (left, right)

(*$T
  let v = of_list [] in let l,r = split v in \
    (to_list l, to_list r) = ([], [])
  let v = of_list [(1,"a");(2,"b");(3,"c")] in let l,r = split v in \
    (to_list l, to_list r) = ([1;2;3], ["a";"b";"c"])
*)

let combine a1 a2 =
  if a1.len <> a2.len then
    invalid_arg "DynArray.iter2i";
  let arr = imake a1.len in
  for i = 0 to a1.len - 1 do
    iset arr i (iget a1.arr i, iget a2.arr i)
  done;
  {
    resize = a1.resize; 
    len = a1.len;
    arr = arr;
  }

(*$T
  let l,r = (of_list [], of_list []) in let c = combine l r in \
    to_list c = []
  let l, r = (of_list [1;2;3], of_list ["a";"b";"c"]) in let c = combine l r in \
    to_list c = [(1,"a");(2,"b");(3,"c")]
*)

let iter f d =
  let i = ref 0 in
  while !i < d.len do
    f (iget d.arr !i);
    incr i
  done

(*$T
  let v = of_list [1;2;3] and v2 = create() in \
    iter (add v2) v; to_list v2 = [1;2;3]
*)

(* string_of_int and int_of_string seems useless but it
   is because if you only manipulate integers, you aren't
   likely to have segfaults even if the code is wrong *)
(*$R iter
  let n = 20 in
  let acc = ref 0 in
  let d = init n (fun i -> string_of_int i) in
  iter (fun s -> assert (Obj.tag (Obj.repr s) = Obj.string_tag); acc := !acc + int_of_string s) d;
  assert_bool "iter" (!acc = (n - 1) * n / 2)
*)

(* checking the absence of segfault when the array shrinks *)
(*$R iter
  let n = 40 in
  let d = init n (fun i -> string_of_int i) in
  let i = ref (-1) in
  iter (fun s -> assert (Obj.tag (Obj.repr s) = Obj.string_tag); incr i;
          if !i = 0 then
            for _count = 0 to n * 4 / 5 do delete_last d done
  ) d
*)

(* checking the absence of segfault when the array grows *)
(*$R iter
  let n = 40 in
  let d = init n (fun i -> string_of_int i) in
  let i = ref (-1) in
  iter (fun s -> assert (Obj.tag (Obj.repr s) = Obj.string_tag); incr i;
          if !i = 0 then
            for _count = 0 to n * 4 do add d "poi" done
  ) d
*)

let iteri f d =
  let i = ref 0 in
  while !i < d.len do
    f !i (iget d.arr !i);
    incr i
  done

(*$R iteri
  let n = 20 in
  let acc = ref 0 in
  let d = init n (fun i -> string_of_int i) in
  let i = ref (-1) in
  iteri (fun idx s -> assert (Obj.tag (Obj.repr s) = Obj.string_tag); incr i;
                     assert (idx = !i); acc := !acc + int_of_string s) d;
  assert_bool "iteri" (!acc = (n - 1) * n / 2)
*)

(*$R iteri
  let n = 40 in
  let d = init n (fun i -> string_of_int i) in
  let i = ref (-1) in
  iteri (fun idx s -> assert (Obj.tag (Obj.repr s) = Obj.string_tag); incr i;
          assert (idx = !i);
          if !i = 0 then
            for _count = 0 to n * 4 / 5 do delete_last d done
  ) d
*)

(*$R iteri
  let n = 40 in
  let d = init n (fun i -> string_of_int i) in
  let i = ref (-1) in
  iteri (fun idx s -> assert (Obj.tag (Obj.repr s) = Obj.string_tag); incr i;
          assert (idx = !i);
          if !i = 0 then
            for _count = 0 to n * 4 do add d "poi" done
  ) d
*)

(* Old implementation *)
(*let filter f d =
  let l = d.len in
  let dest = make l in
  let a2 = d.arr in
  let p = ref 0 in (* p is index of next unused element *)
  let i = ref 0 in
  while !i < d.len && !i < l do
    (* beware that the call to f might make lengthen d
       in which case, if we iterate on the new elements
       dest.array may be too short
       so when some elements are added, we do not iterate on them
       (test !i < len)
       if some elements are removed, we are also careful not to
       iterate on the removed elements (test !i < d.len)
    *)
    let x = iget a2 !i in
    if f x then begin
      iset dest.arr !p x;
      incr p;
    end;
    incr i
  done;
  dest.len <- !p;
  changelen dest !p;
  dest*)

(* Efficient implementation using BitSet, lifted from BatArray implementation of filter *)
let filter p a =
  let n = a.len in
  (* Use a bitset to store which elements will be in the final array. *)
  let bs = BatBitSet.create n in
  for i = 0 to n-1 do
    if p @@ iget a.arr i then 
      BatBitSet.set bs i
  done;
  (* Allocate the final array and copy elements into it. *)
  let n' = BatBitSet.count bs in
  let j = ref 0 in
  init n'
    (fun _ -> match BatBitSet.next_set_bit bs !j with
      | Some i -> j := i+1; iget a.arr i
      | None ->
        (* not enough 1 bits - incorrect count? *)
        assert false
    )

(*$T filter
  let v = filter (fun x -> x mod 3 = 0) (of_list @@ BatList.range 1 `To 10) in \
    to_list v = [3;6;9]
  let v = filter (fun _ -> assert false) (create()) in \
    to_list v = []
*)

let find_all = filter

let filteri p a =
  let n = a.len in
  (* Use a bitset to store which elements will be in the final array. *)
  let bs = BatBitSet.create n in
  for i = 0 to n-1 do
    if p i @@ iget a.arr i then 
      BatBitSet.set bs i
  done;
  (* Allocate the final array and copy elements into it. *)
  let n' = BatBitSet.count bs in
  let j = ref 0 in
  init n'
    (fun _ -> match BatBitSet.next_set_bit bs !j with
      | Some i -> j := i+1; iget a.arr i
      | None ->
        (* not enough 1 bits - incorrect count? *)
        assert false
    )

(*$T filteri
  let v = filteri (fun i x -> (i+x) mod 2 = 0) (of_list [1;2;3;4;0;1;2;3]) in \
    to_list v = [0;1;2;3]
  let v = filteri (fun _ _ -> assert false) (create()) in \
    to_list v = []
*)

let keep f d =
  let result = filter f d in
  d.arr <- result.arr;
  d.len <- result.len

(*$R keep
  let e = create () in
  add e "a";
  add e "b";
  keep ((=) "a") e;
  assert_equal ~printer:(fun x -> x) (get e 0) "a"
*)

let filter_map f d =
  let l = d.len in
  let dest = make l in (*Create the destination array with size [l]*)
  let a2 = d.arr in
  let p = ref 0 in
  let i = ref 0 in
  while !i < d.len && !i < l do
    (match f (iget a2 !i) with
     | None -> ()
     | Some x -> begin
         iset dest.arr !p x;
         incr p;
       end);
    incr i
  done;
  dest.len <- !p;
  changelen dest !p; (*Trim the destination array to the right size*)
  dest

(*$R filter_map
  let n = 20 in
  let d = init n (fun i -> string_of_int i) in
  let i = ref (-1) in
  let d = filter_map (fun s ->
     assert (Obj.tag (Obj.repr s) = Obj.string_tag); incr i;
     if !i mod 2 = 0 then Some (s ^ s) else None) d in
  assert_bool "filter_map" (length d = n / 2);
  let acc = ref true in
  iteri (fun idx s -> acc := (!acc && (s = string_of_int (2 * idx) ^ string_of_int (2 * idx)))) d;
  assert_bool "filter_map" !acc
*)

(*$R filter_map
  let n = 40 in
  let d = init n (fun i -> string_of_int i) in
  let i = ref (-1) in
  ignore (filter_map (fun s -> assert (Obj.tag (Obj.repr s) = Obj.string_tag); incr i;
          if !i = 0 then
            for _count = 0 to n * 4 / 5 do delete_last d done;
          Some s
  ) d)
*)

(*$R filter_map
  let n = 40 in
  let d = init n (fun i -> string_of_int i) in
  let i = ref (-1) in
  ignore (filter_map (fun s -> assert (Obj.tag (Obj.repr s) = Obj.string_tag); incr i;
          if !i = 0 then
            for _count = 0 to n * 4 do add d "poi" done;
          Some s
  ) d)
*)

let partition p a =
  let n = a.len in
  (* Use a bitset to store which elements will be in the final array. *)
  let bs = BatBitSet.create n in
  for i = 0 to n-1 do
    if p @@ iget a.arr i then 
      BatBitSet.set bs i
  done;
  (* Allocate the arrays and copy elements into them. *)
  let n' = BatBitSet.count bs in
  let pos = make n' in
  let neg = make (n-n') in
  for i = 0 to n-1 do
    if BatBitSet.mem bs i then
      add pos @@ iget a.arr i
    else
      add neg @@ iget a.arr i
  done;
  (pos, neg)

(*$T partition
  let v,w = partition (fun x -> x mod 3 = 0) (of_list @@ BatList.range 1 `To 10) in \
    (to_list v, to_list w) = ([3;6;9], [1;2;4;5;7;8;10])
  let v,w = partition (fun _ -> assert false) (create()) in \
    empty v && empty w
*)

let for_all p a =
  let n = a.len in
  let rec loop i =
    if i = n then true
    else if p (iget a.arr i) then loop (succ i)
    else false
  in
  loop 0

(*$T for_all
   for_all (fun x -> x mod 2 = 0) (of_list [2;4;6]) = true
   for_all (fun x -> x mod 2 = 0) (of_list [2;3;6]) = false
   for_all (fun _ -> false) (create()) = true
*)

let exists p a =
  let n = a.len in
  let rec loop i =
    if i = n then false
    else if p (iget a.arr i) then true
    else loop (succ i)
  in
  loop 0

(*$T exists
   exists (fun x -> x mod 2 = 0) (of_list [1;4;5]) = true
   exists (fun x -> x mod 2 = 0) (of_list [1;3;5]) = false
   exists (fun _ -> false) (create()) = false
*)


let mem x a =
  let n = a.len in
  let rec loop i =
    if i = n then
      false
    else if x = iget a.arr i then
      true
    else
      loop (succ i)
  in
  loop 0

(*$T mem
   mem 2 (of_list [1;2;3]) = true
   mem 2 (create()) = false
   mem (ref 3) (of_list [ref 1; ref 2; ref 3]) = true
*)

let memq x a =
  let n = a.len in
  let rec loop i =
    if i = n then
      false
    else if x == iget a.arr i then
      true
    else
      loop (succ i)
  in
  loop 0

(*$T memq
   memq 2 (of_list [1;2;3]) = true
   memq 2 (create()) = false
   memq (ref 3) (of_list [ref 1; ref 2; ref 3]) = false
*)

let index_of p a =
  let rec loop i =
    if i = a.len then
      raise Not_found
    else if p (iget a.arr i) then
      i
    else
      loop (succ i)
  in
  loop 0

let findi = index_of
(*$T findi
  findi (fun x -> x mod 3 = 0) (of_list [1;2;3;4;5;6]) = 2
  try ignore @@ findi (fun x -> x mod 3 = 0) (of_list [1;2;4;5]); false \
    with Not_found -> true
  try ignore @@ findi (fun _ -> assert false) (create()); false \
    with Not_found -> true
*)

(* let find p a = iget a.arr (findi p a) *)
let find p a =
  let rec loop i =
    if i = a.len then
      raise Not_found
    else 
      let x = iget a.arr i in
      if p x then
        x
      else
        loop (succ i)
  in
  loop 0
(*$T find
  find (fun x -> x mod 3 = 0) (of_list [1;2;3;4;5;6]) = 3
  try ignore @@ find (fun x -> x mod 3 = 0) (of_list [1;2;4;5]); false \
    with Not_found -> true
  try ignore @@ find (fun _ -> assert false) (create()); false \
    with Not_found -> true
*)

let map f src =
  let len = src.len in
  let arr = imake len in
  let i = ref 0 in
  while !i < src.len && !i < len do
    iset arr !i (f (iget src.arr !i));
    incr i
  done;
  {
    resize = src.resize;
    len = BatInt.min len src.len;
    arr = arr;
  }

(*$R map
  let n = 20 in
  let d = init n (fun i -> string_of_int i) in
  let i = ref (-1) in
  let res = map (fun s ->
     assert_bool "DynArray.map1" (Obj.tag (Obj.repr s) = Obj.string_tag); incr i;
     s ^ s) d in
  assert_bool "DynArray.map2" (length res = n);
  iteri (fun idx s -> assert_bool "DynArray.map3" (s ^ s = get res idx)) d
*)

(*$R map
  let n = 40 in
  let newlen = n / 5 in
  let d = init n (fun i -> string_of_int i) in
  let i = ref (-1) in
  let res = map (fun s -> assert (Obj.tag (Obj.repr s) = Obj.string_tag); incr i;
          if !i = 0 then
            for _count = 0 to n - 1 - newlen do delete_last d done;
          true
  ) d in
  assert_bool "DynArray.map4" (length res = newlen)
  (* could be something else if the implementation changed *)
*)

(*$R map
  let n = 40 in
  let d = init n (fun i -> string_of_int i) in
  let i = ref (-1) in
  let res = map (fun s -> assert (Obj.tag (Obj.repr s) = Obj.string_tag); incr i;
          if !i = 0 then
            for _count = 0 to n * 4 do add d "poi" done;
          true
  ) d in
  assert_bool "DynArray.map5" (length res = n)
  (* could be something else if the implementation changed *)
*)

let mapi f src =
  let len = src.len in
  let arr = imake len in
  let i = ref 0 in
  while !i < src.len && !i < len do
    iset arr !i (f !i (iget src.arr !i));
    incr i
  done;
  {
    resize = src.resize;
    len = BatInt.min len src.len;
    arr = arr;
  }

(*$R mapi
  let n = 20 in
  let d = init n (fun i -> string_of_int i) in
  let i = ref (-1) in
  let res = mapi (fun idx s ->
     assert_bool "DynArray.map1" (Obj.tag (Obj.repr s) = Obj.string_tag); incr i;
     assert_bool "DynArray.map2" (!i = idx);
     s ^ s) d in
  assert_bool "DynArray.map3" (length res = n);
  iteri (fun idx s -> assert_bool "DynArray.map3" (s ^ s = get res idx)) d
*)

(*$R mapi
  let n = 40 in
  let newlen = n / 5 in
  let d = init n (fun i -> string_of_int i) in
  let i = ref (-1) in
  let res = mapi (fun idx s -> assert (Obj.tag (Obj.repr s) = Obj.string_tag); incr i;
          assert_bool "DynArray.mapi4" (!i = idx);
          if !i = 0 then
            for _count = 0 to n - 1 - newlen do delete_last d done;
          true
  ) d in
  assert_bool "DynArray.mapi5" (length res = newlen)
  (* could be something else if the implementation changed *)
*)

(*$R mapi
  let n = 40 in
  let d = init n (fun i -> string_of_int i) in
  let i = ref (-1) in
  let res = mapi (fun idx s -> assert (Obj.tag (Obj.repr s) = Obj.string_tag); incr i;
          assert_bool "DynArray.mapi6" (!i = idx);
          if !i = 0 then
            for _count = 0 to n * 4 do add d "poi" done;
          true
  ) d in
  assert_bool "DynArray.mapi7" (length res = n)
  (* could be something else if the implementation changed *)
*)

let modify f a =
  for i = 0 to length a - 1 do
    iset a.arr i (f (iget a.arr i))
  done

(*$T modify
  let a = (of_list [3;2;1]) in \
    modify (fun x -> x + 1) a; to_list a = [4;3;2]
*)

let modifyi f a =
  for i = 0 to length a - 1 do
    iset a.arr i (f i (iget a.arr i))
  done

(*$T modifyi
  let a = (of_list [3;2;1]) in \
    modifyi (fun i x -> i * x) a; to_list a = [0;2;2]
*)

let fold_left f x a =
  let rec loop idx x =
    if idx >= a.len then x else loop (idx + 1) (f x (iget a.arr idx))
  in
  loop 0 x

let fold_right f a x =
  let rec loop idx x =
    if idx < 0 || idx >= a.len then x
    else loop (idx - 1) (f (iget a.arr idx) x)
  in
  loop (a.len - 1) x

(*$R fold_right
  let n = 20 in
  let d = init n (fun i -> string_of_int i) in
  let buffer = Buffer.create 10 in
  let buffer2 = Buffer.create 10 in
  let len = fold_right (fun s count ->
     assert_bool "DynArray.fold_right1" (Obj.tag (Obj.repr s) = Obj.string_tag);
     Buffer.add_string buffer s; count + 1) d 0 in
  assert_bool "DynArray.fold_right2" (len = length d);
  List.iter (fun s -> Buffer.add_string buffer2 s) (List.rev (to_list d));
  assert_bool "DynArray.fold_right3" (Buffer.contents buffer = Buffer.contents buffer2)
*)

(*$R fold_right
  let n = 40 in
  let newlen = n / 5 in
  let d = init n (fun i -> string_of_int i) in
  let i = ref (-1) in
  ignore (fold_right (fun s () ->
          assert (Obj.tag (Obj.repr s) = Obj.string_tag); incr i;
          if !i = 0 then
            for _count = 0 to n - 1 - newlen do delete_last d done
  ) d ())
*)

let fold_lefti f x a =
  let r = ref x in
  for i = 0 to a.len - 1 do
    r := f !r i (iget a.arr i)
  done;
  !r

(*$T fold_lefti
   fold_lefti (fun a i x -> a + i * x) 1 (of_list [2;4;5]) = 1 + 0 + 4 + 10
   fold_lefti (fun a i x -> a + i * x) 1 (create()) = 1
*)

let fold_righti f a x =
  let r = ref x in
  for i = a.len - 1 downto 0 do
    r := f i (iget a.arr i) !r
  done;
  !r

(*$T fold_righti
   fold_righti (fun i x a -> a + i * x) (of_list [2;4;5]) 1 = 1 + 0 + 4 + 10
   fold_righti (fun i x a -> a + i * x) (create()) 1 = 1
*)

let reduce f a =
  if a.len = 0 then
    invalid_arg "DynArray.reduce: empty array"
  else (
    let acc = ref (iget a.arr 0) in
    for i = 1 to a.len-1 do 
      acc := f !acc (iget a.arr i)
    done;
    !acc
  )

(*$T reduce
   reduce (+) (of_list [1;2;3]) = 6
   reduce (fun _ -> assert false) (of_list [1]) = 1
   try reduce (fun _ _ -> ()) (create()); false \
     with Invalid_argument _ -> true
*)

let rev a = 
  let n = a.len - 1 in
  let newarr = imake (n+1) in
  for i = 0 to n do
    iset newarr i (iget a.arr (n-i))
  done;
  {
    resize = a.resize;
    len = a.len;
    arr = newarr;
  }

(*$T
  let a = rev (of_list [1;3;2;5]) in to_list a = [5;2;3;1]
  let a = rev (of_list [1;3;2;5;-1]) in to_list a = [-1;5;2;3;1]
  let a = rev (create()) in empty a
 *)

let rev_in_place a = 
  let n = a.len - 1 in
  let lim = a.len/2 - 1 in
  for i = 0 to lim do
    let x = iget a.arr (n-i) in
    iset a.arr (n-i) (iget a.arr i);
    iset a.arr i x
  done

(*$T
  let a = of_list [1;3;2;5] in rev_in_place a; \
    to_list a = [5;2;3;1]
  let a = of_list [1;3;2;5;-1] in rev_in_place a; \
    to_list a = [-1;5;2;3;1]
  let a = create() in rev_in_place a; \
    empty a
 *)

let max a = reduce Pervasives.max a
(*$T
  max (of_list [1;2;3]) = 3
  max (of_list [2;3;1]) = 3
  try ignore (max (create())); false \
    with Invalid_argument _ -> true
 *)

let min a = reduce Pervasives.min a
(*$T
  min (of_list [1;2;3]) = 1
  min (of_list [2;3;1]) = 1
  try ignore (min (create())); false \
    with Invalid_argument _ -> true
 *)

let min_max a =
  let n = a.len in
  if n = 0 then
    invalid_arg "DynArray.min_max: empty array"
  else
    let mini = ref @@ iget a.arr 0 in
    let maxi = ref @@ iget a.arr 0 in
    for i = 1 to n-1 do
      let x = iget a.arr i in
      if x > !maxi then maxi := x;
      if x < !mini then mini := x
    done;
    (!mini, !maxi)
(*$T min_max
    min_max (of_list [1]) = (1, 1)
    min_max (of_list [1;-2;10;3]) = (-2, 10)
    try ignore (min_max (create())); false \
      with Invalid_argument _ -> true
*)

let sum = reduce (+)
(*$T sum
  sum (of_list [1;2;3]) = 6
  sum (of_list [0]) = 0
 *)

let fsum = reduce (+.)
(*$T fsum
  fsum (of_list [1.0;2.0;3.0]) = 6.0
  fsum (of_list [0.0]) = 0.0
 *)

let kahan_sum a =
  let sum = ref 0. in
  let err = ref 0. in
  let n = a.len - 1 in
  for i = 0 to n do
    let x = iget a.arr i -. !err in
    let new_sum = !sum +. x in
    err := (new_sum -. !sum) -. x;
    sum := new_sum +. 0.;
    (* this suspicious +. 0. is added to help
       the hand of the somewhat flaky unboxing optimizer;
       it hopefully won't be necessary anymore
       in a few OCaml versions *)
  done;
  !sum +. 0.

(*$T kahan_sum
  kahan_sum (create()) = 0.
  kahan_sum (of_list [1.;2.]) = 3.
  let n, x = 1_000, 1.1 in \
  Float.approx_equal (float n *. x) (kahan_sum (init n (fun _ -> x)))
*)

let avg a =
  (float_of_int @@ sum a) /. (float_of_int @@ length a)
(*$T avg
  avg (of_list [1;2;3]) = 2.
  avg (of_list [0]) = 0.
*) 

let favg a =
  (fsum a) /. (float_of_int @@ length a)
(*$T favg
  favg (of_list [1.0; 2.0; 3.0]) = 2.0
  favg (of_list [0.0]) = 0.0
*)



let iter2 f a1 a2 =
  if a1.len <> a2.len then 
    invalid_arg "DynArray.iter2";
  for i = 0 to a1.len - 1 do
    f (iget a1.arr i) (iget a2.arr i);
  done
(*$T iter2
  let x = ref 0 in \
    iter2 (fun a b -> x := !x + a*b) (of_list [1;2;3]) (of_list [4;-5;6]); \
    !x = 12
  try iter2 (fun _ _ -> ()) (of_list [1]) (of_list [1;2;3]); false \
    with Invalid_argument _ -> true
  try iter2 (fun _ _ -> ()) (of_list [1]) (of_list []); false \
    with Invalid_argument _ -> true
*)

let iter2i f a1 a2 =
  if a1.len <> a2.len then
    invalid_arg "DynArray.iter2i";
  for i = 0 to a1.len - 1 do
    f i (iget a1.arr i) (iget a2.arr i);
  done
(*$T iter2i
  let x = ref 0 in \
    iter2i (fun i a b -> x := !x + a*b + i) (of_list [1;2;3]) (of_list [4;-5;6]); \
    !x = 15
  try iter2i (fun _ _ _ -> ()) (of_list [1]) (of_list [1;2;3]); false \
    with Invalid_argument _ -> true
  try iter2i (fun _ _ _ -> ()) (of_list [1]) (of_list []); false \
    with Invalid_argument _ -> true
*)

let for_all2 p a1 a2 =
  let n = a1.len in
  if a2.len <> n then 
    invalid_arg "DynArray.for_all2";
  let rec loop i =
    if i = n then
      true
    else if p (iget a1.arr i) (iget a2.arr i) then
      loop (succ i)
    else
      false
  in
  loop 0

(*$T for_all2
   for_all2 (=) (of_list [1;2;3]) (of_list [3;2;1]) = false
   for_all2 (=) (of_list [1;2;3]) (of_list [1;2;3]) = true
   for_all2 (<>) (of_list [1;2;3]) (of_list [3;2;1]) = false
   try ignore (for_all2 (=) (of_list [1;2;3]) (of_list [1;2;3;4])); false \
     with Invalid_argument _ -> true
   try ignore (for_all2 (=) (of_list [1;2]) (of_list [])); false \
     with Invalid_argument _ -> true
*)

let exists2 p a1 a2 =
  let n = a1.len in
  if a2.len <> n then 
    invalid_arg "DynArray.exists2";
  let rec loop i =
    if i = n then
      false
    else if p (iget a1.arr i) (iget a2.arr i) then
      true
    else
      loop (succ i)
  in
  loop 0

(*$T exists2
   exists2 (=) (of_list [1;2;3]) (of_list [3;2;1])
   exists2 (<>) (of_list [1;2;3]) (of_list [1;2;3]) = false
   try ignore (exists2 (=) (of_list [1;2]) (of_list [3])); false \
     with Invalid_argument _ -> true
*)

let map2 f a1 a2 =
  let n = a1.len in
  if a2.len <> n then 
    invalid_arg "DynArray.map2";
  init n (fun i -> f (iget a1.arr i) (iget a2.arr i))

(*$T map2
   let v = map2 (-) (of_list [1;2;3]) (of_list [6;3;1]) in to_list v = [-5;-1;2]
   let v = map2 (-) (of_list [2;4;6]) (of_list [1;2;3]) in to_list v = [1;2;3]
   try ignore (map2 (-) (of_list [2;4]) (of_list [1;2;3])); false \
     with Invalid_argument _ -> true
   try ignore (map2 (-) (of_list [2;4]) (of_list [3])); false \
     with Invalid_argument _ -> true
*)

let map2i f a1 a2 =
  let n = a1.len in
  if a2.len <> n then 
    invalid_arg "DynArray.map2i";
  init n (fun i -> f i (iget a1.arr i) (iget a2.arr i))

(*$T map2i
   let v = map2i (fun i a b -> a-b + i) (of_list [1;2;3]) (of_list [6;3;1]) in to_list v = [-5;0;4]
   let v = map2i (fun i a b -> a-b + i) (of_list [2;4;6]) (of_list [1;2;3]) in to_list v = [1;3;5]
   try ignore (map2i (fun i a b -> a-b + i) (of_list [2;4]) (of_list [1;2;3])); false \
     with Invalid_argument _ -> true
   try ignore (map2i (fun i a b -> a-b + i) (of_list [2;4]) (of_list [3])); false \
     with Invalid_argument _ -> true
*)

let cartesian_product a1 a2 =
  let na = a1.len in
  let nb = a2.len in
  init
    (na * nb)
    (fun j -> 
      let i = j / nb in
      (iget a1.arr i, iget a2.arr (j - i*nb))
    )

(*$T cartesian_product
  let a = cartesian_product (of_list [1;2]) (of_list ["a";"b"]) in \
    to_list a = [(1,"a"); (1,"b"); (2,"a"); (2,"b")]
*)



let enum d =
  let rec make start =
    let idxref = ref start in
    let next () =
      if !idxref >= d.len then
        raise BatEnum.No_more_elements
      else
        let retval = iget d.arr !idxref in
        incr idxref;
        retval
    and count () =
      if !idxref >= d.len then 0
      else d.len - !idxref
    and clone () =
      make !idxref
    in
    BatEnum.make ~next:next ~count:count ~clone:clone
  in
  make 0

let of_enum e =
  if BatEnum.fast_count e then begin
    let c = BatEnum.count e in
    let arr = imake c in
    BatEnum.iteri (fun i x -> iset arr i x) e;
    {
      resize = default_resizer;
      len = c;
      arr = arr;
    }
  end else
    let d = make 0 in
    BatEnum.iter (add d) e;
    d

(*$Q
  (Q.list Q.small_int) (fun l -> \
    let v = of_list l in \
    enum v |> of_enum |> to_list = l)
*)

let range xs = BatEnum.(--^) 0 (xs.len)



module Exceptionless =
struct
  let find p a =
    try Some (find p a)
    with Not_found -> None

  let findi p a =
    try Some (findi p a)
    with Not_found -> None
end



let unsafe_get a n =
  iget a.arr n

let unsafe_set a n x =
  iset a.arr n x

let unsafe_upd a n f =
  iset a.arr n (f @@ iget a.arr n)

let print ?(first="[|") ?(last="|]") ?(sep="; ") print_a out t =
  BatEnum.print ~first ~last ~sep print_a out (enum t)

(*$T
  Printf.sprintf2 "%a" (print Int.print) (of_list [1;2]) = "[|1; 2|]"
  Printf.sprintf2 "%a" (print Int.print) (of_list []) = "[||]"
*)
