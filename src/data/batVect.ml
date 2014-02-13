(*
 * Vect - Extensible arrays based on ropes
 * Copyright (C) 2007 Mauricio Fernandez <mfp@acm.org>
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

module STRING : sig
  (* this module must provide the following functions: *)
  type 'a t = 'a array
  val length : 'a t -> int
  val make : int -> 'a -> 'a t
  val copy : 'a t -> 'a t
  val unsafe_get : 'a t -> int -> 'a
  val unsafe_set : 'a t -> int -> 'a -> unit
  val sub : 'a t -> int -> int -> 'a t
  val iter : ('a -> unit) -> 'a t -> unit
  val fold_right : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val append : 'a t -> 'a t -> 'a t
  val concat : 'a t list -> 'a t
end = struct include Array include BatArray end

type 'a t =
  | Empty
  | Concat of 'a t * int * 'a t * int * int
  | Leaf of 'a STRING.t

(* these invariants may be incomplete, feel free to improve it *)
let invariants t =
  let rec inv_height = function
    | Empty
    | Leaf _ -> 0
    | Concat (l, _, r, _, h) ->
      assert (h = 1 + max (inv_height l) (inv_height r));
      h in
  let rec inv_length = function
    | Empty -> 0
    | Leaf a -> STRING.length a
    | Concat (l, cl, r, cr, _) ->
      assert (inv_length l = cl);
      assert (inv_length r = cr);
      cl + cr in
  let rec other_inv depth = function
    | Empty -> assert (depth = 0)
    | Leaf a -> assert (STRING.length a > 0)
    | Concat (l, _, r, _, _) ->
      other_inv (depth + 1) l;
      other_inv (depth + 1) r in
  ignore (inv_height t);
  ignore (inv_length t);
  other_inv 0 t

type 'a forest_element = { mutable c : 'a t; mutable len : int }

let str_append = STRING.append
let string_of_string_list = STRING.concat

let singleton x = Leaf [|x|]

(* 48 limits max rope size to 236.10^9 elements on 64 bit,
 * ~ 734.10^6 on 32bit (length fields overflow after that) *)
let max_height = 48

(* actual size will be that plus 1 word header;
 * the code assumes it's an even num.
 * 32 gives up to 50% overhead in the worst case (all leaf nodes near
 * half-filled; 8 words for bookkeeping, 16 words worth of data per leaf node *)
let leaf_size = 16



exception Out_of_bounds

let empty = Empty

(* by construction, there cannot be Empty or Leaf "" leaves *)
let is_empty = function
  | Empty -> true
  | Leaf _ | Concat _ -> false

let height = function
  | Empty | Leaf _ -> 0
  | Concat (_, _, _, _, h) -> h

let length = function
  | Empty -> 0
  | Leaf s -> STRING.length s
  | Concat (_, cl, _, cr, _) -> cl + cr

let make_concat l r =
  let hl = height l and hr = height r in
  let cl = length l and cr = length r in
  Concat (l, cl, r, cr, if hl >= hr then hl + 1 else hr + 1)

let min_len =
  let fib_tbl = Array.make max_height 0 in
  let rec fib n =
    match fib_tbl.(n) with
    | 0 ->
      let last = fib (n - 1) and prev = fib (n - 2) in
      let r = last + prev in
      let r = if r > last then r else last in (* check overflow *)
      fib_tbl.(n) <- r; r
    | n -> n in
  fib_tbl.(0) <- leaf_size + 1; fib_tbl.(1) <- 3 * leaf_size / 2 + 1;
  Array.init max_height (fun i -> if i = 0 then 1 else fib (i - 1))

let max_length = min_len.(Array.length min_len - 1)

let concat_fast l r =
  match l with
  | Empty -> r
  | Leaf _ | Concat _ ->
    match r with
    | Empty -> l
    | Leaf _ | Concat _ -> make_concat l r

(* based on Hans-J. Boehm's *)
let add_forest forest rope len =
  let i = ref 0 in
  let sum = ref empty in
  while len > min_len.(!i+1) do
    if forest.(!i).c <> Empty then begin
      sum := concat_fast forest.(!i).c !sum;
      forest.(!i).c <- Empty
    end;
    incr i
  done;
  sum := concat_fast !sum rope;
  let sum_len = ref (length !sum) in
  while !sum_len >= min_len.(!i) do
    if forest.(!i).c <> Empty then begin
      sum := concat_fast forest.(!i).c !sum;
      sum_len := !sum_len + forest.(!i).len;
      forest.(!i).c <- Empty;
    end;
    incr i
  done;
  decr i;
  forest.(!i).c <- !sum;
  forest.(!i).len <- !sum_len

let concat_forest forest =
  Array.fold_left (fun s x -> concat_fast x.c s) Empty forest

let rec balance_insert rope len forest = match rope with
  | Empty -> ()
  | Leaf _ -> add_forest forest rope len
  | Concat (l, cl, r, cr, h) when h >= max_height || len < min_len.(h) ->
    balance_insert l cl forest;
    balance_insert r cr forest
  | Concat _ as x -> add_forest forest x len (* function or balanced *)

let balance r =
  match r with
  | Empty -> Empty
  | Leaf _ -> r
  | Concat _ ->
    let forest = Array.init max_height (fun _ -> {c = Empty; len = 0}) in
    balance_insert r (length r) forest;
    concat_forest forest

let bal_if_needed l r =
  let r = make_concat l r in
  if height r < max_height then r else balance r

let concat_str l = function
  | Empty | Concat _ -> assert false (*BISECT-VISIT*)
  | Leaf rs as r ->
    let lenr = STRING.length rs in
    match l with
    | Empty -> r
    | Leaf ls ->
      let slen = lenr + STRING.length ls in
      if slen <= leaf_size then Leaf (str_append ls rs)
      else make_concat l r (* height = 1 *)
    | Concat (ll, cll, Leaf lrs, clr, h) ->
      let slen = clr + lenr in
      if clr + lenr <= leaf_size then
        Concat (ll, cll, Leaf (str_append lrs rs), slen, h)
      else
        bal_if_needed l r
    | Concat _ -> bal_if_needed l r

let append_char c r = concat_str r (Leaf (STRING.make 1 c))

let concat l = function
  | Empty -> l
  | Leaf _ as r -> concat_str l r
  | Concat (Leaf rls, rlc, rr, rc, h) as r -> (
      match l with
      | Empty -> r
      | Concat _ -> bal_if_needed l r
      | Leaf ls ->
        let slen = rlc + STRING.length ls in
        if slen <= leaf_size then
          Concat (Leaf (str_append ls rls), slen, rr, rc, h)
        else
          bal_if_needed l r
    )
  | Concat _ as r -> (
      match l with
      | Empty -> r
      | Leaf _ | Concat _ -> bal_if_needed l r
    )

let prepend_char c r = concat (Leaf (STRING.make 1 c)) r

let rec get t i =
  match t with
  | Empty -> raise Out_of_bounds
  | Leaf s ->
    if i >= 0 && i < STRING.length s then STRING.unsafe_get s i
    else raise Out_of_bounds
  | Concat (l, cl, r, cr, _) ->
    if i < cl then get l i
    else get r (i - cl)

let rec set t i x =
  match t with
  | Empty ->
    raise Out_of_bounds
  | Leaf s ->
    if i >= 0 && i < STRING.length s then (
      let s = STRING.copy s in
      STRING.unsafe_set s i x;
      Leaf s
    ) else raise Out_of_bounds
  | Concat (l, cl, r, cr, _) ->
    if i < cl then concat (set l i x) r
    else concat l (set r (i - cl) x)

let at = get

let rec modify t i f =
  match t with
  | Empty ->
    raise Out_of_bounds
  | Leaf s ->
    if i >= 0 && i < STRING.length s then (
      let s = STRING.copy s in
      STRING.unsafe_set s i (f (STRING.unsafe_get s i));
      Leaf s
    ) else raise Out_of_bounds
  | Concat (l, cl, r, cr, _) ->
    if i < cl then concat (modify l i f) r
    else concat l (modify r (i - cl) f)

let of_string = function
  | [||] -> Empty
  | s ->
    let rec loop r s len i =
      if i < len then (* len - i > 0, thus Leaf "" can't happen *)
        loop (concat r (Leaf (STRING.sub s i (BatInt.min (len - i) leaf_size))))
          s len (i + leaf_size)
      else
        r in
    loop Empty s (STRING.length s) 0

let rec make len c =
  let rec concatloop len i r =
    if i <= len then
      concatloop len (i * 2) (concat r r)
    else r in
  if len = 0 then Empty
  else if len <= leaf_size then Leaf (STRING.make len c)
  else
    let rope = concatloop len 2 (of_string (STRING.make 1 c)) in
    concat rope (make (len - length rope) c)

(* overridden argument order below *)
let rec sub start len = function
  | Empty ->
    if start <> 0 || len <> 0 then raise Out_of_bounds else Empty
  | Leaf s ->
    if len > 0 then (* Leaf "" cannot happen *)
      (try Leaf (STRING.sub s start len) with _ -> raise Out_of_bounds)
    else if len < 0 || start < 0 || start > STRING.length s then
      raise Out_of_bounds
    else Empty
  | Concat (l, cl, r, cr, _) ->
    if start < 0 || len < 0 || start + len > cl + cr then raise Out_of_bounds;
    let left =
      if start = 0 then
        if len >= cl then
          l
        else sub 0 len l
      else if start > cl then Empty
      else if start + len >= cl then
        sub start (cl - start) l
      else sub start len l in
    let right =
      if start <= cl then
        let upto = start + len in
        if upto = cl + cr then r
        else if upto < cl then Empty
        else sub 0 (upto - cl) r
      else sub (start - cl) len r in
    concat left right

(* change argument order on Vect.sub *)
let sub v s l = sub s l v

let insert start rope r =
  concat (concat (sub r 0 start) rope) (sub r start (length r - start))

let remove start len r =
  concat (sub r 0 start) (sub r (start + len) (length r - start - len))

let to_string r =
  let rec strings l = function
    | Empty -> l
    | Leaf s -> s :: l
    | Concat (left, _, right, _, _) -> strings (strings l right) left in
  string_of_string_list (strings [] r)

let rec iter f = function
  | Empty -> ()
  | Leaf s -> STRING.iter f s
  | Concat (l, _, r, _, _) -> iter f l; iter f r

type 'a iter = E | C of 'a STRING.t * int * 'a t * 'a iter

let rec cons_iter s t =
  match s with
  | Empty -> t
  | Leaf s -> C (s, 0, Empty, t)
  | Concat (l, _llen, r, _rlen, _h) -> cons_iter l (cons_iter r t)

let rec rev_cons_iter s t =
  match s with
  | Empty -> t
  | Leaf s -> C (s, (STRING.length s - 1), Empty, t)
  | Concat (l, _, r, _, _) -> rev_cons_iter r (rev_cons_iter l t)

let enum_next l () =
  match !l with
  | E -> raise BatEnum.No_more_elements
  | C (s, p, r, t) ->
    if p + 1 = STRING.length s then
      l := cons_iter r t
    else
      l := C (s, p + 1, r, t);
    STRING.unsafe_get s p

let enum_backwards_next l () =
  match !l with
  | E -> raise BatEnum.No_more_elements
  | C (s, p, r, t) ->
    if p = 0 then
      l := rev_cons_iter r t
    else
      l := C (s, p - 1, r, t);
    STRING.unsafe_get s p

let enum_count l () =
  let rec aux n = function
    | E -> n
    | C (s, p, m, t) -> aux (n + (STRING.length s - p) + length m) t
  in aux 0 !l

let rev_enum_count l () =
  let rec aux n = function
    | E -> n
    | C (s, p, m, t) -> aux (n + (p + 1) + length m) t
  in aux 0 !l

let enum t =
  let rec make l =
    let l = ref l in
    let clone () = make !l in
    BatEnum.make ~next:(enum_next l) ~count:(enum_count l) ~clone
  in make (cons_iter t E)

let backwards t =
  let rec make l =
    let l = ref l in
    let clone () = make !l in
    BatEnum.make ~next:(enum_backwards_next l) ~count:(rev_enum_count l) ~clone
  in make (rev_cons_iter t E)

let of_enum e =
  BatEnum.fold (fun acc x -> append_char x acc) empty e

let of_backwards e =
  BatEnum.fold (fun acc x -> prepend_char x acc) empty e

let iteri f r =
  let rec aux f i = function
    | Empty -> ()
    | Leaf s ->
      for j = 0 to STRING.length s - 1 do
        f (i + j) (STRING.unsafe_get s j)
      done
    | Concat (l, cl, r, _, _) -> aux f i l; aux f (i + cl) r in
  aux f 0 r

let rec rangeiter f start len = function
  | Empty -> if start <> 0 || len <> 0 then raise Out_of_bounds
  | Leaf s ->
    let n = start + len in
    let lens = STRING.length s in
    if start >= 0 && len >= 0 && n <= lens then
      for i = start to n - 1 do
        f (STRING.unsafe_get s i)
      done
    else raise Out_of_bounds
  | Concat (l, cl, r, cr, _) ->
    if start < 0 || len < 0 || start + len > cl + cr then raise Out_of_bounds;
    if start < cl then begin
      let upto = start + len in
      if upto <= cl then
        rangeiter f start len l
      else begin
        rangeiter f start (cl - start) l;
        rangeiter f 0 (upto - cl) r
      end
    end else begin
      rangeiter f (start - cl) len r
    end

let rec fold f a = function
  | Empty -> a
  | Leaf s ->
    let acc = ref a in
    for i = 0 to STRING.length s - 1 do
      acc := f !acc (STRING.unsafe_get s i)
    done;
    !acc
  | Concat (l, _, r, _, _) -> fold f (fold f a l) r

let foldi f a v =
  let rec aux i a = function
    | Empty -> a
    | Leaf s ->
      let acc = ref a in
      for j = 0 to STRING.length s - 1 do
        acc := f (i+j) !acc (STRING.unsafe_get s j)
      done;
      !acc
    | Concat (l, cl, r, _, _) -> aux (i+cl) (aux i a l) r in
  aux 0 a v

let fold_left = fold

let fold_right (f:'a -> 'b -> 'b) (v:'a t) (acc:'b)  : 'b =
  let rec aux (acc:'b) = function
    | Empty  -> acc
    | Leaf s -> STRING.fold_right f s acc
    | Concat (l, _, r, _, _) -> aux (aux acc r) l
  in aux acc v

let reduce f v =
  let acc = ref (get v 0) in
  rangeiter (fun e -> acc := f !acc e) 1 (length v - 1) v;
  !acc

let of_array = of_string
let to_array = to_string
let append = append_char
let prepend = prepend_char

let rec map f = function
  | Empty -> Empty
  | Leaf a -> Leaf (BatArray.map f a)
  | Concat (l, cl, r, cr, h) ->
    let l = map f l in
    let r = map f r in
    Concat (l, cl, r, cr, h)

let mapi f v =
  let off = ref 0 in
  map (fun x -> f (BatRef.post_incr off) x) v

let exists f v =
  BatReturn.label (fun label ->
    let rec aux = function
      | Empty -> ()
      | Leaf a -> if BatArray.exists f a then BatReturn.return label true else ()
      | Concat (l, _, r, _, _) -> aux l; aux r in
    aux v;
    false
  )

let for_all f v =
  BatReturn.label (fun label ->
    let rec aux = function
      | Empty -> ()
      | Leaf a -> if not (BatArray.for_all f a) then BatReturn.return label false else ()
      | Concat (l, _, r, _, _) -> aux l; aux r in
    aux v;
    true
  )

let find f v =
  BatReturn.label (fun label ->
    let rec aux = function
      | Empty -> ()
      | Leaf a -> (try BatReturn.return label (BatArray.find f a) with Not_found -> ())
      | Concat (l, _, r, _, _) -> aux l; aux r in
    aux v;
    raise Not_found
  )

let findi f v =
  let off = ref (-1) in
  ignore (find (fun x -> let result = f x in incr off; result) v);
  !off

let partition p v =
  fold_left (fun (yes, no) x -> if p x then (append x yes, no) else (yes, append x no)) (empty, empty) v

let find_all p v =
  fold_left (fun acc x -> if p x then append x acc else acc) empty v

let mem m v = try let _ = find ( ( = ) m ) v in true with Not_found -> false

let memq m v = try let _ = find ( ( == ) m ) v in true with Not_found -> false

let first v = get v 0
let last v = get v (length v - 1)
let shift v = first v, sub v 1 (length v - 1)
let pop v = last v, sub v 0 (length v - 1)

let to_list r =
  let rec aux acc = function
    | Empty -> acc
    | Leaf a -> Array.fold_right (fun x l -> x :: l) a acc
    | Concat (l, _, r, _, _) -> aux (aux acc r) l in
  aux [] r

let filter = find_all

let filter_map f =
  fold (fun acc x ->
    match f x with
    | None   -> acc
    | Some v -> append v acc
  ) Empty

let destructive_set v i x =
  let rec aux i = function
    | Empty  -> raise Out_of_bounds
    | Leaf s ->
      if i >= 0 && i < STRING.length s then
        STRING.unsafe_set s i x
      else raise Out_of_bounds
    | Concat (l, cl, r, cr, _) ->
      if i < cl then aux i l
      else aux (i - cl) r in
  aux i v

let of_list l = of_array (Array.of_list l)

let init n f =
  if n < 0 || n > max_length then raise (Invalid_argument "Vect.init");
  (* Create as many arrays as we need to store all the data *)
  let rec aux off acc =
    if off >= n then acc
    else
      let len = min leaf_size (n - off) in
      let arr = Array.init len (fun i -> f ( off + i ) ) in
      aux (off + len) (arr::acc) in
  let base = aux 0 [] in
  (* And then concatenate them *)
  List.fold_left (fun (acc:'a t) (array:'a array) -> concat (of_array array) acc) (empty:'a t) (base:'a array list)

(*$T init
  init 1000 (fun x -> x * x) |> to_array = Array.init 1000 (fun x -> x * x)
*)

let print ?(first="[|") ?(last="|]") ?(sep="; ") print_a out t =
  BatEnum.print ~first ~last ~sep print_a out (enum t)

let compare cmp_val v1 v2 = BatEnum.compare cmp_val (enum v1) (enum v2)
let equal eq_val v1 v2 = BatEnum.equal eq_val (enum v1) (enum v2)
let ord ord_val v1 v2 =
  let cmp_val = BatOrd.comp ord_val in
  BatOrd.ord0 (BatEnum.compare cmp_val (enum v1) (enum v2))

(* Functorial interface *)

module type RANDOMACCESS =
sig
  type 'a t
  val empty : 'a t
  val get : 'a t -> int -> 'a
  val unsafe_get : 'a t -> int -> 'a
  val set : 'a t -> int -> 'a -> unit
  val unsafe_set : 'a t -> int -> 'a -> unit
  val append : 'a t -> 'a t -> 'a t
  val concat : 'a t list -> 'a t
  val length : 'a t -> int
  val copy : 'a t -> 'a t
  val sub : 'a t -> int -> int -> 'a t
  val make : int -> 'a -> 'a t
  val iter : ('a -> unit) -> 'a t -> unit
  val map : ('a -> 'b) -> 'a t -> 'b t
  val fold_right : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val enum : 'a t -> 'a BatEnum.t
  val backwards : 'a t -> 'a BatEnum.t
  val of_enum : 'a BatEnum.t -> 'a t
  val of_backwards : 'a BatEnum.t -> 'a t
end

module Make(RANDOMACCESS : RANDOMACCESS)
    (PARAM : sig
       val max_height : int
       val leaf_size : int
     end)=
struct
  module STRING = RANDOMACCESS

  type 'a t =
    | Empty
    | Concat of 'a t * int * 'a t * int * int
    | Leaf of 'a STRING.t

  let max_height = PARAM.max_height
  let leaf_size = PARAM.leaf_size

  let min_len =
    let fib_tbl = Array.make max_height 0 in
    let rec fib n =
      match fib_tbl.(n) with
      | 0 ->
        let last = fib (n - 1) and prev = fib (n - 2) in
        let r = last + prev in
        let r = if r > last then r else last in (* check overflow *)
        fib_tbl.(n) <- r; r
      | n -> n in
    fib_tbl.(0) <- leaf_size + 1; fib_tbl.(1) <- 3 * leaf_size / 2 + 1;
    Array.init max_height (fun i -> if i = 0 then 1 else fib (i - 1))

  let max_length = min_len.(Array.length min_len - 1)

  let invariants t =
    let rec inv_height = function
      | Empty
      | Leaf _ -> 0
      | Concat (l, _, r, _, h) ->
        assert (h = 1 + max (inv_height l) (inv_height r));
        h in
    let rec inv_length = function
      | Empty -> 0
      | Leaf a -> STRING.length a
      | Concat (l, cl, r, cr, _) ->
        assert (inv_length l = cl);
        assert (inv_length r = cr);
        cl + cr in
    let rec other_inv depth = function
      | Empty -> assert (depth = 0)
      | Leaf a -> assert (STRING.length a > 0)
      | Concat (l, _, r, _, _) ->
        other_inv (depth + 1) l;
        other_inv (depth + 1) r in
    ignore (inv_height t);
    assert (inv_length t < max_length);
    other_inv 0 t

  type 'a forest_element = { mutable c : 'a t; mutable len : int }

  let str_append = STRING.append
  let string_of_string_list = STRING.concat

  let singleton x = Leaf (STRING.make 1 x)

  exception Out_of_bounds

  let empty = Empty

  (* by construction, there cannot be Empty or Leaf "" leaves *)
  let is_empty = function
    | Empty -> true
    | Leaf _ | Concat _ -> false

  let height = function
    | Empty | Leaf _ -> 0
    | Concat (_, _, _, _, h) -> h

  let length = function
    | Empty -> 0
    | Leaf s -> STRING.length s
    | Concat (_, cl, _, cr, _) -> cl + cr

  let make_concat l r =
    let hl = height l and hr = height r in
    let cl = length l and cr = length r in
    Concat (l, cl, r, cr, if hl >= hr then hl + 1 else hr + 1)

  let concat_fast l r =
    match l with
    | Empty -> r
    | Leaf _ | Concat _ ->
      match r with
      | Empty -> l
      | Leaf _ | Concat _ -> make_concat l r

  (* based on Hans-J. Boehm's *)
  let add_forest forest rope len =
    let i = ref 0 in
    let sum = ref empty in
    while len > min_len.(!i+1) do
      if forest.(!i).c <> Empty then begin
        sum := concat_fast forest.(!i).c !sum;
        forest.(!i).c <- Empty
      end;
      incr i
    done;
    sum := concat_fast !sum rope;
    let sum_len = ref (length !sum) in
    while !sum_len >= min_len.(!i) do
      if forest.(!i).c <> Empty then begin
        sum := concat_fast forest.(!i).c !sum;
        sum_len := !sum_len + forest.(!i).len;
        forest.(!i).c <- Empty;
      end;
      incr i
    done;
    decr i;
    forest.(!i).c <- !sum;
    forest.(!i).len <- !sum_len

  let concat_forest forest =
    Array.fold_left (fun s x -> concat_fast x.c s) Empty forest

  let rec balance_insert rope len forest = match rope with
    | Empty -> ()
    | Leaf _ -> add_forest forest rope len
    | Concat (l, cl, r, cr, h) when h >= max_height || len < min_len.(h) ->
      balance_insert l cl forest;
      balance_insert r cr forest
    | Concat _ as x -> add_forest forest x len (* function or balanced *)

  let balance r =
    match r with
    | Empty -> Empty
    | Leaf _ -> r
    | Concat _ ->
      let forest = Array.init max_height (fun _ -> {c = Empty; len = 0}) in
      balance_insert r (length r) forest;
      concat_forest forest

  let bal_if_needed l r =
    let r = make_concat l r in
    if height r < max_height then r else balance r

  let concat_str l = function
    | Empty | Concat _ -> assert false (*BISECT-VISIT*)
    | Leaf rs as r ->
      let lenr = STRING.length rs in
      match l with
      | Empty -> r
      | Leaf ls ->
        let slen = lenr + STRING.length ls in
        if slen <= leaf_size then Leaf (str_append ls rs)
        else make_concat l r (* height = 1 *)
      | Concat (ll, cll, Leaf lrs, clr, h) ->
        let slen = clr + lenr in
        if clr + lenr <= leaf_size then
          Concat (ll, cll, Leaf (str_append lrs rs), slen, h)
        else
          bal_if_needed l r
      | Concat _ -> bal_if_needed l r

  let append_char c r = concat_str r (Leaf (STRING.make 1 c))

  let concat l = function
    | Empty -> l
    | Leaf _ as r -> concat_str l r
    | Concat (Leaf rls, rlc, rr, rc, h) as r -> (
        match l with
        | Empty -> r
        | Concat _ -> bal_if_needed l r
        | Leaf ls ->
          let slen = rlc + STRING.length ls in
          if slen <= leaf_size then
            Concat (Leaf (str_append ls rls), slen, rr, rc, h)
          else
            bal_if_needed l r
      )
    | Concat _ as r -> (
        match l with
        | Empty -> r
        | Leaf _ | Concat _ -> bal_if_needed l r
      )

  let prepend_char c r = concat (Leaf (STRING.make 1 c)) r

  let rec get t i =
    match t with
    | Empty -> raise Out_of_bounds
    | Leaf s ->
      if i >= 0 && i < STRING.length s then STRING.unsafe_get s i
      else raise Out_of_bounds
    | Concat (l, cl, r, cr, _) ->
      if i < cl then get l i
      else get r (i - cl)

  let rec set t i x =
    match t with
    | Empty ->
      raise Out_of_bounds
    | Leaf s ->
      if i >= 0 && i < STRING.length s then (
        let s = STRING.copy s in
        STRING.unsafe_set s i x;
        Leaf s
      ) else raise Out_of_bounds
    | Concat (l, cl, r, cr, _) ->
      if i < cl then concat (set l i x) r
      else concat l (set r (i - cl) x)

  let at = get

  let rec modify t i f =
    match t with
    | Empty ->
      raise Out_of_bounds
    | Leaf s ->
      if i >= 0 && i < STRING.length s then (
        let s = STRING.copy s in
        STRING.unsafe_set s i (f (STRING.unsafe_get s i));
        Leaf s
      ) else raise Out_of_bounds
    | Concat (l, cl, r, cr, _) ->
      if i < cl then concat (modify l i f) r
      else concat l (modify r (i - cl) f)

  let of_string s =
    if STRING.length s = 0 then Empty
    else
      let rec loop r s len i =
        if i < len then (* len - i > 0, thus Leaf "" can't happen *)
          loop (concat r (Leaf (STRING.sub s i (BatInt.min (len - i) leaf_size))))
            s len (i + leaf_size)
        else
          r in
      loop Empty s (STRING.length s) 0

  let rec make len c =
    let rec concatloop len i r =
      if i <= len then
        concatloop len (i * 2) (concat r r)
      else r in
    if len = 0 then Empty
    else if len <= leaf_size then Leaf (STRING.make len c)
    else
      let rope = concatloop len 2 (of_string (STRING.make 1 c)) in
      concat rope (make (len - length rope) c)

  (* overridden argument order below *)
  let rec sub start len = function
    | Empty ->
      if start <> 0 || len <> 0 then raise Out_of_bounds else Empty
    | Leaf s ->
      if len > 0 then (* Leaf "" cannot happen *)
        (try Leaf (STRING.sub s start len) with _ -> raise Out_of_bounds)
      else if len < 0 || start < 0 || start > STRING.length s then
        raise Out_of_bounds
      else Empty
    | Concat (l, cl, r, cr, _) ->
      if start < 0 || len < 0 || start + len > cl + cr then raise Out_of_bounds;
      let left =
        if start = 0 then
          if len >= cl then
            l
          else sub 0 len l
        else if start > cl then Empty
        else if start + len >= cl then
          sub start (cl - start) l
        else sub start len l in
      let right =
        if start <= cl then
          let upto = start + len in
          if upto = cl + cr then r
          else if upto < cl then Empty
          else sub 0 (upto - cl) r
        else sub (start - cl) len r in
      concat left right

  (* change argument order on Vect.sub *)
  let sub v s l = sub s l v

  let insert start rope r =
    concat (concat (sub r 0 start) rope) (sub r start (length r - start))

  let remove start len r =
    concat (sub r 0 start) (sub r (start + len) (length r - start - len))

  let to_string r =
    let rec strings l = function
      | Empty -> l
      | Leaf s -> s :: l
      | Concat (left, _, right, _, _) -> strings (strings l right) left in
    string_of_string_list (strings [] r)

  let rec iter f = function
    | Empty -> ()
    | Leaf s -> STRING.iter f s
    | Concat (l, _, r, _, _) -> iter f l; iter f r

  type 'a iter = E | C of 'a STRING.t * int * 'a t * 'a iter

  let rec cons_iter s t =
    match s with
    | Empty -> t
    | Leaf s -> C (s, 0, Empty, t)
    | Concat (l, _llen, r, _rlen, _h) -> cons_iter l (cons_iter r t)

  let rec rev_cons_iter s t =
    match s with
    | Empty -> t
    | Leaf s -> C (s, (STRING.length s - 1), Empty, t)
    | Concat (l, _, r, _, _) -> rev_cons_iter r (rev_cons_iter l t)

  let enum_next l () =
    match !l with
    | E -> raise BatEnum.No_more_elements
    | C (s, p, r, t) ->
      if p + 1 = STRING.length s then
        l := cons_iter r t
      else
        l := C (s, p + 1, r, t);
      STRING.unsafe_get s p

  let enum_backwards_next l () =
    match !l with
    | E -> raise BatEnum.No_more_elements
    | C (s, p, r, t) ->
      if p = 0 then
        l := rev_cons_iter r t
      else
        l := C (s, p - 1, r, t);
      STRING.unsafe_get s p

  let enum_count l () =
    let rec aux n = function
      | E -> n
      | C (s, p, m, t) -> aux (n + (STRING.length s - p) + length m) t
    in aux 0 !l

  let rev_enum_count l () =
    let rec aux n = function
      | E -> n
      | C (s, p, m, t) -> aux (n + (p + 1) + length m) t
    in aux 0 !l

  let enum t =
    let rec make l =
      let l = ref l in
      let clone () = make !l in
      BatEnum.make ~next:(enum_next l) ~count:(enum_count l) ~clone
    in make (cons_iter t E)

  let backwards t =
    let rec make l =
      let l = ref l in
      let clone () = make !l in
      BatEnum.make ~next:(enum_backwards_next l) ~count:(rev_enum_count l) ~clone
    in make (rev_cons_iter t E)

  let of_enum e =
    BatEnum.fold (fun acc x -> append_char x acc) empty e

  let of_backwards e =
    BatEnum.fold (fun acc x -> prepend_char x acc) empty e

  let iteri f r =
    let rec aux f i = function
      | Empty -> ()
      | Leaf s ->
        for j = 0 to STRING.length s - 1 do
          f (i + j) (STRING.unsafe_get s j)
        done
      | Concat (l, cl, r, _, _) -> aux f i l; aux f (i + cl) r in
    aux f 0 r

  let rec rangeiter f start len = function
    | Empty -> if start <> 0 || len <> 0 then raise Out_of_bounds
    | Leaf s ->
      let n = start + len in
      let lens = STRING.length s in
      if start >= 0 && len >= 0 && n <= lens then
        for i = start to n - 1 do
          f (STRING.unsafe_get s i)
        done
      else raise Out_of_bounds
    | Concat (l, cl, r, cr, _) ->
      if start < 0 || len < 0 || start + len > cl + cr then raise Out_of_bounds;
      if start < cl then begin
        let upto = start + len in
        if upto <= cl then
          rangeiter f start len l
        else begin
          rangeiter f start (cl - start) l;
          rangeiter f 0 (upto - cl) r
        end
      end else begin
        rangeiter f (start - cl) len r
      end

  let rec fold f a = function
    | Empty -> a
    | Leaf s ->
      let acc = ref a in
      for i = 0 to STRING.length s - 1 do
        acc := f !acc (STRING.unsafe_get s i)
      done;
      !acc
    | Concat (l, _, r, _, _) -> fold f (fold f a l) r

  let foldi f a v =
    let rec aux i a = function
      | Empty -> a
      | Leaf s ->
        let acc = ref a in
        for j = 0 to STRING.length s - 1 do
          acc := f (i+j) !acc (STRING.unsafe_get s j)
        done;
        !acc
      | Concat (l, cl, r, _, _) -> aux (i+cl) (aux i a l) r in
    aux 0 a v

  let fold_left = fold

  let fold_right (f:'a -> 'b -> 'b) (v:'a t) (acc:'b)  : 'b =
    let rec aux (acc:'b) = function
      | Empty  -> acc
      | Leaf s -> STRING.fold_right f s acc
      | Concat (l, _, r, _, _) -> aux (aux acc r) l
    in aux acc v

  let reduce f v =
    let acc = ref (get v 0) in
    rangeiter (fun e -> acc := f !acc e) 1 (length v - 1) v;
    !acc

  let of_array a = of_string (STRING.of_enum (BatArray.enum a))
  let to_array t = BatArray.of_enum (enum t)
  let of_container = of_string
  let to_container = to_string
  let append = append_char
  let prepend = prepend_char

  let rec map f = function
    | Empty -> Empty
    | Leaf a -> Leaf (STRING.map f a)
    | Concat (l, cl, r, cr, h) ->
      let l = map f l in
      let r = map f r in
      Concat (l, cl, r, cr, h)

  let mapi f v =
    let off = ref 0 in
    map (fun x -> f (BatRef.post_incr off) x) v

  let exists f v =
    BatReturn.label (fun label ->
      let rec aux = function
        | Empty -> ()
        | Leaf a -> STRING.iter (fun x -> if f x then BatReturn.return label true) a
        | Concat (l, _, r, _, _) -> aux l; aux r in
      aux v;
      false
    )

  let for_all f v =
    BatReturn.label (fun label ->
      let rec aux = function
        | Empty -> ()
        | Leaf a -> STRING.iter (fun x -> if not (f x) then BatReturn.return label false) a
        | Concat (l, _, r, _, _) -> aux l; aux r in
      aux v;
      true
    )

  let find f v =
    BatReturn.label (fun label ->
      let rec aux = function
        | Empty -> ()
        | Leaf a -> STRING.iter (fun x -> if (f x) then BatReturn.return label x) a
        | Concat (l, _, r, _, _) -> aux l; aux r in
      aux v;
      raise Not_found
    )

  let findi f v =
    let off = ref (-1) in
    ignore (find (fun x -> let result = f x in incr off; result) v);
    !off

  let partition p v =
    fold_left (fun (yes, no) x -> if p x then (append x yes, no) else (yes, append x no)) (empty, empty) v

  let find_all p v =
    fold_left (fun acc x -> if p x then append x acc else acc) empty v

  let mem m v = try let _ = find ( ( = ) m ) v in true with Not_found -> false

  let memq m v = try let _ = find ( ( == ) m ) v in true with Not_found -> false

  let first v = get v 0
  let last v = get v (length v - 1)
  let shift v = first v, sub v 1 (length v - 1)
  let pop v = last v, sub v 0 (length v - 1)

  let to_list r =
    let rec aux acc = function
      | Empty -> acc
      | Leaf a -> STRING.fold_right (fun x l -> x :: l) a acc
      | Concat (l, _, r, _, _) -> aux (aux acc r) l in
    aux [] r

  let filter = find_all

  let filter_map f =
    fold (fun acc x ->
      match f x with
      | None   -> acc
      | Some v -> append v acc
    ) Empty

  let destructive_set v i x =
    let rec aux i = function
      | Empty  -> raise Out_of_bounds
      | Leaf s ->
        if i >= 0 && i < STRING.length s then
          STRING.unsafe_set s i x
        else raise Out_of_bounds
      | Concat (l, cl, r, cr, _) ->
        if i < cl then aux i l
        else aux (i - cl) r in
    aux i v

  let of_list l = of_array (Array.of_list l)

  let init n f =
    if n < 0 || n > max_length then raise (Invalid_argument "Vect.init");
    (* Create as many arrays as we need to store all the data *)
    let rec aux off acc =
      if off >= n then acc
      else
        let len = min leaf_size (n - off) in
        let arr = Array.init len (fun i -> f ( off + i ) ) in
        aux (off + len) (arr::acc) in
    let base = aux 0 [] in
    (* And then concatenate them *)
    List.fold_left (fun (acc:'a t) (array:'a array) -> concat (of_array array) acc) (empty:'a t) (base:'a array list)

  let print ?(first="[|") ?(last="|]") ?(sep="; ") print_a out t =
    BatEnum.print ~first ~last ~sep print_a out (enum t)

end
