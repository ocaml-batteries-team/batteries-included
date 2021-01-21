(*
 * Copyright (C) 2009 Jeremie Dimino
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

type 'a node =
##V>=4.7## 'a Stdlib.Seq.node =
  | Nil
  | Cons of 'a * 'a t

##V<4.7## and 'a t = unit -> 'a node
##V>=4.7## and 'a t = 'a Stdlib.Seq.t

type 'a mappable = 'a t

let nil () = Nil
let cons e s () = Cons(e, s)

let length s =
  let rec aux acc s = match s () with
    | Nil -> acc
    | Cons(_, s) -> aux (acc + 1) s
  in
  aux 0 s

let rec enum_of_ref r =
  BatEnum.make
    ~next:(fun _ -> match !r () with
      | Nil ->
        raise BatEnum.No_more_elements
      | Cons(e, s) ->
        r := s;
        e)
    ~count:(fun _ -> length !r)
    ~clone:(fun _ -> enum_of_ref (ref !r))

let enum s = enum_of_ref (ref s)

let hd s = match s () with
  | Nil -> invalid_arg "Seq.hd"
  | Cons(e, _s) -> e

let tl s = match s () with
  | Nil -> invalid_arg "Seq.tl"
  | Cons(_e, s) -> s

let first s = match s () with
  | Nil -> invalid_arg "Seq.first"
  | Cons(e, _s) -> e

let last s =
  let rec aux e s = match s () with
    | Nil -> e
    | Cons(e, s) -> aux e s
  in
  match s () with
  | Nil -> invalid_arg "Seq.last"
  | Cons(e, s) -> aux e s

let is_empty s = s () = Nil

let at s n =
  let rec aux s n =
    match s () with
    | Nil -> invalid_arg "Seq.at"
    | Cons(e, s) ->
      if n = 0 then
        e
      else
        aux s (n - 1)
  in
  if n < 0 then
    invalid_arg "Seq.at"
  else
    aux s n

let rec append s1 s2 () = match s1 () with
  | Nil -> s2 ()
  | Cons(e, s1) -> Cons(e, append s1 s2)

let concat s =
  let rec aux current rest () = match current () with
    | Cons(e, s) ->
      Cons(e, aux s rest)
    | Nil ->
      match rest () with
      | Cons(e, s) ->
        aux e s ()
      | Nil ->
        Nil
  in
  aux nil s

let flatten = concat

let make n e =
  let rec aux n () =
    if n = 0 then
      Nil
    else
      Cons(e, aux (n - 1))
  in
  if n < 0 then
    invalid_arg "Seq.make"
  else
    aux n

let init n f =
  let rec aux i () =
    if i = n then
      Nil
    else
      Cons(f i, aux (i + 1))
  in
  if n < 0 then
    invalid_arg "Seq.init"
  else
    aux 0

let of_list l =
  let rec aux l () = match l with
    | [] -> Nil
    | x::l' -> Cons(x, aux l')
  in
  aux l

let empty = nil

(*$T empty
   length empty = 0
 *)

let return x =
    cons x empty

(*$T return
  length (return 123) = 1
  at (return 123) 0 = 123
  equal (return 123) (of_list [123])
 *)

let rec unfold f u =
  match f u with
  | Some(v, r) -> (fun () -> Cons(v, unfold f r))
  | None -> nil

(*$T unfold
   equal (unfold (fun x -> if x = 0 then None else Some (x, x-1)) 5) (of_list [5;4;3;2;1])
   fold_left (fun a b -> b :: a) [] (unfold (fun x -> if x = 0 then None else Some (x, x-1)) 5) = [1;2;3;4;5]
 *)

let rec iter f s = match s () with
  | Nil -> ()
  | Cons(e, s) -> f e; iter f s

let iteri f s =
  let rec iteri f i s = match s () with
  | Nil -> ()
  | Cons(e, s) -> f i e; iteri f (i+1) s
  in iteri f 0 s

(*$T iteri
  try iteri (fun i x -> if i<>x then raise Exit) (of_list [0;1;2;3]); true \
  with Exit -> false
*)

let rec iter2 f s1 s2 = match s1 (), s2 () with
  | Nil, _
  | _, Nil -> ()
  | Cons (x1, s1'), Cons (x2, s2') -> f x1 x2; iter2 f s1' s2'

(*$T iter2
  let r = ref 0 in \
    iter2 (fun i j -> r := !r + i*j) (of_list [1;2]) (of_list [3;2;1]); \
    !r = 3 + 2*2
 *)

let rec map f s () = match s () with
  | Nil -> Nil
  | Cons(x, s) -> Cons(f x, map f s)

let flat_map f s = 
    flatten (map f s)
    
(*$T flat_map
  equal (flat_map of_list (of_list [[1;2;3];[4;5;6]])) (of_list [1;2;3;4;5;6])
*)

let mapi f s =
  let rec mapi f i s () = match s () with
  | Nil -> Nil
  | Cons(x, s) -> Cons(f i x, mapi f (i+1) s)
  in mapi f 0 s

(*$T mapi
  equal (of_list [0;0;0;0]) \
    (mapi (fun i x -> i - x) (of_list [0;1;2;3]))
 *)

let rec map2 f s1 s2 () = match s1 (), s2 () with
  | Nil, _
  | _, Nil -> Nil
  | Cons (x1, s1'), Cons (x2, s2') ->
    Cons (f x1 x2, map2 f s1' s2')

(*$T map2
  equal (map2 (+) (of_list [1;2;3]) (of_list [3;2])) \
    (of_list [4;4])
 *)

let rec fold_left f acc s = match s () with
  | Nil -> acc
  | Cons(e, s) -> fold_left f (f acc e) s

let rec fold_right f s acc = match s () with
  | Nil -> acc
  | Cons(e, s) -> f e (fold_right f s acc)

let reduce f s = match s () with
  | Nil -> invalid_arg "Seq.reduce"
  | Cons(e, s) -> fold_left f e s

let max s = match s () with
  | Nil -> invalid_arg "Seq.max"
  | Cons(e, s) -> fold_left Pervasives.max e s

let min s = match s () with
  | Nil -> invalid_arg "Seq.min"
  | Cons(e, s) -> fold_left Pervasives.min e s

let equal ?(eq=(=)) s1 s2 =
  let rec recurse eq s1 s2 =
    match s1 (), s2 () with
    | Nil, Nil -> true
    | Nil, Cons _
    | Cons _, Nil -> false
    | Cons (x1, s1'), Cons (x2, s2') -> eq x1 x2 && recurse eq s1' s2'
  in
  recurse eq s1 s2

(*$T of_list
  equal (of_list [1;2;3]) (nil |> cons 3 |> cons 2 |> cons 1)
*)

let rec for_all f s = match s () with
  | Nil -> true
  | Cons(e, s) -> f e && for_all f s

let rec exists f s = match s () with
  | Nil -> false
  | Cons(e, s) -> f e || exists f s

let mem e s = exists ((=) e) s

let rec find f s = match s () with
  | Nil ->
    None
  | Cons(e, s) ->
    if f e then
      Some e
    else
      find f s

let rec find_map f s = match s () with
  | Nil ->
    None
  | Cons(e, s) ->
    match f e with
    | None ->
      find_map f s
    | x ->
      x

let rec filter f s () = match s () with
  | Nil ->
    Nil
  | Cons(e, s) ->
    if f e then
      Cons(e, filter f s)
    else
      filter f s ()

let rec filter_map f s () = match s () with
  | Nil ->
    Nil
  | Cons(e, s) ->
    match f e with
    | None ->
      filter_map f s ()
    | Some e ->
      Cons(e, filter_map f s)

let assoc key s = find_map (fun (k, v) -> if k = key then Some v else None) s

let rec take n s () =
  if n <= 0 then
    Nil
  else
    match s () with
    | Nil ->
      Nil
    | Cons(e, s) ->
      Cons(e, take (n - 1) s)

let rec drop n s =
  if n <= 0 then
    s
  else
    match s () with
    | Nil ->
      nil
    | Cons(_e, s) ->
      drop (n - 1) s

let rec take_while f s () = match s () with
  | Nil ->
    Nil
  | Cons(e, s) ->
    if f e then
      Cons(e, take_while f s)
    else
      Nil

let rec drop_while f s = match s () with
  | Nil ->
    nil
  | Cons(e, s) ->
    if f e then
      drop_while f s
    else
      cons e s

let split s = (map fst s, map snd s)

let rec combine s1 s2 () = match s1 (), s2 () with
  | Nil, Nil ->
    Nil
  | Cons(e1, s1), Cons(e2, s2) ->
    Cons((e1, e2), combine s1 s2)
  | _ ->
    invalid_arg "Seq.combine"

let print ?(first="[") ?(last="]") ?(sep="; ") print_a out s = match s () with
  | Nil ->
    BatInnerIO.nwrite out first;
    BatInnerIO.nwrite out last
  | Cons(e, s) ->
    match s () with
    | Nil ->
      BatPrintf.fprintf out "%s%a%s" first print_a e last
    | _ ->
      BatInnerIO.nwrite out first;
      print_a out e;
      iter (BatPrintf.fprintf out "%s%a" sep print_a) s;
      BatInnerIO.nwrite out last

let to_buffer ?(first="[") ?(last="]") ?(sep=";") to_str buff s =
  match s () with
  | Nil -> (Buffer.add_string buff first;
            Buffer.add_string buff last)
  | Cons(e, s) ->
    match s () with
    | Nil -> (Buffer.add_string buff first;
              Buffer.add_string buff (to_str e);
              Buffer.add_string buff last)
    | _ ->
      Buffer.add_string buff first;
      Buffer.add_string buff (to_str e);
      iter (fun e ->
          Buffer.add_string buff sep;
          Buffer.add_string buff (to_str e)
        ) s;
      Buffer.add_string buff last

let to_string ?(first="[") ?(last="]") ?(sep=";") to_str s =
  let buff = Buffer.create 80 in
  to_buffer ~first ~last ~sep to_str buff s;
  Buffer.contents buff

(*$T to_string
  to_string string_of_int (of_list [1;2;3]) = "[1;2;3]"
  to_string ~first:"{" ~sep:"," ~last:"}" string_of_int (of_list [1;2;3]) = "{1,2,3}"
  to_string string_of_int (of_list []) = "[]"
*)

let of_string ?(first="[") ?(last="]") ?(sep=";") of_str s =
  if not (BatString.starts_with s first) then
    raise
      (Invalid_argument
         ("Seq.of_string: wrong prefix: " ^ first ^ " not prefix of " ^ s));
  if not (BatString.ends_with s last) then
    raise
      (Invalid_argument
         ("Seq.of_string: wrong suffix: " ^ last ^ " not suffix of " ^ s));
  let prfx_len = String.length first in
  let sufx_len = String.length last in
  let n = String.length s in
  if n = prfx_len + sufx_len then nil
  else
    let body = BatString.chop ~l:prfx_len ~r:sufx_len s in
    let strings = BatString.nsplit ~by:sep body in
    of_list (List.map of_str strings)

(*$T of_string
  equal (of_string int_of_string "[1;2;3]") (of_list [1;2;3])
  equal (of_string int_of_string "[]") (of_list [])
  equal (of_string ~first:"{" ~sep:"," ~last:"}" int_of_string "{1,2,3}") (of_list [1;2;3])
  try equal (of_string ~first:"{" int_of_string "[1;2;3]") (of_list []) with (Invalid_argument _) -> true
  try equal (of_string ~last:"}" int_of_string "[1;2;3]") (of_list []) with (Invalid_argument _) -> true
*)

module Infix = struct
  (** Infix operators matching those provided by {!BatEnum.Infix} *)

  let ( -- ) a b =
    if b < a then
      nil
    else
      init (b - a + 1) (fun x -> a + x)

  let ( --^ ) a b = a -- (b - 1)

  let ( --. ) (a, step) b =
    let n = int_of_float ((b -. a) /. step) + 1 in
    if n < 0 then
      nil
    else
      init n (fun i -> float_of_int i *. step +. a)

  let ( --- ) a b =
    let n = abs (b - a) in
    if b < a then
      init n (fun x -> a - x)
    else
      a -- b

  let ( --~ ) a b =
    map Char.chr (Char.code a -- Char.code b)

  let ( // ) s f = filter f s

  let ( /@ ) s f = map f s
  let ( @/ ) = map

  let ( //@ ) s f = filter_map f s
  let ( @// ) = filter_map
end

include Infix

module Exceptionless = struct
  (*$< Exceptionless *)
  (* This function could be used to eliminate a lot of duplicate code below...
     let exceptionless_arg f s e =
     try Some (f s)
     with Invalid_argument e -> None
  *)

  let hd s =
    try Some (hd s)
    with Invalid_argument _ -> None

  let tl s =
    try Some (tl s)
    with Invalid_argument _ -> None

  let first s =
    try Some (first s)
    with Invalid_argument _ -> None

  let last s =
    try Some (last s)
    with Invalid_argument _ -> None

  let at s n =
    try Some (at s n)
    with Invalid_argument _ -> None

  (*
  let make n e =
    try Some (make n e)
    with Invalid_argument _ -> None

  let init n e =
    try Some (init n e)
    with Invalid_argument _ -> None
  *)

  let reduce f s =
    try Some (reduce f s)
    with Invalid_argument _ -> None

  let max s =
    try Some (max s)
    with Invalid_argument _ -> None

  let min s =
    try Some (min s)
    with Invalid_argument _ -> None

  let rec combine s1 s2 () = match s1 (), s2 () with
  | Nil, Nil ->
    Nil
  | Cons(e1, s1), Cons(e2, s2) ->
    Cons((e1, e2), combine s1 s2)
  | _ ->
    Nil

  (*$T combine
    equal (combine (of_list [1;2]) (of_list ["a";"b"]))     (of_list [1,"a"; 2,"b"])
    equal (combine (of_list [1;2]) (of_list ["a";"b";"c"])) (of_list [1,"a"; 2,"b"])
    equal (combine (of_list [1;2;3]) (of_list ["a";"b"]))   (of_list [1,"a"; 2,"b"])
  *)
  (*$>*)
end
