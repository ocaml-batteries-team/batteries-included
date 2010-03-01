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
  | Nil
  | Cons of 'a * 'a t

and 'a t = unit -> 'a node

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
  | Nil -> raise (Invalid_argument "Seq.hd")
  | Cons(e, s) -> e

let tl s = match s () with
  | Nil -> raise (Invalid_argument "Seq.tl")
  | Cons(e, s) -> s

let first s = match s () with
  | Nil -> raise (Invalid_argument "Seq.first")
  | Cons(e, s) -> e

let last s =
  let rec aux e s = match s () with
    | Nil -> e
    | Cons(e, s) -> aux e s
  in
  match s () with
    | Nil -> raise (Invalid_argument "Seq.last")
    | Cons(e, s) -> aux e s

let is_empty s = s () = Nil

let at s n =
  let rec aux s n =
    match s () with
      | Nil -> raise (Invalid_argument "Seq.at")
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

let rec iter f s = match s () with
  | Nil -> ()
  | Cons(e, s) -> f e; iter f s

let rec map f s () = match s () with
  | Nil -> Nil
  | Cons(x, s) -> Cons(f x, map f s)

let rec fold_left f acc s = match s () with
  | Nil -> acc
  | Cons(e, s) -> fold_left f (f acc e) s

let rec fold_right f s acc = match s () with
  | Nil -> acc
  | Cons(e, s) -> f e (fold_right f s acc)

let reduce f s = match s () with
  | Nil -> raise (Invalid_argument "Seq.reduce")
  | Cons(e, s) -> fold_left f e s

let max s = match s () with
  | Nil -> raise (Invalid_argument "Seq.max")
  | Cons(e, s) -> fold_left Pervasives.max e s

let min s = match s () with
  | Nil -> raise (Invalid_argument "Seq.min")
  | Cons(e, s) -> fold_left Pervasives.min e s

let rec for_all f s = match s () with
  | Nil -> true
  | Cons(e, s) -> f e && for_all f s

let rec exists f s = match s () with
  | Nil -> false
  | Cons(e, s) -> f e || for_all f s

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
      | Cons(e, s) ->
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
      raise (Invalid_argument "Seq.combine")

let print ?(first="[") ?(last="]") ?(sep="; ") print_a out s = match s () with
  | Nil ->
      BatInnerIO.nwrite out first;
      BatInnerIO.nwrite out last
  | Cons(e, s) ->
      match s () with
        | Nil ->
            BatInnerIO.Printf.fprintf out "%s%a%s" first print_a e last
        | _ ->
            BatInnerIO.nwrite out first;
            print_a out e;
            iter (BatInnerIO.Printf.fprintf out "%s%a" sep print_a) s;
            BatInnerIO.nwrite out last

let t_printer a_printer paren out s =
  print ~first:"[" ~sep:"; " ~last:"]" (a_printer false) out s

let sprint ?(first="[") ?(last="]") ?(sep="; ") print_a s =
  BatInnerIO.Printf.sprintf2 "%a" (print ~first ~last ~sep print_a) s

module Exceptionless = struct
  (* This function could be used to eliminate a lot of duplicate code below...
  let exceptionless_arg f s e =
    try Some (f s)
    with Invalid_argument e -> None
  *)

  let hd s =
    try Some (hd s)
    with Invalid_argument "Seq.hd" -> None

  let tl s =
    try Some (tl s)
    with Invalid_argument "Seq.tl" -> None

  let first s =
    try Some (first s)
    with Invalid_argument "Seq.first" -> None

  let last s =
    try Some (last s)
    with Invalid_argument "Seq.last" -> None

  let at s n =
    try Some (at s n)
    with Invalid_argument "Seq.at" -> None

  (*
  let make n e =
    try Some (make n e)
    with Invalid_argument "Seq.make" -> None

  let init n e =
    try Some (init n e)
    with Invalid_argument "Seq.init" -> None
  *)

  let reduce f s =
    try Some (reduce f s)
    with Invalid_argument "Seq.reduce" -> None

  let max s =
    try Some (max s)
    with Invalid_argument "Seq.max" -> None

  let min s =
    try Some (min s)
    with Invalid_argument "Seq.min" -> None

  let combine s1 s2 =
    try Some (combine s1 s2)
    with Invalid_argument "Seq.combine" -> None
end
