(*
 * Option - functions for the option type
 * Copyright (C) 2003 Nicolas Cannasse
 *               2008 David Teller (Contributor)
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

exception No_value

type 'a t = 'a option

let may f = function
  | None -> ()
  | Some v -> f v
(*$T may
  let x = ref 3 in may incr (Some x); !x = 4
*)


let map f = function
  | None -> None
  | Some v -> Some (f v)
(*$T map
  map succ None = None
  map succ (Some 3) = (Some 4)
*)

let apply = function
  | None -> (fun x -> x)
  | Some f -> f
(*$T apply
  apply None 3 = 3
  apply (Some succ) 3 = 4
*)


let filter f = function
  | Some x when f x -> Some x
  | _ -> None
(*$T filter
  filter (fun _ -> true) None = None
  filter (fun _ -> true) (Some 3) = Some 3
  filter (fun _ -> false) (Some 3) = None
*)


let default v = function
  | None -> v
  | Some v -> v
(*$T default
  default 3 None = 3
  default 3 (Some 4) = 4
*)

let default_delayed l = function
	| None -> l ()
	| Some v -> v
(*$T default_delayed
  default_delayed (fun () -> 3) None = 3
  default_delayed (fun () -> assert false) (Some 4) = 4
*)

let is_some = function
  | None -> false
  | _ -> true
(*$T is_some
  not (is_some None)
  is_some (Some ())
*)

let is_none = function
  | None -> true
  | _ -> false
(*$T is_none
  is_none None
  not (is_none (Some ()))
*)

let get_exn s e = match s with
  | None   -> raise e
  | Some v -> v
(*$T get_exn
  try get_exn None Exit with Exit -> true
  try get_exn (Some true) Exit with Exit -> false
*)

let get s = get_exn s (Invalid_argument "Option.get")
(*$T get
  try get None with Invalid_argument _ -> true
  try get (Some true) with Invalid_argument _ -> false
*)

let map_default f v = function
  | None -> v
  | Some v2 -> f v2
(*$T map_default
  map_default succ 2 None = 2
  map_default succ 2 (Some 3) = 4
*)

let map_default_delayed f l = function
	| None -> l ()
	| Some v -> f v
(*$T map_default_delayed
  map_default_delayed succ (fun () -> 2) None = 2
  map_default_delayed succ (fun () -> assert false) (Some 3) = 4
*)

let compare ?(cmp=Pervasives.compare) a b = match a with
    None -> (match b with
        None -> 0
      | Some _ -> -1)
  | Some x -> (match b with
        None -> 1
      | Some y -> cmp x y)
(*$T compare
   compare (Some 0) (Some 1) < 0
   compare (Some 0) (Some 0) = 0
   compare (Some 0) (Some (-1)) > 0
   compare None (Some ()) < 0
   compare None None = 0
   compare (Some ()) None > 0
   compare ~cmp:(fun _ _ -> 0) (Some (fun x -> x)) (Some (fun y -> y)) = 0
*)


let eq ?(eq=(=)) x y = match x,y with
  | None, None -> true
  | Some a, Some b -> eq a b
  | _ -> false

(*$T eq
   eq ~eq:(fun a b -> (a land 1) = (b land 1)) (Some 1) (Some 3)
   eq (Some 3) (None) = false
   eq None None = true
*)

let enum = function
  | None   -> BatEnum.from (fun () -> raise BatEnum.No_more_elements)
  | Some e -> BatEnum.singleton e
(*$T enum
   BatList.of_enum (enum None) = []
   BatList.of_enum (enum (Some 3)) = [3]
*)

let of_enum = BatEnum.get
(*$T of_enum
   of_enum (BatList.enum []) = None
   let e = BatList.enum [1; 2; 3] in of_enum e = Some 1 && BatList.of_enum e = [2; 3]
*)

let print print_a out = function
  | None   -> BatInnerIO.nwrite out "None"
  | Some x -> BatPrintf.fprintf out "Some %a" print_a x

let maybe_printer a_printer paren out = function
  | None -> ()
  | Some x -> a_printer paren out x

module Monad =
struct
  type 'a m = 'a option
  let return x = Some x
  let bind m f = match m with
    | None -> None
    | Some x -> f x
end

let bind = Monad.bind
(*$T bind
  bind None (fun s -> Some s) = None
  bind (Some ()) (fun s -> Some s) = Some ()
*)

module Labels =
struct
  let may ~f o = may f o
  let map ~f o = map f o
  let map_default ~f d o = map_default f d o
end

module Infix =
struct
  let ( |? ) x def = default def x
end

include Infix
