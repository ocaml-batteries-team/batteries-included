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

let map f = function
	| None -> None
	| Some v -> Some (f v)

let bind f = function
  | None -> None
  | Some v -> f v

let default v = function
	| None -> v
	| Some v -> v

let is_some = function
	| None -> false
	| _ -> true

let is_none = function
	| None -> true
	| _ -> false

let get_exn s e = match s with
        | None   -> raise e
	| Some v -> v

let get s = get_exn s Not_found

let map_default f v = function
	| None -> v
	| Some v2 -> f v2

let compare ?(cmp=Pervasives.compare) a b = match a with
    None -> (match b with
      None -> 0
    | Some _ -> -1)
  | Some x -> (match b with
      None -> 1
    | Some y -> cmp x y)

let eq ?(eq=(=)) x y = match x,y with 
  | None, None -> true 
  | Some a, Some b -> eq a b
  | _ -> false

(**T eq
   eq ~eq:(fun a b -> (a land 1) = (b land 1)) (Some 1) (Some 3)
   eq (Some 3) (None) = false
   eq None None = true
**)

let enum = function
        | None   -> BatEnum.from (fun () -> raise BatEnum.No_more_elements)
        | Some e -> BatEnum.singleton e

let of_enum = BatEnum.get

let print print_a out = function
  | None   -> BatInnerIO.nwrite out "None"
  | Some x -> BatPrintf.fprintf out "Some %a" print_a x

let t_printer a_printer paren out = function
  | Some x ->
      if paren then
        BatIO.write out '(';
      BatIO.nwrite out "Some ";
      a_printer true out x;
      if paren then
        BatIO.write out ')';
  | None ->
      BatIO.nwrite out "None"

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


module Labels =
struct
  let may ~f o = may f o
  let map ~f o = map f o
  let map_default ~f d o = map_default f d o
end
