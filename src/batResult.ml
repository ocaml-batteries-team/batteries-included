(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                         The OCaml programmers                          *)
(*                                                                        *)
(*   Copyright 2018 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type ('a, 'e) t = ('a, 'e) BatStdlib.result =
  | Ok  of 'a
  | Error of 'e

let ok v = Ok v
let error e = Error e
let value r ~default = match r with Ok v -> v | Error _ -> default
let get_ok = function Ok v -> v | Error _ -> invalid_arg "result is Error _"
let get_error = function Error e -> e | Ok _ -> invalid_arg "result is Ok _"
let bind r f = match r with Ok v -> f v | Error _ as e -> e
let join = function Ok r -> r | Error _ as e -> e
let map_error f = function Error e -> Error (f e) | Ok _ as v -> v
let fold ~ok ~error = function Ok v -> ok v | Error e -> error e
let iter f = function Ok v -> f v | Error _ -> ()
let iter_error f = function Error e -> f e | Ok _ -> ()
let is_error = function Error _ -> true | Ok _ -> false

let equal ~ok ~error r0 r1 = match r0, r1 with
| Ok v0, Ok v1 -> ok v0 v1
| Error e0, Error e1 -> error e0 e1
| _, _ -> false

let compare ~ok ~error r0 r1 = match r0, r1 with
| Ok v0, Ok v1 -> ok v0 v1
| Error e0, Error e1 -> error e0 e1
| Ok _, Error _ -> -1
| Error _, Ok _ -> 1

let to_list = function Ok v -> [v] | Error _ -> []
let to_seq = function Ok v -> BatSeq.(cons v nil) | Error _ -> BatSeq.nil

let catch f x = try Ok (f x) with e -> Error e
let catch2 f x y = try Ok (f x y) with e -> Error e
let catch3 f x y z = try Ok (f x y z) with e -> Error e

let of_option = function
  | Some x -> Ok x
  | None   -> Error ()

let to_option = function
  | Ok x   -> Some x
  | Error _-> None

let default def = function
  | Ok x  -> x
  | Error _ -> def

let map f = function
  | Error e -> Error e
  | Ok v  -> Ok (f v)
(*$T map
  map succ (Error (-1)) = (Error (-1))
  map succ (Error 0) = (Error 0)
  map succ (Ok 3) = (Ok 4)
*)

let map_both f g = function
  | Error e -> Error (g e)
  | Ok v  -> Ok (f v)
(*$T map_both
  map_both succ pred (Error (-1)) = (Error (-2))
  map_both succ pred (Error 0) = (Error (-1))
  map_both succ pred (Error 1) = (Error 0)
  map_both succ pred (Ok (-1)) = (Ok 0)
  map_both succ pred (Ok 0) = (Ok 1)
  map_both succ pred (Ok 1) = (Ok 2)
*)

let map_default def f = function
  | Ok x -> f x
  | Error _ -> def

let is_ok = function Ok _ -> true | Error _ -> false

let is_bad = function Error _ -> true | Ok _ -> false

let is_exn e = function Error exn -> exn = e | Ok _ -> false

let get = function Ok x -> x | Error e -> raise e

let print print_val oc = function
  | Ok x -> BatPrintf.fprintf oc "Ok(%a)" print_val x
  | Error e -> BatPrintf.fprintf oc "Error(%a)" BatPrintexc.print e


module Monad = struct
  let bind m k = match m with
    | Ok  x      -> k x
    | Error _ as e -> e

  let return x = Ok x

  let (>>=) = bind
end

module Infix = struct
  let (>>=) = Monad.bind
end
