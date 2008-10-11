open Sexplib
open Conv
TYPE_CONV_PATH "Batteries.Standard" (*For Sexplib, Bin-prot...*)

open Std
type ('a, 'b) t = ('a, 'b) Std.result = 
 | Ok  of 'a
 | Bad of 'b with sexp

let catch f x =
  try  Ok (f x)
  with e -> Bad e

let of_option = function
  | Some x -> Ok x
  | None   -> Bad ()

let to_option = function
  | Ok x   -> Some x
  | Bad _-> None

let bind m k = match m with
  | Ok  x      -> k x
  | Bad _ as e -> e

let (>>=) = bind

