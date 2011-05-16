
open BatStd
type ('a, 'b) t = ('a, 'b) BatStd.result = 
 | Ok  of 'a
 | Bad of 'b

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

let return x = Ok x

let default def = function
  | Ok x  -> x
  | Bad _ -> def

let default_map def f = function
  | Ok x -> f x
  | Bad _ -> def
