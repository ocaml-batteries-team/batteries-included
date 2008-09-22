open Std
type ('a, 'b) t = ('a, 'b) Std.result

let catch f x =
  try  Ok (f x)
  with e -> Error e

let of_option = function
  | Some x -> Ok x
  | None   -> Error ()

let to_option = function
  | Ok x   -> Some x
  | Error _-> None

let bind m k = match m with
  | Ok  x      -> k x
  | Error _ as e -> e

let (>>=) = bind
