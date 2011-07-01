
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

module Infix = struct
  let (>>=) = (>>=)
end
