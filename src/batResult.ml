
open BatPervasives
type ('a, 'b) t = ('a, 'b) BatPervasives.result =
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

let default def = function
  | Ok x  -> x
  | Bad _ -> def

let map_default def f = function
  | Ok x -> f x
  | Bad _ -> def

let is_ok = function Ok _ -> true | Bad _ -> false

let is_bad = function Bad _ -> true | Ok _ -> false

let get = function Ok x -> x | Bad e -> raise e

let print print_val oc = function 
  | Ok x -> BatInnerIO.Printf.fprintf oc "Ok(%a)" print_val x
  | Bad e -> BatInnerIO.Printf.fprintf oc "Bad(%a)" BatPrintexc.print e


module Monad = struct
  let bind m k = match m with
    | Ok  x      -> k x
    | Bad _ as e -> e

  let return x = Ok x

  let (>>=) = bind
end

module Infix = struct
  let (>>=) = Monad.bind
end

