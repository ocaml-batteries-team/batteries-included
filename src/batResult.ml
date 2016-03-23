
type ('a, 'b) t = ('a, 'b) result =
  | Ok  of 'a
  | Error of 'b

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
