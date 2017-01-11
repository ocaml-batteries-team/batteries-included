
type ('a, 'b) t = ('a, 'b) BatPervasives.result =
  | Ok  of 'a
  | Bad of 'b

let catch f x = try Ok (f x) with e -> Bad e
let catch2 f x y = try Ok (f x y) with e -> Bad e
let catch3 f x y z = try Ok (f x y z) with e -> Bad e

let of_option = function
  | Some x -> Ok x
  | None   -> Bad ()

let to_option = function
  | Ok x   -> Some x
  | Bad _-> None

let default def = function
  | Ok x  -> x
  | Bad _ -> def

let map f = function
  | Bad e -> Bad e
  | Ok v  -> Ok (f v)
(*$T map
  map succ (Bad (-1)) = (Bad (-1))
  map succ (Bad 0) = (Bad 0)
  map succ (Ok 3) = (Ok 4)
*)

let map_both f g = function
  | Bad e -> Bad (g e)
  | Ok v  -> Ok (f v)
(*$T map_both
  map_both succ pred (Bad (-1)) = (Bad (-2))
  map_both succ pred (Bad 0) = (Bad (-1))
  map_both succ pred (Bad 1) = (Bad 0)
  map_both succ pred (Ok (-1)) = (Ok 0)
  map_both succ pred (Ok 0) = (Ok 1)
  map_both succ pred (Ok 1) = (Ok 2)
*)

let map_default def f = function
  | Ok x -> f x
  | Bad _ -> def

let is_ok = function Ok _ -> true | Bad _ -> false

let is_bad = function Bad _ -> true | Ok _ -> false

let is_exn e = function Bad exn -> exn = e | Ok _ -> false

let get = function Ok x -> x | Bad e -> raise e

let print print_val oc = function
  | Ok x -> BatPrintf.fprintf oc "Ok(%a)" print_val x
  | Bad e -> BatPrintf.fprintf oc "Bad(%a)" BatPrintexc.print e


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
