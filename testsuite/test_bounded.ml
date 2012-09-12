open BatPervasives
module R = BatRandom
module U = OUnit
open BatPrintf

module Int10_base = struct
  type t = int
  let min = `c 1
  let max = `c 10
  let default_low = None
  let default_high = None
  let bounded = BatBounded.bounding_of_ord ?default_low ?default_high BatInt.ord
end

(** Only accept integers between 1 and 10, inclusive *)
module Int10 = BatBounded.Make(Int10_base)

module Float10_base = struct
  type t = float
  let min = `o 1.0
  let max = `o 10.0
  let default_low = None
  let default_high = None
  let bounded = BatBounded.bounding_of_ord ?default_low ?default_high BatFloat.ord
end

(** Only accept floating point values between 1 and 10, exclusive *)
module Float10 = BatBounded.Make(Float10_base)

let assert_make_make_exn (type s) m to_string x =
  let module B = (val m : BatBounded.S with type u = s) in
  let res = B.make x in
  let res_exn =
    try
      Some (B.make_exn x)
    with
    | B.Out_of_range -> None
  in
  if res = res_exn then
    ()
  else
    U.assert_failure (sprintf "make mismatch for %s" (to_string x))

let test_make_make_exn () =
  let xs = BatList.init 100 identity in
  let m = (module Int10 : BatBounded.S with type u = int) in
  List.iter (assert_make_make_exn m string_of_int) xs;
  let xs = BatList.init 110 (fun x -> float_of_int x /. 10.0) in
  let m = (module Float10 : BatBounded.S with type u = float) in
  List.iter (assert_make_make_exn m string_of_float) xs

let (>:), (>::), (>:::) = U.(>:), U.(>::), U.(>:::)
let (@?) = U.(@?)
let (@!) msg (exn, f) = U.assert_raises ~msg exn f


let tests = "Bounded" >::: [
  "value creation" >:: test_make_make_exn
]
