open BatPervasives
module R = BatRandom
module U = OUnit

module Int10_base = struct
  type base_t = int
  type t = int option
  let bounds = `c 1, `c 10
  let bounded = BatBounded.opt_of_ord BatInt.ord
  let base_of_t x = x
  let base_of_t_exn x = BatOption.get x
  module Infix = BatInt.Infix
end

(** Only accept integers between 1 and 10, inclusive *)
module Int10 = BatBounded.MakeNumeric(Int10_base)

module Float10_base = struct
  type base_t = float
  type t = float option
  let bounds = `o 1.0, `o 10.0
  let bounded = BatBounded.opt_of_ord BatFloat.ord
  let base_of_t x = x
  let base_of_t_exn x = BatOption.get x
  module Infix = BatFloat.Infix
end

(** Only accept floating point values between 1 and 10, exclusive *)
module Float10 = BatBounded.MakeNumeric(Float10_base)

let assert_make (type s) m to_string (xs : s list) =
  let module B =
    (
      val m :
        BatBounded.NumericSig with type base_u = s and type u = s option
    )
  in
  let min_bound, max_bound = B.bounds in
  let min_check =
    match min_bound with
    | `o a -> (fun x -> x > a)
    | `c a -> (fun x -> x >= a)
    | `u -> (const true)
  in
  let max_check =
    match max_bound with
    | `o a -> (fun x -> x < a)
    | `c a -> (fun x -> x <= a)
    | `u -> (const true)
  in
  List.iter (
    fun x ->
      let printer b = Printf.sprintf "%s (%b)" (to_string x) b in
      U.assert_equal ~printer (max_check x && min_check x) (BatOption.is_some ((B.make |- B.extract) x))
  ) xs;
  ()

let test_make () =
  let xs = BatList.init 100 identity in
  let m =
    (module Int10 : BatBounded.NumericSig with type base_u = int and type u = int option)
  in
  assert_make m string_of_int xs;
  let xs = BatList.init 110 (fun x -> float_of_int x /. 10.0) in
  let m =
    (module Float10 : BatBounded.NumericSig with type base_u = float and type u = float option)
  in
  assert_make m string_of_float xs

let (>::), (>:::) = U.(>::), U.(>:::)

let tests = "Bounded" >::: [
  "value creation" >:: test_make
]
