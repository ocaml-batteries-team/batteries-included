open OUnit
open BatVect
open BatPervasives

(**Initialize data sample*)
let state  = BatRandom.State.make [|0|]
let buffer = BatArray.of_enum (BatEnum.take 1000 (BatRandom.State.enum_int state 255))
let vect   = of_array buffer

let print_array out =
  BatArray.print ~sep:"; " BatInt.print out
let print_vect  out =
  BatVect.print ~sep:"; " BatInt.print out

let sprint_vect v = BatPrintf.sprintf2 "%a" print_vect v

let test_array_conversion () =
  assert_equal ~printer:sprint_vect
    vect
    (to_array vect |> of_array |> to_array |> of_array)

let test_init () =
  let f i = i * i in
  let vect = init 1000 f
  and array = Array.init 1000 f
  in
    if BatEnum.compare ( BatInt.compare ) (enum vect) (BatArray.enum array) = 0 then
      () (* pass *)
    else assert_failure
           (BatPrintf.sprintf2 "Hoping: %a\n\tGot:    %a" print_array array print_vect vect)

let test_fold_left () =
  let f i = i * i
  and g i j = i * i + j in
  let vect  = fold_left g 0 (init 1000 f)
  and array = Array.fold_left g 0 (Array.init 1000 f)
  in
    assert_equal ~printer:string_of_int array vect

let test_fold_right () =
  let f i = i * i
  and g i j = i * i + j in
  let vect  = fold_right g (init 1000 f) 0
  and array = Array.fold_right g (Array.init 1000 f) 0
  in
    assert_equal ~printer:string_of_int array vect

let tests = "Vect" >::: [
  "Converting to/from array" >:: test_array_conversion;
  "Init" >:: test_init;
  "Fold_left" >:: test_fold_left;
  "Fold_right" >:: test_fold_right;
]
