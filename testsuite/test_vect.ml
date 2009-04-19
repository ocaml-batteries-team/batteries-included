open OUnit
open Vect

(**Initialize data sample*)
let state  = Random.State.make [|0|]
let buffer = Array.of_enum (Enum.take 1000 (Random.State.enum_int state 255))
let vect   = of_array buffer

let print_array out =
  Array.print ~sep:"; " Int.print out
let print_vect  out =
  Vect.print ~sep:"; " Int.print out

let sprint_array a = Printf.sprintf2 "%a" print_array a
let sprint_vect v = Printf.sprintf2 "%a" print_vect v

let test_array_conversion () =
  assert_equal ~printer:sprint_vect
    vect
    (of_array (to_array (of_array (to_array vect))))

let test_init () =
  let f i = i * i in
  let vect = init 1000 f
  and array = Array.init 1000 f
  in
    if Enum.compare ( Int.compare ) (enum vect) (Array.enum array) = 0 then
      () (* pass *)
    else assert_failure
           (Printf.sprintf2 "Hoping: %a\n\tGot:    %a" print_array array print_vect vect)

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
