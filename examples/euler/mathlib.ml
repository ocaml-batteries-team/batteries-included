open Batteries

let rec factorial = function 1 -> 1 | n -> n * factorial (n-1)

let rec big_factorial acc = function
  | 1 -> acc
  | n -> big_factorial (Big_int.mult_int_big_int n acc) (n-1)

let big_factorial n = big_factorial Big_int.unit_big_int n

let factors i f x =
  let acc = ref i in (* already counted 1 *)
  let max_test = x |> float |> sqrt |> Float.to_int in
  for i = 2 to max_test-1 do
    if x mod i = 0 then acc := f (x/i) (f i !acc)
  done;
  if x mod max_test = 0 && max_test <> 1
  then
    if max_test * max_test = x
    then f max_test !acc (* square - don't double count *)
    else f (x/max_test) (f max_test !acc)
  else !acc

let list_factors n = factors [1] List.cons n
let sum_factors n = factors 1 (+) n
