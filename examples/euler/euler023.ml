open Batteries 
open List

let is_abundant n = n < Mathlib.sum_factors n

let max_sum = if Array.length Sys.argv > 1
then int_of_string Sys.argv.(1) else 28123

let () =
  let x = BitSet.create_full max_sum in
  let found = RefList.empty () in
  for i = 12 to max_sum do
    if is_abundant i then begin
      RefList.push found i;
      RefList.iter (fun j -> BitSet.unset x (i+j)) found;
    end
  done;

  BitSet.enum x
  |> Enum.reduce (+) |> print_int;

  print_newline ()
