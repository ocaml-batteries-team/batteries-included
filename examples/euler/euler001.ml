open Batteries
open Enum

let say e = e |> map string_of_int |> print ~last:"\n" IO.nwrite stdout
let print_sum e = e |> reduce (+) |> string_of_int |> print_endline

let top = 999

let () =
  (1 -- top)
  |> filter (fun x -> x mod 3 = 0 or x mod 5 = 0)
  |> print_sum

let () =
  let mul3 = (1 -- (top / 3)) |> map ( ( * ) 3)
  and mul5 = (1 -- (top / 5)) |> map ( ( * ) 5)
  in
(*  say (clone mul3);
  say (clone mul5); *)
  merge (<) mul3 mul5 |> uniq |> print_sum

