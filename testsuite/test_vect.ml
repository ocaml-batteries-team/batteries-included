open Vect

(**Initialize data sample*)
let state  = Random.State.make [|0|]
let buffer = Array.of_enum (Enum.take 1000 (Random.State.enum_int state 255))
let vect   = of_array buffer

let print_array out = 
  Array.print ~sep:"; " Int.print out
let print_vect  out =
  Vect.print ~sep:"; " Int.print out

let test_1 =
  ("Vect: converting to/from array",
   fun () ->
     let found = of_array (to_array (of_array (to_array vect)))
     in 
       if found = vect then Testing.Pass
       else Testing.Fail (Printf.sprintf2 "Hoping: %a\n\tGot:    %a" print_vect vect print_vect found)
  )

let test_2 =
  ("Vect: init",
   fun () ->
     let f i = i * i in
     let vect = init 1000 f
     and array= Array.init 1000 f
     in
       if Enum.compare ( Int.compare ) (enum vect) (Array.enum array) = 0 then Testing.Pass
       else Testing.Fail (Printf.sprintf2 "Hoping: %a\n\tGot:    %a" print_array array print_vect vect)
  )

