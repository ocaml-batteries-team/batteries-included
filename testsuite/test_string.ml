open String

let string = "Jon \"Maddog\" Orwant" 

let test_1 =
  ("Strings: Taking and skipping",
   
   fun () -> let foo s : string list = let e = enum s in 
     [? List : of_enum (f e) | f <- List : open Enum in [take 5; skip 3 |- take 5; take 5 ; identity]]
   in
  let result   = foo string
  and expected = ["Jon \""; "dog\" "; "Orwan"; "t"]
  in
    if result = expected then Testing.Pass
    else Testing.Fail (Printf.sprintf2 "Expected: %a\n\tBatteries:%a" (List.print String.print_quoted) result (List.print String.print_quoted) expected))

let test_2 =
  ("Strings: Start with (1)",
   fun () ->
   let prefix = "Jon" in
   if starts_with string prefix then Testing.Pass
   else Testing.Fail (Printf.sprintf "String %S should start with %S" string prefix))

let test_3 =
  ("Strings: Start with (2)",
   fun () ->
   let prefix = "Jon \"Maddog\" Orwants" in
   if not (starts_with string prefix) then Testing.Pass
   else Testing.Fail (Printf.sprintf "String %S should not start with %S" string prefix))

let test_4 =
  ("Strings: Start with (3)",
   fun () ->
   let prefix = "Orwants" in
   if not (starts_with string prefix) then Testing.Pass
   else Testing.Fail (Printf.sprintf "String %S should not start with %S" string prefix))


let test_5 =
  ("Strings: End with (1)",
   fun () ->
   let suffix = "want" in
   if ends_with string suffix then Testing.Pass
   else Testing.Fail (Printf.sprintf "String %S should end with %S" string suffix))

let test_6 =
  ("Strings: End with (2)",
   fun () ->
   let suffix = "I'm Jon \"Maddog\" Orwant" in
   if not (ends_with string suffix) then Testing.Pass
   else Testing.Fail (Printf.sprintf "String %S should not end with %S" string suffix))

let test_7 =
  ("Strings: End with (3)",
   fun () ->
   let suffix = "Jon" in
   if not(ends_with string suffix) then Testing.Pass
   else Testing.Fail (Printf.sprintf "String %S should not end with %S" string suffix))


