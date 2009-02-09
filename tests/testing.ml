type result =
  | Pass          (**Test succeeded*)
  | Fail of string(**Test failed*)
  | Err  of string(**Other error*)

let successes = ref 0
let failures  = ref 0
let errors    = ref 0

let pass = "\033[34mPASS\033[0m"
let fail = "\033[31mFAIL\033[0m"
let err  = "????"

let result name status =
  match status with
    | Pass -> incr successes; 
	Printf.printf "%-60s [%s]\n%!" name pass
    | Fail e ->
	incr failures;
	Printf.printf "%-60s [%s]\n%s\n%!" name fail e
    | Err e ->
	incr errors;
	Printf.printf "%-60s [%s]\n%s\n%!" name err e

let init   () =
  Printf.printf "Testing starts\n"

let finish () =
  Printf.printf "\nTesting complete.\n\tSuccesses:%d\nFailures:%d\nErrors:%d\n%!" !successes !failures !errors
