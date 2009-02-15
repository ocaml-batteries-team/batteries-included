type result =
  | Pass          (**Test succeeded*)
  | Fail of string(**Test failed*)
  | Err  of string(**Other error*)

let successes = ref 0
let failures  = ref 0
let errors    = ref 0

(*let pass = "\033[34mPASS\033[0m"*)
(*let pass = "\033[01;32mPASS\033[0m"*)
(*let fail = "\033[31mFAIL\033[0m"*)
(*let err  = "\033[37m????\033[0m"*)
let pass = "PASS"
let err  = "????"
let fail = "FAIL"

let result name status =
  match status with
    | Pass -> incr successes; 
	Printf.printf "%-60s [%s]\n%!" name pass
    | Fail e ->
	incr failures;
	Printf.printf "%-60s [%s]\n\t%s\n\n%!" name fail e
    | Err e ->
	incr errors;
	Printf.printf "%-60s [%s]\n\t%s\n\n%!" name err e

let init   () =
  Printf.printf "Testing starts\n"

let finish () =
  Printf.printf "\nTesting complete.\n\tSuccesses:%d\n\tFailures:%d\n\tErrors:%d\n%!" !successes !failures !errors
