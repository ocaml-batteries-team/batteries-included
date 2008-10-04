(*Trivial test, used only to check whether the non-threaded version of
  [Batteries] can be opened and linked.*)
open Batteries
open Data.Persistent.List

let _ = iter print_endline ["a"; "b"; "c"];;
let _ = flush_all ();;
