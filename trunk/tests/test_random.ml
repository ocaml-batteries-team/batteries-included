(*
  Testing 
  - Random.shuffle 
  - more [open...in...]
  - <|
*)

(*No need to [open Batteries], we're using syntax extensions*)
open Util.Random with self_init () (*Syntax extension*)
open Data.Mutable, Data.Persistent (*Syntax extension*)

let _ = Array.iter print_endline ( shuffle <| args () )

