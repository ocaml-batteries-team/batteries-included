(*
  Testing 
  - Random.shuffle 
  - more [open...in...]
  - <|
  - simplified toplevel functions
*)

(*No need to [open Batteries], we're using syntax extensions*)
open Util.Random with self_init () (*Syntax extension*)
open Data.Mutable, Data.Persistent (*Syntax extension*)

(*No need for [let _ = ...], we're using syntax extensions*)
Array.iter print_endline ( shuffle <| args () )

