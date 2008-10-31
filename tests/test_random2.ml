(*
  Testing Random.shuffle and more [open...in...]
*)

(*No need to [open Batteries]*)
open Util.Random with self_init () (*Syntax extension*)
open Data.Mutable, System          (*Syntax extension*)

let _ = Array.iter print_endline (shuffle (File.lines_of Sys.argv.(1)))

