

open Batteries

module A = 
struct
  open Data.Persistent.List
  let _ = iter print_endline ["a"; "b"; "c"]
end

module B =
struct
  open System.OptParse.OptParser

(*  let p = make ~description:"Just testing a few features of Batteries Included" () in
    add ~short_name:'n' ~long_name:"num" p ;
    add *)
end

