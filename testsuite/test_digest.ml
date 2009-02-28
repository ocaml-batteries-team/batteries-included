(*1. Compute the digest of this file using Legacy.Digest*)


let legacy_result () =
open Legacy.Pervasives in
open Legacy.Digest     in
  let inp    = open_in Sys.argv.(0) in
  let result = channel inp (-1) in
    close_in inp;
    result

(*2. Compute the digest of this file using Batteries.Digest*)

let batteries_result () =
  let inp    = File.open_in Sys.argv.(0) in
  let result = MD5.channel inp (-1)   in
    IO.close_in inp;
    result

(*3. Compare*)
let name    = "Comparing Legacy.Digest and MD5"
let test () =
  let leg = legacy_result    ()
  and bat = batteries_result ()
  in
    if leg = bat then Testing.Pass
    else Testing.Fail (Printf.sprintf "\tLegacy:%s\n\tBatteries:%s" leg bat)

    
