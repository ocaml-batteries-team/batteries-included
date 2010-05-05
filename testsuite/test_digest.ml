open OUnit

(*1. Compute the digest of this file using Legacy.Digest*)

let legacy_result () =
  let inp    = Pervasives.open_in_bin Sys.argv.(0) in
  let result = Digest.channel inp (-1) in
    Pervasives.close_in inp;
    result

(*2. Compute the digest of this file using Batteries.Digest*)

let batteries_result () =
  let inp    = BatFile.open_in Sys.argv.(0) in
  let result = BatDigest.channel inp (-1)   in
    BatIO.close_in inp;
    result

(*3. Compare*)
let test_legacy_against_batteries () =
  assert_equal ~printer:(Printf.sprintf "%S")
    (legacy_result ()) (batteries_result ())

let tests = "Digest" >::: [
  "Comparing Legacy.Digest and MD5" >:: test_legacy_against_batteries;
]
