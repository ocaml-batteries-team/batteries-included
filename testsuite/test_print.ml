open OUnit
open Gc

let few_tests = 10
let many_tests= 100000

let run_legacy number_of_runs = 
begin
  Gc.major ();
  let devnull = Legacy.Pervasives.open_out "/dev/null" in
  foreach (1 -- number_of_runs) (fun _ ->
    Legacy.Printf.fprintf devnull "%a%!" (fun ch () -> Legacy.Printf.fprintf ch "Hello, world!") ()
  );
  Legacy.Pervasives.close_out devnull;
  Gc.major ();
  (Gc.stat()).live_words
end

let test_leak_legacy () =
  let words_few = run_legacy few_tests  in
  let words_many= run_legacy many_tests in
    if words_few < words_many then assert_failure (Printf.sprintf "Memory use grew by %d" (words_many - words_few))

let run_oldstyle number_of_runs = 
open Printf in
  begin
  Gc.major ();
  foreach (1 -- number_of_runs) (fun _ ->
    fprintf stdnull "%a%!" (fun ch () -> fprintf ch "Hello, world!") ()
  );
  Gc.major ();
  (Gc.stat()).live_words
end

let test_leak_oldstyle () =
  let words_few = run_oldstyle few_tests  in
  let words_many= run_oldstyle many_tests in
    if words_few < words_many then assert_failure (Printf.sprintf "Memory use grew by %d" (words_many - words_few))

let run_newstyle number_of_runs = 
open Print in
  begin
  Gc.major ();
  let printer_hello k () = k (fun ch -> fprintf ch p"Hello, world!") in
  foreach (1 -- number_of_runs) (fun _ ->
    fprintf stdnull p"{%hello}%!" ()
  );
  Gc.major ();
  (Gc.stat()).live_words
end

let test_leak_newstyle () =
  let words_few = run_newstyle few_tests  in
  let words_many= run_newstyle many_tests in
    if words_few < words_many then assert_failure (Printf.sprintf "Memory use grew by %d" (words_many - words_few))


let tests = "Print" >::: [
  "Legacy printing memory leak" >:: test_leak_legacy ;
  "Old-style printing memory leak" >:: test_leak_oldstyle ;
  "New-style printing memory leak" >:: test_leak_newstyle
]
