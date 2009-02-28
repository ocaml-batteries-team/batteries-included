let rec seq i = function
    1 -> i
  | n when n land 1 = 0 -> seq (i+1) (n asr 1)
  | n (* odd *) -> seq (i+1) (3*n+1)

let () =
  let best_i = ref 1
  and best_n0 = ref 1 in
  for n = 1 to 1_000_000 do
    let i = seq 1 n in
    if i > !best_i then
      ( best_i := i; best_n0 := n );
  done;
  print_int !best_n0; print_newline ()
