open Batteries
let num_div x = 
  let count = ref 1 in (* already counted 1 *)
  let max_test = x |> float |> sqrt |> Float.to_int in
  for i = 2 to max_test do
    if x mod i = 0 then incr count
  done;
  count := !count * 2; (* every factor < max_test has a corresponding one > *)
  if x mod max_test = 0 then decr count; (* dont double count root if x square *)
  !count

let rec loop i n = 
  let d = num_div n in
  if d > 500 then begin
    print_int n; print_newline(); exit 0
  end else loop (i+1) (n+i+1)

let () = loop 1 1
