open Batteries
open Enum

let max_val = 2_000_000
let max_test = max_val |> float |> sqrt |> Float.to_int

let () = 
  let primes = ref (2--max_val) in
  let s = ref 0 in
  let rec loop () = 
    match get !primes with
      | None -> print_int !s; print_newline ()
      | Some p -> 
	  s := !s + p;
	  if p < max_test then 
	    primes := !primes // (fun x -> x mod p != 0); (* damn inefficient *)
	  loop()
  in
  loop ()
