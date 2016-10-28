open Batteries

let digits = List.unfold 0 (fun n -> if n >= 10 then None else Some (n, n + 1))

let () = List.iter (Int.print stdout) digits; print_newline ()
