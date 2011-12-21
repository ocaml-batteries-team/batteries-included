let generic_pow ~zero ~one ~div_two ~mod_two ~mul:( * ) =
  let rec pow a n =
    if      n = zero then one
    else if n = one  then a
    else
      let b = pow a (div_two n) in
b * b * (if mod_two n = zero then one else a)
    in pow

let n = int_of_string (Sys.argv.(1))

let bases = Array.init n (fun _ -> Random.bits ())
and exps = Array.init n (fun _ -> Random.bits ())

let pow1 = generic_pow ~zero:0 ~one:1 ~div_two:(fun n -> n/2) ~mod_two:(fun n -> n mod 2) ~mul:( * )

let pow2 = generic_pow ~zero:0 ~one:1 ~div_two:(fun n -> n asr 1) ~mod_two:(fun n -> n land 1) ~mul:( * )

let pow3 =
  let rec pow a n =
    if      n = 0 then 0
    else if n = 1  then a
    else
      let b = pow a (n asr 1) in
      b * b * (if n land 1 = 0 then 1 else a)
    in pow

let time f =
  let t0 = Sys.time () in
  for i = 0 to n-1 do
    ignore (f bases.(i) exps.(i))
  done;
  Sys.time () -. t0

let () =
  Printf.printf "Time pow1: %f\n" (time pow1);
  Printf.printf "Time pow2: %f\n" (time pow2);
  Printf.printf "Time pow3: %f\n" (time pow3)
