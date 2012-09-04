
let rand_float _ = (BatRandom.float 2. -. 1.) *. 2. ** (float (BatRandom.int 80 - 40))
let nums = Array.init 10000 rand_float

let test f () = f (BatArray.enum nums)

let () =
  let results = Bench.bench_funs [
    "Enum.reduce", test (BatEnum.reduce (+.));
    "Enum.fsum (Kahan)", test BatEnum.fsum;
    "Array.fold", (fun () -> Array.fold_left (+.) 0. nums);
    "for loop", (fun () -> let s = ref 0. in for i = 0 to 9_999 do s := !s +. nums.(i); done; !s);
    "unsafe for loop", (fun () -> let s = ref 0. in for i = 0 to 9_999 do s := !s +. Array.unsafe_get nums i; done; !s);
  ] () in
  print_endline "For summing an array of 10K floats,";
  Bench.summarize results
