(* cd .. && ocamlbuild benchsuite/test_int.native && _build/benchsuite/test_int.native *)


external primitive_int_compare : int -> int -> int = "caml_int_compare"

let test_compare () =
  Printf.printf "test compare against stdlib's compare and a naive impl.";
  
  let length = 1000 in

  let input =
    Array.init length (fun _ -> BatRandom.(full_range (), full_range ())) in

  let output = Array.map (fun (x, y) -> Pervasives.compare x y) input in

  let test cmp n =
    Array.iteri (fun i (x, y) ->
      assert (cmp x y = output.(i));
      for i = 1 to n do
        ignore (cmp x y);
      done)
      input in

  let naive_compare x y =
    (* this code actually mirrors an implementation that has been used
       as BatInt.compare *)
    if x > y then 1
    else if y > x then -1
    else 0 in

  let mfp_compare (x : int) y =
    if x > y then 1
    else if y > x then -1
    else 0 in

  let samples = Bench.bench_n 
    [
      "BatInt.compare", test BatInt.compare;
      "stdlib's compare", test Pervasives.compare;
      "external compare", test primitive_int_compare;
      "mfp's compare", test mfp_compare;
      "naive compare", test naive_compare;
    ] 
  in
  Bench.summarize 0.05 samples

let () =
  test_compare ();
  ()
