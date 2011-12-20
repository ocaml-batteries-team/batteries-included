let lt1 (x:int) y = x < y
let lt2 x y = x < y
let lt3 x y = BatInt.Compare.(<) x y

let n = 100_000

let test_array = Array.init n (fun _ -> BatRandom.full_range ())

let test_f f niters =
  for j = 1 to niters do
  for i = 1 to n-1 do
    let x = test_array.(i-1) in
    let y = test_array.(i) in
      ignore (f x y);
  done
  done

let () = Bench.bench_n [
  "Specialized", test_f lt1;
  "Polymorphic", test_f lt2;
  "BatInt.Compare", test_f lt3;
]
