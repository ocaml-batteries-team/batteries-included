let mid1 a b =
  if (0 <= a && 0 <= b) || (a < 0 && b < 0) then
    if a <= b then a + ((b-a)/2) else b + ((a-b)/2)
  else
    let s = a + b in
    if s >= 0 then s/2 else s - s/2

let mid2 a b = (a+b)/2

let mid3 a b =
  if (a >= 0) then
    if (b >= 0) then
      a + (b - a) / 2
    else
      (a+b) / 2
  else
    if (b < 0) then
      a + (b - a) / 2
    else
      (a+b) / 2

let mid4 a b =
  if (0 <= a && 0 <= b) || (a < 0 && b < 0) then
    if a <= b then a + ((b-a)/2) else b + ((a-b)/2)
  else
    (a + b)/2

let array_len = 10000
let xs = Array.init array_len (fun _ -> BatRandom.full_range_int ())

let harn f n =
  for i = 1 to n do
    for j = 0 to array_len-2 do
      ignore (f xs.(j) xs.(j+1));
    done
  done

let () = Bench.(summarize ~alpha:0.05 (bench_n ["mid1", harn mid1; "mid2", harn mid2; "mid3", harn mid3; "mid4", harn mid4]))
