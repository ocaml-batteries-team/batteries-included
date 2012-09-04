let () =
  let max_search = 100 in
  for n = 1 to max_search do
    for m = n+1 to max_search do
      let a = 2 * m * n
      and b = m * m - n * n
      and c = m * m + n * n
      in
      let s = a + b + c in
      if 1000 mod s = 0 then
	let m = (1000 / s) in
	Printf.printf "mult: %d\n" m;
	Printf.printf "a: %d b: %d c: %d\n" (a*m) (b*m) (c*m);
	print_int (a * b * c * m * m * m); print_newline ();
	exit 0
    done;
  done
