let rec makeintervals_aux d lo hi acc = function
  | [] -> List.rev ((lo,hi)::acc)
  | h::t when h > hi+d -> makeintervals_aux d h h ((lo,hi)::acc) t
  | h::t (* h <= lim *) -> makeintervals_aux d lo h acc t

let make_intervals d = function
  | [] -> []
  | h::t -> makeintervals_aux d h h [] t

let makeIntervals d =
  let merge ((start,stop) :: tail as s) num =
    if abs(num-stop) <= d then
      (start,num) :: tail
    else
      (num,num) :: s
  in
  function
    | []           -> []
    | head :: tail -> List.fold_left merge [(head,head)] tail

let g = [1;3;5;9;12;13;14]

let tests = [ "fsharp", makeIntervals 2, g;
	      "ocaml", make_intervals 2, g;
	    ]

open Benchmark

let () = tabulate (latencyN 50_000_000L tests)
