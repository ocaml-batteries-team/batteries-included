let rec makeintervals_aux d lo hi acc = function
  | [] -> List.rev ((lo,hi)::acc)
  | h::t when h > hi+d -> makeintervals_aux d h h ((lo,hi)::acc) t
  | h::t (* h <= lim *) -> makeintervals_aux d lo h acc t

let make_intervals d = function
  | [] -> []
  | h::t -> makeintervals_aux d h h [] t

let makeIntervals d =
  let merge s num =
    match s with
      | (start,stop) :: tail ->
        if abs(num-stop) <= d then
          (start,num) :: tail
        else
          (num,num) :: s
      | _ -> assert false
  in
  function
    | []           -> []
    | head :: tail -> List.fold_left merge [(head,head)] tail

let g = [1;3;5;9;12;13;14]

let repeat f n =
  for _i = 1 to n do
    ignore (f g)
  done

let tests = [ "fsharp", repeat (makeIntervals 2);
              "ocaml", repeat (make_intervals 2);
            ]

let () = Bench.bench_n tests |> Bench.run_outputs
