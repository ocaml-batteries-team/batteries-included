open Mathlib

let pos = 1_000_000

let tokens = [9; 8; 7; 6; 5; 4; 3; 2; 1; 0]

let rec permute tokens acc pos =
  match tokens with
      [] -> List.rev (acc)
    | [x] -> List.rev (x :: acc)
    | t ->
	let len = List.length t in
	let sub_count = factorial (len-1) in
	let token_pos = pos / sub_count
	and next_pos = pos mod sub_count in
	let found = List.at t token_pos in
Printf.printf "subperm %d: %d (pos %d clust %d) next: %d\n" pos found token_pos sub_count next_pos;
	permute (List.remove t found) (found::acc) next_pos

let () =
  permute tokens [] (pos-1) |> List.iter (fun i -> print_int i);
  print_newline()

