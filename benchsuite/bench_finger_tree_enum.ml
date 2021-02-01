(* cd .. && ocamlbuild -use-ocamlfind benchsuite/bench_finger_tree_enum.native && _build/benchsuite/bench_finger_tree_enum.native *)

module Fg = BatFingerTree

let test_input =
  let s = ref Fg.empty in
  for i = 0 to 999_999 do
    s := Fg.snoc !s i;
  done;
  !s

let () =
  assert (BatList.of_enum (Fg.enum test_input) = Fg.to_list test_input);
  assert (BatList.of_enum (Fg.backwards test_input) = Fg.to_list_backwards test_input);
  assert (BatList.of_enum (Fg.backwards test_input) = List.rev (Fg.to_list test_input));
  ()

let test to_enum n =
  for i = 1 to n do
    let enum = to_enum test_input in
    BatEnum.iter ignore enum
  done

let () =
  Bench.bench_n [
      "implemented", test Fg.enum;
    ] |>  Bench.run_outputs
