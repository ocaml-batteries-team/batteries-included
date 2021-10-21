(* cd .. && ocamlbuild -use-ocamlfind benchsuite/bench_set_to_seq.native && _build/benchsuite/bench_set_to_seq.native *)

(* The purpose of this test is to compare different implementation of
   BatSet.to_seq. *)

(* the type BatSet.t is abstract,
   we break the abstraction boundary locally to implement our versions outside the module. *)
type 'a set =
  | Empty
  | Node of 'a set * 'a * 'a set * int
external hide : 'a set -> 'a BatSet.t = "%identity"
external reveal : 'a BatSet.t -> 'a set = "%identity"


module TooStrict = struct
  let rec to_seq m =
    match m with
    | Empty -> BatSeq.nil
    | Node(l, v, r, _) ->
       BatSeq.append (to_seq l) (fun () -> BatSeq.Cons (v, to_seq r))

  let to_seq s = to_seq (reveal s)
end

module Simple = struct
  let rec to_seq m =
    fun () ->
    match m with
    | Empty -> BatSeq.Nil
    | Node(l, v, r, _) ->
       BatSeq.append (to_seq l) (fun () -> BatSeq.Cons (v, to_seq r)) ()

  let to_seq s = to_seq (reveal s)
end

module Enumeration = struct
  type 'a iter = E | C of 'a * 'a set * 'a iter

  let rec cons_iter s t = match s with
    | Empty -> t
    | Node (l, e, r, _) -> cons_iter l (C (e, r, t))

  let to_seq s =
    let rec to_seq iter () =
      match iter with
      | E -> BatSeq.Nil
      | C (e, r, t) ->
         BatSeq.Cons (e, to_seq (cons_iter r t))
    in
    to_seq (cons_iter s E)

  let to_seq s = to_seq (reveal s)
end

let test_input =
  let s = ref BatSet.empty in
  for i = 0 to 9999 do
    s := BatSet.add i !s;
  done;
  !s

let test to_seq =
  test_input
  |> to_seq
  |> BatSeq.length

let () =
  assert (test TooStrict.to_seq = test BatSet.to_seq);
  assert (test Simple.to_seq = test BatSet.to_seq);
  assert (test Enumeration.to_seq = test BatSet.to_seq);
  ()

let () =
  let repeat f n = for _i = 1 to n do ignore (f ()) done in
  Bench.bench_n [
      "too strict", repeat (fun () -> test TooStrict.to_seq);
      "simple", repeat (fun () -> test Simple.to_seq);
      "enumeration", repeat (fun () -> test Enumeration.to_seq);
      "batseq", repeat (fun () -> test BatSet.to_seq);
    ] |>  Bench.run_outputs
