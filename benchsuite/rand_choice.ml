open Batteries
open Random

let choice e =
  let a   = BatArray.of_enum e in
  let len = Array.length  a in
  Array.get a (int len)

let choice2 e = Enum.drop (int (Enum.count e)) e; Enum.get_exn e

let choice3 e =
  if Enum.fast_count e then choice2 e
  else choice e

let test n f =
(* data structures to test *)
  let a = Array.init n identity in
  let b = List.init n identity in
  let c () = Random.enum_bits () |> Enum.take n in
  let d () = 1--n in
    fun () ->
      f (Array.enum a);
      f (List.enum b);
      f (c ());
      f (d ())

let test = test 10_000

let () = Bench.bench ["Choice", test choice;
                      "Choice2", test choice2;
                      "Choice3", test choice3;
                     ]
