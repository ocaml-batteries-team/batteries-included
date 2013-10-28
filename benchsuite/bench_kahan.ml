open Batteries

let array_fsum t = Array.reduce (+.) t

(* naive kahan version *)
let array_kahan t =
  let sum = ref 0. in
  let err = ref 0. in
  for i = 0 to Array.length t - 1 do
    let x = t.(i) -. !err in
    let new_sum = !sum +. x in
    err := (new_sum -. !sum) -. x;
    sum := new_sum;
  done;
  !sum

(* current implementation optimized for float unboxing *)
let array_kahan_opt t =
  let sum = ref 0. in
  let err = ref 0. in
  for i = 0 to Array.length t - 1 do
    let x = t.(i) -. !err in
    let new_sum = !sum +. x in
    err := (new_sum -. !sum) -. x;
    sum := new_sum +. 0.;
  done;
  !sum +. 0.

let list_fsum t = List.reduce (+.) t

(* naive kahan version *)
let list_kahan t =
  let rec loop sum err = function
    | [] -> sum
    | x::xs ->
      let x = x -. err in
      let new_sum = sum +. x in
      loop new_sum ((new_sum -. sum) -. x) xs
  in loop 0. 0. t

(* current implementation optimized for float unboxing *)
let list_kahan_opt t =
  let li = ref t in
  let continue = ref (!li <> []) in
  let sum = ref 0. in
  let err = ref 0. in
  while !continue do
    match !li with
      | [] -> continue := false
      | x::xs ->
        li := xs;
        let x = x -. !err in
        let new_sum = !sum +. x in
        err := (new_sum -. !sum) -. x;
        sum := new_sum +. 0.;
  done;
  !sum +. 0.


let enum_fsum t = Enum.reduce (+.) t

(* current implementation of fsum *)
let enum_kahan t =
  match Enum.get t with
  | None -> 0.
  | Some i ->
    let sum = ref i in
    let c = ref 0. in
    Enum.iter (fun x ->
      let y = x -. !c in
      let t = !sum +. y in
      c := (t -. !sum) -. y;
      sum := t
    ) t;
    !sum

(* trying to use the same unboxing trick
   (probably won't work though given the higher-order function used)
*)
let enum_kahan_opt t =
  match Enum.get t with
  | None -> 0.
  | Some i ->
    let sum = ref i in
    let c = ref 0. in
    Enum.iter (fun x ->
      let y = x -. !c in
      let t = !sum +. y in
      c := (t -. !sum) -. y;
      sum := t +. 0.
    ) t;
    !sum +. 0.


let () =
  let array = Array.make 1_000_000 1.01 in
  let list = List.make 1_000_000 1.01 in
  let enum () = Array.enum array in
  assert (array_fsum array = list_fsum list);
  assert (list_fsum list = enum_fsum (enum ()));
  assert (array_kahan array = list_kahan list);
  assert (list_kahan list = enum_kahan (enum ()));
  assert (array_kahan_opt array = list_kahan_opt list);
  assert (list_kahan_opt list = enum_kahan_opt (enum ()));
  let repeat f n = for i = 1 to n do ignore (f ()) done in
  Bench.bench_n [
    "array fsum", repeat (fun () -> array_fsum array);
    "array kahan", repeat (fun () -> array_kahan array);
    "array kahan opt", repeat (fun () -> array_kahan_opt array);
  ] |>  Bench.run_outputs;
  Bench.bench_n [
    "list fsum", repeat (fun () -> list_fsum list);
    "list kahan", repeat (fun () -> list_kahan list);
    "list kahan opt", repeat (fun () -> list_kahan_opt list);
  ] |>  Bench.run_outputs;
  Bench.bench_n [
    "enum fsum", repeat (fun () -> enum_fsum (enum ()));
    "enum kahan", repeat (fun () -> enum_kahan (enum ()));
    "enum kahan opt", repeat (fun () -> enum_kahan_opt (enum ()));
  ] |>  Bench.run_outputs;
  ()

(* The sad truth is that the result of these benchmarks vary too much
   from machine to machine to deduce interesting things from them. The
   following conclusions seem to hold:

   - on arrays, kahan summation is indeed four times slower than usual
   summation; on lists and enum the difference is much less visible
   (e.g. imperative kahan outperforms List.reduce-summation on lists)

   - the boxing optimization is a win for arrays and lists as it
   avoids allocation in the loop.

   Anything else is hard to tell.
 *)
