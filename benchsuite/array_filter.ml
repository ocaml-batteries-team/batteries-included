let (|>) x f = f x

let list_filter a p =
  Array.to_list a |> List.filter p |> Array.of_list

open Array
let new_filter xs p =
  let n = length xs in
  (* Use a bitset to store which elements will be in the final array. *)
  let bs = BatBitSet.create n in
  for i = 0 to n-1 do
    if p xs.(i) then BatBitSet.set bs i
  done;
  (* Allocate the final array and copy elements into it. *)
  let n' = BatBitSet.count bs in
  let j = ref 0 in
  init n'
    (fun _ -> match BatBitSet.next_set_bit bs !j with
      | Some i -> j := i+1; xs.(i)
      | None -> assert false (* not enough 1 bits - incorrect count? *)
    )

let old_filter xs p =
  let n = length xs in
  (* Use a bitset to store which elements will be in the final array. *)
  let bs = BatBitSet.create n in
  for i = 0 to n-1 do
    if p xs.(i) then BatBitSet.set bs i
  done;
  (* Allocate the final array and copy elements into it. *)
  let n' = BatBitSet.count bs in
  let j = ref 0 in
  let xs' = init n'
    (fun _ ->
       (* Find the next set bit in the BitSet. *)
       while not (BatBitSet.mem bs !j) do incr j done;
       let r = xs.(!j) in
       incr j;
       r) in
  xs'

let input_gen n = Array.init n (fun x -> x)

let m4 = fun x -> x mod 4 = 0
let m5 = fun x -> x mod 5 = 0
let m10 = fun x -> x mod 10 = 0

let () =
  Bench.config.Bench.samples <- 100;
  Bench.bench_2d ["list_filter", (fun a -> list_filter a m4);
		  "old_filter", (fun a -> old_filter a m4);
		  "new_filter", (fun a -> new_filter a m4);
		 ] ~input_gen (1000, 1_000_000) |> Bench.print_2d "filter.bdata"
