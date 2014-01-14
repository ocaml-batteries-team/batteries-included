let width = 100000
let op_count = 1000
let set_poss = Array.init op_count (fun _ -> Random.int width)
let clear_poss = Array.init op_count (fun _ -> Random.int width)
let get_poss = Array.init op_count (fun _ -> Random.int width)

let fill_arr s =
  for i = 0 to op_count-1 do
    s.(Array.unsafe_get clear_poss i) <- false;
    s.(Array.unsafe_get set_poss i) <- true;
  done

let farr n =
  let s = Array.create width false in
  for _a = 1 to n do
    fill_arr s;
    for _b = 1 to 100 do
      for i = 0 to op_count-1 do
        let _bool : bool =
          s.(Array.unsafe_get get_poss i)
        in
          ()
      done
    done
  done

let count_arr n =
  let s = Array.create width false in
  for _a = 1 to n do
    let count = ref 0 in
    fill_arr s;
    for i = 0 to op_count-1 do
      if s.(i) then incr count;
    done
  done

let next_bit_set_arr n =
  count_arr n (* Code almost look like count_arr *)

open Batteries

let fill_bitset s =
  for i = 0 to op_count-1 do
    BitSet.unset s (Array.unsafe_get clear_poss i);
    BitSet.set s (Array.unsafe_get set_poss i);
  done

let fbs n =
  let s = BitSet.create width in
  for _a = 1 to n do
    fill_bitset s;
    for _b = 1 to 100 do
      for i = 0 to op_count-1 do
        let _bool : bool =
          BitSet.mem s (Array.unsafe_get get_poss i)
        in
          ()
      done
    done
  done

let count_bitset n =
  let s = BitSet.create width in
  for _a = 1 to n do
    fill_bitset s;
    let _count: int = BitSet.count s in
    ()
  done

let next_bit_set_bitset n =
  let s = BitSet.create width in
  for _a = 1 to n do
    let res = ref (Some 0) in
    fill_bitset s;
    while !res <> None do
      match !res with
        | Some idx ->
            res := BitSet.next_set_bit s (idx + 1)
        | None ->
            ()
    done
  done

let next_bit_set_enum n =
  let s = BitSet.create width in
  for _a = 1 to n do
    let () = fill_bitset s in
    let enum = BitSet.enum s in
      BatEnum.iter ignore enum
  done

let () =
  Bench.config.Bench.gc_between_tests <- true;
  Bench.bench_n ["bitset.general", fbs; "array.general", farr]
  |> Bench.summarize ~alpha:0.05;
  Bench.bench_n ["bitset.count", count_bitset; "array.count", count_arr]
  |> Bench.summarize ~alpha:0.05;
  Bench.bench_n ["bitset.next", next_bit_set_bitset;
                 "array.next", next_bit_set_arr;
                 "bitset(enum).next", next_bit_set_enum]
  |> Bench.summarize ~alpha:0.05;
