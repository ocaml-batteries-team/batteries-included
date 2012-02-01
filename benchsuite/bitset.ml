

let width = 100000
let op_count = 1000
let set_poss = Array.init op_count (fun _ -> Random.int width)
let clear_poss = Array.init op_count (fun _ -> Random.int width)
let get_poss = Array.init op_count (fun _ -> Random.int width)

let farr n =
  let s = Array.create width false in
  for _a = 1 to n do
    for i = 0 to op_count-1 do
      s.(Array.unsafe_get clear_poss i) <- false;
      s.(Array.unsafe_get set_poss i) <- true;
    done;
    for _b = 1 to 100 do 
      for i = 0 to op_count-1 do
        let _bool : bool = 
          s.(Array.unsafe_get get_poss i)
        in 
          ()
      done
    done
  done

open Batteries

let fbs  n =
  let s = BitSet.create width in
  for _a = 1 to n do
    for i = 0 to op_count-1 do
      BitSet.unset s (Array.unsafe_get clear_poss i);
      BitSet.set s (Array.unsafe_get set_poss i);
    done;
    for _b = 1 to 100 do 
      for i = 0 to op_count-1 do
        let _bool : bool = 
          BitSet.mem s (Array.unsafe_get get_poss i)
        in
          ()
      done
    done
  done


let () = 
  Bench.config.Bench.gc_between_tests <- true;
  Bench.bench_n ["bitset", fbs; "array", farr]
  |> Bench.summarize ~alpha:0.05
