

let width = 100000
let op_count = 1000
let set_poss = Array.init op_count (fun _ -> Random.int width)
let clear_poss = Array.init op_count (fun _ -> Random.int width)
let get_poss = Array.init op_count (fun _ -> Random.int width)

let f (make,get,clear,set) n =
  let s = make width in
  for _a = 1 to n do
    for i = 0 to op_count-1 do
      get s get_poss.(i);
      clear s clear_poss.(i);
      set s set_poss.(i);
    done
  done


module Eratosthenes =
struct
  let create n =
    let () = assert (n < Sys.max_array_length) in
    let arr = Array.make (n + 1) true in
    let rec cross start step =
      if 0 <= start && start <= n then
        begin
          if arr.(start) then arr.(start) <- false;
          cross (start + step) step
        end
      else
        ()
    in

      for i = 2 to n do
        if arr.(i) then
          cross (i * i) i
      done;

      arr

  let fold f a t =
    let len = Array.length t in

    let rec fold' pos a =
      if pos < len then
        if t.(pos) then
          fold' (pos + 1) (f a pos)
        else
          fold' (pos + 1) a
      else
        a
    in
      fold' 2 a

  let list t =
    let rlst = ref [] in
      for i = (Array.length t) - 1 downto 2 do
        if t.(i) then
          rlst := i :: !rlst
      done;
      !rlst

end

let is_prime t n =
  if n < Array.length t then
    t.(n)
  else
    begin
      let idx = ref 2 in
      let res = ref true in
      let sqrt = int_of_float (sqrt (float_of_int n)) in
        while !res && !idx < Array.length t do
          if t.(!idx) then
            res := (n mod !idx) <> 0;
          incr idx
        done;
        (* We reach the end of primes table *)
        while !res && !idx <= sqrt do
          res := (n mod !idx) <> 0;
          incr idx
        done;
        !res
    end


open Batteries

let bs = BitSet.(create, mem, unset, set)
let arr = (fun n -> Array.create n false), Array.get,
  (fun a n -> a.(n) <- false),
  (fun a n -> a.(n) <- true)




let () = Bench.bench_n ["bitset", f bs; "array", f arr]
  |> Bench.summarize ~alpha:0.05
