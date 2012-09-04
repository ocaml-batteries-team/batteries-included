open Batteries

let rec factorial = function 1 -> 1 | n -> n * factorial (n-1)

let rec big_factorial acc = function
  | 1 -> acc
  | n -> big_factorial (Big_int.mult_int_big_int n acc) (n-1)

let big_factorial n = big_factorial Big_int.unit_big_int n

let factors i f x =
  let acc = ref i in (* already counted 1 *)
  let max_test = x |> float |> sqrt |> Float.to_int in
  for i = 2 to max_test-1 do
    if x mod i = 0 then acc := f (x/i) (f i !acc)
  done;
  if x mod max_test = 0 && max_test <> 1
  then
    if max_test * max_test = x
    then f max_test !acc (* square - don't double count *)
    else f (x/max_test) (f max_test !acc)
  else !acc

let list_factors n = factors [1] List.cons n
let sum_factors n = factors 1 (+) n

(* list of small primes *)
let easy = [2; 3; 5; 7]
(* let patt = [2; 4; 2; 4; 6; 2; 6; 4; 2; 4; 6; 6; 2; 6; 4; 2; 6; 4; 6; 8; 4; 2;
 4; 2; 4; 8; 6; 4; 6; 2; 4; 6; 2; 6; 6; 4; 2; 4; 6; 2; 6; 4; 2; 4; 2; 10; 2; 10]
 *)


(* The following code builds a pattern list of skip sizes that can be
   used when searching for primes.

   The idea is an extension of checking only odd numbers (greater than
   2) for primality.  Knowing that [2;3;5] are prime, we can check [x;
   x+4; x+6; x+10; x+12; x+16; x+22; x+24; x+30] for primality (given
   an appropriate start value x), as we can know ahead of time that
   [x+1; x+2; x+3; x+5; x+7; ...] are composite by the same reasoning
   that we know that [x+1; x+3; x+5] are composite for odd [x].

   Instead of storing the offsets from [x], we store the offset from
   the previous, so the sequence above is represented by the pattern
   [4;2;4;2;4;6;2;6].

   We derive this sequence from a list of [n] small primes by first
   inspecting integers 2--10^n for being relatively prime to our small
   primes.  We then take the differences between subsequent values,
   and search for a periodic pattern in the results.  The smallest
   sequence that repeats is our desired testing pattern.


 *)

(* Find relative primes *)
let primes easy =
  let pattsearch_max = List.fold_left (fun a _ -> a * 10) 1 easy in
  let filter l n = Enum.filter (fun i -> i mod n > 0) l in
  List.fold_left filter (2--pattsearch_max) easy |> List.of_enum

(* compute successive differences *)
let diffs list =
  let rec loop acc = function
      a :: b :: t -> loop ((b-a)::acc) (b::t)
    | _ -> List.rev acc
  in
  loop [] list

(* test whether the elements of l1 and l2 are the same, ignoring any extra elements in one list *)
let rec list_eq_head l1 l2 = match l1,l2 with
    | h1::t1, h2::t2 -> h1 == h2 && list_eq_head t1 t2
    | [],[] | [],_ | _,[] -> true

(* test whether the sublist [sub] repeats to form the rest of [l] *)
let rec is_rep sub n lst =
  try
    let head,rest = List.split_nth n lst in
    head = sub && is_rep sub n rest
  with Invalid_argument _ -> list_eq_head lst sub

(* given a list, find the smallest sublist that repeats to create that list *)
let find_sub lst =
  let half_len = List.length lst / 2 in
  let rec loop n =
    if n > half_len then
      failwith "No repeating subsequence found, need more test primes";
    let sub = List.take n lst in
    if is_rep sub n lst then sub else loop (n+1)
  in
  loop 1

(* combines the above routines to build a primality testing skip pattern *)
let patt, patt_init =
  let ps = primes easy in
  let patt = diffs ps |> find_sub in
  patt, List.hd ps


open Return (* use batReturn for flow control *)

let test_sequence () = Enum.scanl (+) patt_init (List.enum patt |> Enum.cycle)

(* factor [comp], calling [found] on each prime factor *)
let factor found n =
  (* factor out as many factors of [t] from [n] *)
  label (fun label ->
    let rec test n t =
      (* exit from infinite fold here *)
      if t * t > n then (if n > t then found n; return label ())
      else if n mod t = 0 then
	let quot = n / t in (found t; test quot t)
      else n
    in
    (* test/reduce small primes *)
    let n = List.fold_left test n easy in
    (* infinite fold for rest of divisors, no return value *)
    test_sequence () |> Enum.fold test n |> ignore
  )

let factors n =
  let ret = RefList.empty () in
  factor (RefList.push ret) n;
  assert (RefList.fold_left ( * ) 1 ret = n);
  List.rev (RefList.to_list ret)
