let (|>) x f = f x

let list_partition p a =
  let left, right = Array.to_list a |> List.partition p in
  Array.of_list left, Array.of_list right

open Array
let current_partition p xs =
  let n = length xs in
  (* Use a bitset to store which elements will be in which final array. *)
  let bs = BatBitSet.create n in
  for i = 0 to n-1 do
    if p xs.(i) then BatBitSet.set bs i
  done;
  (* Allocate the final arrays and copy elements into them. *)
  let n1 = BatBitSet.count bs in
  let n2 = n - n1 in
  let j = ref 0 in
  let xs1 = init n1
      (fun _ ->
        (* Find the next set bit in the BitSet. *)
        while not (BatBitSet.mem bs !j) do incr j done;
        let r = xs.(!j) in
        incr j;
        r) in
  let j = ref 0 in
  let xs2 = init n2
      (fun _ ->
        (* Find the next clear bit in the BitSet. *)
        while BatBitSet.mem bs !j do incr j done;
        let r = xs.(!j) in
        incr j;
        r) in
  xs1, xs2

let unixjunkie_partition p a =
  let n = length a in
  if n = 0 then ([||], [||])
  else
    let mask = make n false in
    let ok_count = ref 0 in
    iteri (fun i x ->
        if p x then
          (unsafe_set mask i true;
           incr ok_count)
      ) a;
    let ko_count = n - !ok_count in
    let init = unsafe_get a 0 in
    let ok = make !ok_count init in
    let ko = make ko_count init in
    let j = ref 0 in
    let k = ref 0 in
    iteri (fun i px ->
        let x = unsafe_get a i in
        if px then
          (unsafe_set ok !j x;
           incr j)
        else
          (unsafe_set ko !k x;
           incr k)
      ) mask;
    (ok, ko)

let gasche_partition p xs =
  let n = length xs in
  if n = 0 then ([||], [||]) else begin
    let size_yes = ref 0 in
    let bs = Array.init n (fun i ->
        let b = p (unsafe_get xs i) in
        if b then incr size_yes;
        b) in
    let yes = Array.make !size_yes xs.(0) in
    let no = Array.make (n - !size_yes) xs.(0) in
    let iyes = ref 0 in
    let ino = ref 0 in
    for i = 0 to n - 1 do
      if (unsafe_get bs i) then begin
        unsafe_set yes !iyes (unsafe_get xs i);
        incr iyes;
      end else begin
        unsafe_set no !ino (unsafe_get xs i);
        incr ino;
      end
    done;
    yes, no
  end

let input_gen n = Array.init (1000 * n) (fun x -> x)

let m4 = fun x -> x mod 4 = 0
let m5 = fun x -> x mod 5 = 0
let m10 = fun x -> x mod 10 = 0

let () =
  Bench.config.Bench.samples <- 1000;
  Bench.config.Bench.gc_between_tests <- true;
  Bench.bench_n [
    "list_partition",
    (fun a -> list_partition m4 (input_gen a));
    "current_partition",
    (fun a -> current_partition m4 (input_gen a));
    "unixjunkie_partition", (fun a -> unixjunkie_partition m4 (input_gen a));
    "gasche_partition", (fun a -> gasche_partition m4 (input_gen a));
  ] |> Bench.summarize ~alpha:0.05
