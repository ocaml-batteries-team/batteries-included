let popcount_test =
  if Sys.word_size = 32 then
    let k1 = 0x55555555 in
    let k2 = 0x33333333 in
    let k3 = 0x0f0f0f0f in
    (fun x ->
      let x = x - (x lsr 1) land k1 in
      let x = ((x lsr 2) land k2) + (x land k2) in
      let x = (x + (x lsr 4)) land k3 in
      let x = x + x lsr 8 in
      (x + x lsr 16) land 0x3f
    )
  else (* word_size = 64 *)
    (* uses int_of_string to hide these constants from the 32-bit compiler *)
    let k1 = int_of_string "0x5555_5555_5555_5555" in
    let k2 = int_of_string "0x3333_3333_3333_3333" in
    let k4 = int_of_string "0x0f0f_0f0f_0f0f_0f0f" in
    (fun x ->
      let x = x - (x lsr 1) land k1 in
      let x = (x land k2) + ((x lsr 2) land k2) in
      let x = (x + x lsr 4) land k4 in
      let x = x + x asr 8 in
      let x = x + x asr 16 in
      let x = x + x asr 32 in
      x land 0x7f
    )

let popcount =
    let k1 = 0x55555555 in
    let k2 = 0x33333333 in
    let k3 = 0x0f0f0f0f in
    (fun x ->
      let x = x - (x lsr 1) land k1 in
      let x = ((x lsr 2) land k2) + (x land k2) in
      let x = (x + (x lsr 4)) land k3 in
      let x = x + x lsr 8 in
      (x + x lsr 16) land 0x3f
    )

let popcount2 =
    (fun x ->
      let k1 = 0x55555555 in
      let k2 = 0x33333333 in
      let k3 = 0x0f0f0f0f in
      let x = x - (x lsr 1) land k1 in
      let x = ((x lsr 2) land k2) + (x land k2) in
      let x = (x + (x lsr 4)) land k3 in
      let x = x + x lsr 8 in
      (x + x lsr 16) land 0x3f
    )

let popcount_sparse x =
  let rec loop n x = if x = 0 then n else loop (n+1) (x land (x-1)) in
  loop 0 x

(* a takes 256k in 32 bits, 512 in 64 bits *)
let a = Array.init (1 lsl 16) (fun i -> popcount i)
let popcount_lookup x =
  a.(x land 0xFFFF) + a.(x lsr 16)

(* a takes 64k *)
let a = String.create (1 lsl 16)
let () =
  for i = 0 to String.length a - 1 do
    a.[i] <- Char.chr (popcount i)
  done
let popcount_lookup2 x =
  Char.code a.[x land 0xFFFF] + Char.code a.[x lsr 16]

(* a takes 1k in 32 bits, 2k in 64 bits *)
let a = Array.init (1 lsl 8) (fun i -> popcount i)
let popcount_byte_lookup x =
  a.(x land 0xFF) + a.(x lsr 8 land 0xFF) + a.(x lsr 16 land 0xFF) + a.(x lsr 24 land 0xFF)

(* a takes 1k in 32 bits, 2k in 64 bits *)
let popcount_byte_lookup2 x =
  let pop = a.(x land 0xFF) in
  let x = x lsr 8 in
  let pop = pop + a.(x land 0xFF) in
  let x = x lsr 8 in
  let pop = pop + a.(x land 0xFF) in
  let x = x lsr 8 in
  let pop = pop + a.(x land 0xFF) in
  pop

let test_sparse =
  fun n ->
    for i = 0 to n do
      ignore (popcount_sparse i)
    done

let test_mask =
  fun n ->
    for i = 0 to n do
      ignore (popcount i)
    done

let test_mask2 =
  fun n ->
    for i = 0 to n do
      ignore (popcount2 i)
    done

let test_masktest =
  fun n ->
    for i = 0 to n do
      ignore (popcount_test i)
    done

let test_lookup =
  fun n ->
    for i = 0 to n do
      ignore (popcount_lookup i)
    done

let test_lookup2 =
  fun n ->
    for i = 0 to n do
      ignore (popcount_lookup2 i)
    done

let test_byte_lookup =
  fun n ->
    for i = 0 to n do
      ignore (popcount_byte_lookup i)
    done

let test_byte_lookup2 =
  fun n ->
    for i = 0 to n do
      ignore (popcount_byte_lookup2 i)
    done

let () =
  let readings =
    Bench.bench_n [
      "Sparse", test_sparse;
      "Mask", test_mask;
      "Mask2", test_mask2;
      "MaskTest", test_masktest;
      "Lookup", test_lookup;
      "Lookup2", test_lookup2;
      "ByteLookup", test_byte_lookup;
      "ByteLookup2", test_byte_lookup2;
    ] in
  Bench.summarize readings
