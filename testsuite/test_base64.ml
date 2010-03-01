open OUnit
open BatBase64

let string = "hello world"

let assert_equal_strings s1 s2 =
  assert_equal ~printer:(fun s -> "“"^s^"”") s1 s2

let hexa s =
  (* really not perf critical *)
  let r = ref "" in
  for i = 0 to String.length s - 1 do
    r := !r ^ (Printf.sprintf "%x" (Char.code s.[i]))
  done;
  !r

let assert_equal_bytes s1 s2 =
  assert_equal ~printer:(fun s -> "“"^s^"” (0x"^(hexa s)^")") s1 s2

let test_encdec_aux str =
  assert_equal_bytes str (str_decode (str_encode str))

let test_decenc_aux str =
  let enc = str_encode str in
  assert_equal_strings enc (str_encode (str_decode enc))

let random_string len =
  let r = String.create len in
  for i = 0 to len - 1
  do r.[i] <- BatRandom.char () done;
  r

let map_generated_data f iters max_len =
  for len = 0 to max_len do
    for i = 1 to iters do
      f (random_string len)
    done done


let test_encdec () =
  map_generated_data test_encdec_aux 4 50

let test_decenc () =
  map_generated_data test_decenc_aux 4 50


let tests = "Base64" >::: [
  "Decode undoes encode"          >:: test_encdec;
  "Encode undoes decode"          >:: test_decenc;
  (*"Encode works as expected"      >:: test_enc;
  "Decode works as expected"      >:: test_dec;*)
]
