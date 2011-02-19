open Bigarray


let stride = 8 (* bytes *)
let ba_cat = int64  

let blit_string_to_ba s = 
  let ba = Array1.create ba_cat c_layout ((String.length s + stride - 1) / stride) in
  let s' = (Obj.magic (Obj.field (Obj.repr ba) 1) : string) in 
  for i = 0 to String.length s - 1 do 
    String.unsafe_set s' i (String.unsafe_get s i); 
  done;
  ba

let build_srch_ht n_ba =
  let len = Array1.dim n_ba in
  let ht = Hashtbl.create len in
  for i = 0 to len - 1 do
    Hashtbl.add ht (Array1.get n_ba i) i
  done;
  ht

let volnit ~n_ba =
  let ht = build_srch_ht n_ba in
  let ret = ref [] in
  fun ~hs_ba verify -> 
  for i = 0 to Array1.dim hs_ba - 1 do
    try 
      let off = Hashtbl.find ht (Array1.unsafe_get hs_ba i) in
      let srch_off = i * stride - off in
      if verify ~off:srch_off then
	ret := srch_off :: !ret
    with
	Not_found -> ()
  done;
  !ret


let vol n = 
  let s = volnit ~n_ba:(blit_string_to_ba n) in
  fun hs -> s ~hs_ba:(blit_string_to_ba hs) (fun ~off -> String.sub hs off (String.length n) = n)

open Batteries

let rec find_all_aux n hs last acc =
  match 
    try Some (String.find_from hs (last+1) n) with Not_found -> None
  with
    | Some i -> find_all_aux n hs i (last::acc)
    | None -> List.rev (last::acc)

let find_all n hs = 
  try 
    let i0 = String.find hs n in
    find_all_aux n hs i0 []
  with Not_found -> []

let n1 = "abcde"
let hs1 = "abcabcabdeabcdeabbaab"

let na1 = blit_string_to_ba n1
let hsa1 = blit_string_to_ba hs1

let test_vol =
  let v = vol n1 in
  fun () -> v hs1

let test_vol_ba =
  let v = volnit ~n_ba:na1 in
  fun () -> v ~hs_ba:hsa1 (fun ~off -> String.sub hs1 off (String.length n1) = n1)

let test_bf () = find_all n1 hs1

let tests =
  [ "vol", test_vol , ();
    "vol_ba", test_vol_ba, ();
    "batfind", test_bf, ();
  ]

open Benchmark

let () =
  latencyN 1_000_000L tests |> tabulate
