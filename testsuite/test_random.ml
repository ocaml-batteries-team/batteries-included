open OUnit
open BatPervasives

let assert_equal_arrays =
  assert_equal ~printer:(BatIO.to_string (BatArray.print BatInt.print))

let take_array n e = BatArray.of_enum (BatEnum.take n e)

let test_enum_helper reset create modify =
  let make n = take_array n (create ()) in

  (* Enumerations constructed for the same state should be equal. *)
  let () = reset () in
  let a = make 10 in
  let () = reset () in
  let b = make 10 in
  let () = assert_equal_arrays a b in

  (* The states should be shared: if the state is modified then the second
     stream should be different. *)
  let () = reset () in
  let a = make 1000 in
  let () = reset () in
  let () = modify () in
  let b = make 1000 in
  let () = assert_bool "Different states but equal arrays" (a <> b) in

  (* Cloning should work even if the RNG state is changing. *)
  let e = create () in
  let e_clone = BatEnum.clone e in
  let () = modify () in
    assert_equal_arrays
      (take_array 10 e)
      (take_array 10 e_clone)

(* Wrapper that assures that [cmd] does not modify the default state. *)
let with_saved_state cmd =
  let state = BatRandom.get_state () in
  let () = cmd () in
    BatRandom.set_state state

let test_enum_default () =
  let reset () = BatRandom.init 0 in
  let create () = BatRandom.enum_int 100 in
  let modify () = let _ = BatRandom.int 100 in () in
    with_saved_state
      (fun () -> test_enum_helper reset create modify)

let test_enum_state () =
  let make_seed () = BatRandom.State.make [| 0 |] in
  let state = ref (make_seed ()) in
  let reset () = state := make_seed () in
  let create () = BatRandom.State.enum_int !state 100 in
  let modify () = let _ = BatRandom.State.int !state 100 in () in
    test_enum_helper reset create modify

module PSE = BatRandom.Incubator.Private_state_enums

let test_enum_default_priv () =
  let reset () = BatRandom.init 0 in
  let create () = PSE.enum_int 100 in
  let modify () = let _ = BatRandom.int 100 in () in
  with_saved_state (fun () -> test_enum_helper reset create modify)

let test_enum_state_priv () =
  let make_seed () = BatRandom.State.make [| 0 |] in
  let state = ref (make_seed ()) in
  let reset () = state := make_seed () in
  let create () = PSE.State.enum_int !state 100 in
  let modify () = let _ = PSE.State.int !state 100 in () in
  test_enum_helper reset create modify


let tests = "BatRandom" >::: [
  "enum_default" >:: test_enum_default;
  "enum_state" >:: test_enum_state;
  "enum_default_priv" >:: test_enum_default_priv;
  "enum_state_priv" >:: test_enum_state_priv;
]
