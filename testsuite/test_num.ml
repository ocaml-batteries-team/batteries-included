open OUnit
open BatNum

let tests = "Num" >::: [
  "of_float" >::: [
    "zero" >:: begin function () ->
      assert_equal ~cmp:(=) ~printer:to_string zero (of_float 0.)
    end;
    "numbers" >:: begin function () ->
      Array.iter begin function f ->
          assert_equal ~printer:BatFloat.to_string f
          (to_float (of_float f))
        end
        [|2.5; 1.0; 0.5; -0.5; -1.0; -2.5|]
    end;
    "infinity/nan" >:::
    let (save_set, restore) =
      let saved_state = Stack.create () in
      begin fun state () ->
        Stack.push
          (Arith_status.get_error_when_null_denominator ())
          saved_state;
        Arith_status.set_error_when_null_denominator state;
      end,
      begin fun () ->
        Arith_status.set_error_when_null_denominator
        (Stack.pop saved_state)
      end
    in
    let test () =
      Array.iter
        begin fun (f, (n,d)) ->
          if Arith_status.get_error_when_null_denominator ()
          then
            assert_raises
              (Failure "create_ratio infinite or undefined rational number")
              (fun () -> ignore (of_float f))
          else
            assert_equal ~cmp:equal ~printer:to_string
              (div n d)
              (of_float f)
        end
        [| infinity, (one,zero); neg_infinity, (neg one,zero); nan, (zero,zero) |]
    in
    [
      "allow_null_denom" >:: bracket (save_set false) test restore;
      "forbid_null_denom" >:: bracket (save_set true) test restore;
    ]
  ]
]
