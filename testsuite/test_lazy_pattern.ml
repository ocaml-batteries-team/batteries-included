open OUnit
open Batteries_uni
module L = LazyList

let tests = "LazyPattern" >::: [
  "match nil" >:: begin fun () ->
    assert_bool "nil didn't match"
      (match L.nil with [%] -> true | _ -> false)
  end;
  "nil doesn't match non-nil" >:: begin fun () ->
    assert_bool "nil matched incorrectly"
      (match L.cons 3 L.nil with [%] -> false | _ -> true)
  end;
  "match cons" >:: begin fun () ->
    assert_equal ~printer:string_of_int 5
      (match L.cons 5 L.nil with x %:: _ -> x | _ -> 0)
  end;
  "form nil" >:: begin fun () ->
    assert_equal L.Nil (Lazy.force [%])
  end;
];;
