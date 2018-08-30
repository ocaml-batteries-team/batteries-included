open OUnit
open BatOption

let fail_eq x y = failwith "should not be called"

let poly_eq = Pervasives.(=)

let tests = "Option tests" >::: [
  "None = None" >:: begin function () ->
    assert_bool "None should equal itself" (equal ~eq:fail_eq None None)
  end;
  "None <> Some x" >:: begin function () ->
    assert_bool "None and Some shouldn't amtch" (not (equal ~eq:fail_eq None (Some 4)))
  end;
  "Some x <> None" >:: begin function () ->
    assert_bool "Some and None shouldn't match" (not (equal ~eq:fail_eq (Some 4) None))
  end;
  "Some 4 = Some 4" >:: begin function () ->
    assert_bool "Some 4 should match itself" (equal ~eq:poly_eq (Some 4) (Some 4))
  end;
  "Some 4 = Some 5" >:: begin function () ->
    assert_bool "Some 4 shouldn't match Some 5" (not (equal ~eq:poly_eq (Some 4) (Some 5)))
  end;
]
