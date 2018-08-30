open OUnit
open BatPervasives
open BatOption

let fail_eq x y = failwith "should not be called"

let poly_eq = Pervasives.(=)

let tests = "Option tests" >::: [
  "None = None" >:: begin function () ->
    assert (equal ~eq:fail_eq None None)
  end;
  "None <> Some x" >:: begin function () ->
    assert (not (equal ~eq:fail_eq None (Some 4)))
  end;
  "Some x <> None" >:: begin function () ->
    assert (not (equal ~eq:fail_eq (Some 4) None))
  end;
  "Some 4 = Some 4" >:: begin function () ->
    assert (equal ~eq:poly_eq (Some 4) (Some 4))
  end;
  "Some 4 = Some 5" >:: begin function () ->
    assert (not (equal ~eq:poly_eq (Some 4) (Some 5)))
  end;
]
