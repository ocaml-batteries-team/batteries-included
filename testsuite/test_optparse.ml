open OUnit
open BatPervasives
open BatOptParse

let printer = BatStd.dump

let tests = "OptParse tests" >::: [
  "parse no options" >:: begin function () ->
    let p = OptParser.make () in
    assert_equal ~printer ["foo"] (OptParser.parse p [|"foo"|])
  end;
]
