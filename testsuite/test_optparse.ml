open OUnit
open BatPervasives
open BatOptParse

let printer = dump

let tests = "OptParse tests" >::: [
  "parse empty" >:: begin function () ->
    let p = OptParser.make () in
    assert_equal ~printer [] (OptParser.parse p [||])
  end;
  "parse no options" >:: begin function () ->
    let p = OptParser.make () in
    assert_equal ~printer ["foo"] (OptParser.parse p [|"foo"|])
  end;
  "parse empty (only leading)" >:: begin function () ->
    let p = OptParser.make ~only_leading_opts:true () in
    assert_equal ~printer [] (OptParser.parse p [||])
  end;
  "parse no options (only leading)" >:: begin function () ->
    let p = OptParser.make ~only_leading_opts:true () in
    assert_equal ~printer ["foo"] (OptParser.parse p [|"foo"|])
  end;
]
