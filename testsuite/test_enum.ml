open OUnit

let array     = [|'1';'2';'3';'4';'5'|]
let array2    = [|[|'1';'2';'3';'4';'5'|];
                  [|'6';'7';'8';'9';'A'|];
                  [|'B';'C';'D';'E';'F'|]|]
let array3    = [|[|[|'1';'2';'3';'4';'5'|];
                    [|'6';'7';'8';'9';'A'|];
                    [|'B';'C';'D';'E';'F'|]|];
                  [|[|'G';'H';'I';'J';'K'|];
                    [|'L';'M';'N';'O';'P'|];
                    [|'Q';'R';'S';'T';'U'|]|]|]
let list      = ['1';'2';'3';'4';'5']
let string    = "12345"
let bigarray1 = Big_array.Array1.of_array Big_array.char Big_array.c_layout array
let bigarray2 = Big_array.Array2.of_array Big_array.char Big_array.c_layout array2
let bigarray3 = Big_array.Array3.of_array Big_array.char Big_array.c_layout array3

let utf8   = UTF8.of_string string
let rope   = Rope.of_ustring utf8

let test_array_enums () =
  open Array in begin
    let source = array in
    let aeq = assert_equal ~printer:(Printf.sprintf2 "%a" (print Char.print)) in
      aeq (of_backwards (enum source)) (of_enum (backwards source));
      aeq source (of_backwards (backwards source));
  end

let test_list_enums () =
  open List in begin
    let source = list in
    let aeq = assert_equal ~printer:(Printf.sprintf2 "%a" (print Char.print)) in
      aeq (of_backwards (enum source)) (of_enum (backwards source));
      aeq source (of_backwards (backwards source));
  end

let test_string_enums () =
  open String in begin
    let source = string in
    let aeq = assert_equal ~printer:(Printf.sprintf "%S") in
      aeq (of_backwards (enum source)) (of_enum (backwards source));
      aeq source (of_backwards (backwards source));
  end

let test_rope_enums () =
  open Rope in begin
    let source = rope in
    let aeq = assert_equal ~printer:(Printf.sprintf2 "%a" print) in
      aeq (of_backwards (enum source)) (of_enum (backwards source));
      aeq source (of_backwards (backwards source));
  end

let test_UTF8_enums () =
  open UTF8 in begin
    let source = utf8 in
    let aeq = assert_equal ~printer:(Printf.sprintf2 "%a" print) in
      aeq (of_backwards (enum source)) (of_enum (backwards source));
      aeq source (of_backwards (backwards source));
  end

let test_bigarray_enums () =
  open Array in begin
    let aeq = assert_equal ~printer:(Printf.sprintf2 "%a" (print Char.print)) in
    let enum_flatten x = Enum.flatten (Enum.map enum x) in
      aeq (of_enum (enum array)) (of_enum (Big_array.Array1.enum bigarray1));
      aeq
        (enum array2 |> enum_flatten |> of_enum)
        (of_enum (Big_array.Array2.enum bigarray2));
      aeq
        (enum array3 |> enum_flatten |> enum_flatten |> of_enum)
        (of_enum (Big_array.Array3.enum bigarray3))
  end

let tests = "Enum" >::: [
  "Array" >:: test_array_enums;
  "List" >:: test_list_enums;
  "String" >:: test_string_enums;
  "Rope" >:: test_rope_enums;
  "UTF8" >:: test_UTF8_enums;
  "bigarray" >:: test_bigarray_enums;
]
