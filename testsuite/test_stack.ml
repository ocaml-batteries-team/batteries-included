open OUnit
module Enum = BatEnum
module Stack = struct
  include Stack
  include BatStack
end
module List = struct
  include List
  include BatList
end

let tests = "Stack" >::: [
  "of_enum empty" >:: begin function () ->
    let e = Enum.empty () in
    let s = Stack.of_enum e in
    assert_bool "stack is not empty" (Stack.is_empty s);
    assert_equal ~printer:string_of_int 0 (Stack.length s);
  end;
  "of_enum simple" >:: begin function () ->
    let e = List.enum [1;2;3] in
    let s = Stack.of_enum e in
    assert_bool "stack is empty" (not (Stack.is_empty s));
    assert_equal ~printer:string_of_int 3 (Stack.length s);
    assert_equal ~printer:string_of_int 3 (Stack.pop s);
    assert_equal ~printer:string_of_int 2 (Stack.pop s);
    assert_equal ~printer:string_of_int 1 (Stack.pop s);
    assert_raises Stack.Empty (fun () -> Stack.pop s);
  end;
  "enum empty" >:: begin function () ->
    let e = Stack.enum (Stack.create ()) in
    assert_bool "enum is not empty" (Enum.is_empty e);
  end;
  "enum nonempty" >:: begin function () ->
    let s = Stack.create () in
    Stack.push 5 s;
    Stack.push 7 s;
    assert_equal [7;5] (List.of_enum (Stack.enum s));
  end
]
