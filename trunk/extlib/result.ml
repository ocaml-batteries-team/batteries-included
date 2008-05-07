type ('a, 'b) t = ('a, 'b) Std.result

let catch f x =
  try  Std.Ok (f x)
  with e -> Std.Error e
