(**
*)

type ('a, 'b) t = ('a, 'b) Std.result

val catch: ('a -> 'b) -> 'a -> ('b, exn) Std.result
  (** Execute a function and catch any exception as a [!Std.result]*)

