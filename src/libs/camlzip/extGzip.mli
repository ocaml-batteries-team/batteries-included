
(** Mimic the part of Camlzip's [Gzip] API about channels, but working
    on Batteries' input/output channels instead of [Pervasives]'s
    channels *)

open Batteries_core

type in_channel
val open_input: System.IO.input-> in_channel
val input_char: in_channel -> char
val input_byte: in_channel -> int
val input: in_channel -> string -> int -> int -> int
val really_input: in_channel -> string -> int -> int -> unit
val close_in: in_channel -> unit
val dispose: in_channel -> unit

type 'a out_channel
val open_output: ?level:int -> 'a System.IO.output -> 'a out_channel
val output_char: 'a out_channel -> char -> unit
val output_byte: 'a out_channel -> int -> unit
val output: 'a out_channel -> string -> int -> int -> int
val close_out: 'a out_channel -> 'a
val flush: 'a out_channel -> unit
