(** Implementation of a cat-like tool: read each file whose name is
    given on the command-line and print the contents to stdout.

    Usage: 
     ./cat2.byte *.ml
     
    Variants based on function composition
*)

(*
For reference

write_lines : unit output -> string Enum.t -> unit
stdout      : unit output
args        : unit -> string Enum.t
()          : unit
concat      : string Enum.t Enum.t -> string Enum.t
map         : (string -> string Enum.t) -> string Enum.t -> string Enum.t Enum.t
File.lines_of:string -> string Enum.t
*)

(*(*Variant 1*)
let _ =
  write_lines stdout -| concat <| map lines_of (args ())
*)

(*Variant 2*)
let _ =
  () |> args |> (File.lines_of |> map) |> concat |> (stdout |> IO.write_lines)

