(** Testing syntax extension [open...in].
    We're compiling with [use_batteries], so there's no need to [open Batteries]*)

print_endline "starting";

open Languages.Printf in
  printf "%s\n" "Open...in works";
