(**
   Call your favorite browser to browse each of the URLs entered on the command-line.
*)
open Batteries_config

iter (fun x -> ignore **> browse x) (args ())
