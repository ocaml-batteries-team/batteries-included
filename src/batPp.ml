(* batteries super simple pre processor.
   Cannot depend on anything else than the stdlib.

man ocamlc:
  [...]
  -pp <command>:
    Cause the compiler to call the given command as a preprocessor
    for  each source file. The output of command is redirected to an
    intermediate file, which is compiled. If there are  no  compila‐
    tion  errors,  the  intermediate file is deleted afterwards. The
    name of this file is built from the basename of the source  file
    with  the  extension  .ppi for an interface (.mli) file and .ppo
    for an implementation (.ml) file.
   [...]

It must support the following line directives which are used by batteries:
^##V(>|>=|=|<|<=)%d[.%d[.%d]]## ... rest of the line ... $
*)

open Printf

module A = Array
module S = String

type version = int * int * int
type operation = Veq of version (* ^##V=...  *)
               | Vlt of version (* ^##V<...  *)
               | Vle of version (* ^##V<=... *)
               | Vgt of version (* ^##V>...  *)
               | Vge of version (* ^##V>=... *)

let discard_additional_info version_str =
  try
    let i = S.index version_str '+' in
    S.sub version_str 0 i
  with Not_found -> version_str
(* let () = assert(discard_additional_info "4.11.2+trunk+fp" = "4.11.2") *)

let parse_version_string s' =
  let s = discard_additional_info s' in
  try Scanf.sscanf s "%d.%d.%d" (fun major minor bugfix ->
      (major, minor, bugfix))
  with _ ->
  try Scanf.sscanf s "%d.%d" (fun major minor -> (major, minor, 0))
  with _ ->
  try Scanf.sscanf s "%d" (fun major -> (major, 0, 0))
  with exn -> (eprintf "BatPp.parse_version_string: cannot parse '%s'" s;
               raise exn)

let op_of_string s =
  let n = S.length s in
  match s.[0], s.[1] with
  | '<', '=' -> Vle (parse_version_string (S.sub s 2 (n - 2)))
  | '>', '=' -> Vge (parse_version_string (S.sub s 2 (n - 2)))
  | '<', _   -> Vlt (parse_version_string (S.sub s 1 (n - 1)))
  | '=', _   -> Veq (parse_version_string (S.sub s 1 (n - 1)))
  | '>', _   -> Vgt (parse_version_string (S.sub s 1 (n - 1)))
  | other -> failwith ("BatPp.op_of_string: cannot parse: " ^ s)

let string_of_version_striplet = function
  | (x, 0, 0) -> sprintf "%d" x
  | (x, y, 0) -> sprintf "%d.%d" x y
  | (x, y, z) -> sprintf "%d.%d.%d" x y z

(* for debug *)
let string_of_op = function
  | Vle xyz -> sprintf "##V<=%s##" (string_of_version_striplet xyz)
  | Vge xyz -> sprintf "##V>=%s##" (string_of_version_striplet xyz)
  | Vlt xyz -> sprintf "##V<%s##"  (string_of_version_striplet xyz)
  | Veq xyz -> sprintf "##V=%s##"  (string_of_version_striplet xyz)
  | Vgt xyz -> sprintf "##V>%s##"  (string_of_version_striplet xyz)

(* should we output the current line? *)
let process_op curr_ver pp_dir = match pp_dir with
  | Vle required -> (curr_ver <= required)
  | Vge required -> (curr_ver >= required)
  | Vlt required -> (curr_ver <  required)
  | Veq required -> (curr_ver =  required)
  | Vgt required -> (curr_ver >  required)

let min_pp_dir_len = S.length "##V=1##"
let current_version = parse_version_string Sys.ocaml_version

let process_line debug l =
  let n = S.length l in
  if n > min_pp_dir_len then
    try Scanf.sscanf l "##V%s@##%s@!" (fun op_str code_str ->
        let pp_dir = op_of_string op_str in
        let prfx = if debug then string_of_op pp_dir else "" in
        if process_op current_version pp_dir then
          printf "%s%s\n" prfx code_str
        else (if debug then printf "%s\n" prfx)
      )
    with _ -> printf "%s\n" l (* no pp directive: output line as is *)
  else
    printf "%s\n" l

let with_in_file fn f =
  let input = open_in_bin fn in
  let res = f input in
  close_in input;
  res

let main () =
  let major, minor, bugfix = current_version in
  let input_fn = Sys.argv.(1) in
  let debug = A.mem "-v" Sys.argv in
  if debug then eprintf "BatPp: OCaml version %d.%d.%d\n" major minor bugfix;
  with_in_file input_fn (fun input ->
      try
        while true do
          process_line debug (input_line input)
        done
      with End_of_file -> ()
    )

let () = main ()
