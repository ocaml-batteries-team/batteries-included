let (major, minor) =
  Scanf.sscanf Sys.ocaml_version
    "%d.%d." (fun j n -> (j, n))

let filter_cookie_re =
  Str.regexp "^##V\\([<>]?=?\\)\\([^#]+\\)##"
let version_re =
  Str.regexp "\\([0-9]+\\)\\(\\.\\([0-9]+\\)\\)?"

(* We track line count in the input source, to print location
   directives for the OCaml lexer:

# 123 foo.mlv

   lets the compiler know that it should consider the current location
   to be line 123 in file foo.mlv, which lets it report errors in the
   right place in the .mlv instead of some random place in a generated
   .ml.

   The [stale] reference is purely cosmetic: it would be correct to
   print a lexer directive at each line, but generate much less
   readable preprocessed outputs.
 *)
let mark_loc_stale = function
  | None -> ()
  | Some (file, count, stale) -> stale := true

let incr_loc = function
  | None -> ()
  | Some (file, count, stale) -> incr count

let print_loc = function
  | None -> ()
  | Some (file, count, stale) ->
     if !stale then begin
       Printf.printf "# %d %S\n" !count file;
       stale := false;
     end

let process_line loc line =
  if Str.string_match filter_cookie_re line 0 then begin
    mark_loc_stale loc;
    let cmp = match Str.matched_group 1 line with
    | "<" -> (<) | ">" -> (>) | "=" -> (=)
    | "<=" -> (<=) | ">=" -> (>=)
    | _ -> failwith "The ##V8## form is now disabled, use ##V>=8## instead"
    in
    let ver_string = Str.matched_group 2 line in
    assert (Str.string_match version_re ver_string 0) ;
    let ver_maj = int_of_string (Str.matched_group 1 ver_string) in
    let ver_min = try int_of_string (Str.matched_group 3 ver_string) with _ -> 0 in
    let pass = cmp (major*100+minor) (ver_maj*100+ver_min) in
    if pass
    then print_endline (Str.replace_first filter_cookie_re "" line)
  end else begin
    print_loc loc;
    print_endline line;
   end

let ( |> ) x f = f x

let process in_channel loc =
  try
    while true do
      input_line in_channel |> process_line loc;
      incr_loc loc;
    done
  with End_of_file -> ()

let from_stdin () = process stdin None

let from_file file =
  let in_channel = open_in file in
  let loc = Some (file, ref 1, ref true) in
  process in_channel loc;
  close_in in_channel

let () =
  if not !Sys.interactive then begin
    match Array.length Sys.argv with
      | 1 -> (* no param *)
         from_stdin ()
      | 2 -> (* one filename *)
         from_file Sys.argv.(1)
      | _ ->
         failwith "expected zero parameter (read from stdin) or one (filename)"
  end
