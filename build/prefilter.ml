let (major, minor) =
  Scanf.sscanf Sys.ocaml_version
    "%d.%d." (fun j n -> (j, n))

let filter_cookie_re =
  Str.regexp "^##V\\([<>]?=?\\)\\([^#]+\\)##"
let version_re =
  Str.regexp "\\([0-9]+\\)\\(\\.\\([0-9]+\\)\\)?"

let process_line line =
  if Str.string_match filter_cookie_re line 0 then begin
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
    if pass then Str.replace_first filter_cookie_re "" line
    else ""
  end else line

let ( |> ) x f = f x

let doit () =
  try
    while true do
      input_line stdin |> process_line |> print_endline
    done
  with End_of_file -> ()

let () =
  if not !Sys.interactive then doit ()
