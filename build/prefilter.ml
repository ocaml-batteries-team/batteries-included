let (major, minor) =
  Scanf.sscanf Sys.ocaml_version
    "%d.%d." (fun j n -> (j, n))

let filter_cookie_re =
  Str.regexp "^##V\\([^#]+\\)##"
let version_re =
  Str.regexp "\\([0-9]+\\)\\(\\.\\([0-9]+\\)\\)?"

let maybe f x = try Some (f x) with _ -> None

let process_line line =
  if Str.string_match filter_cookie_re line 0 then begin
    let ver_string = Str.matched_group 1 line in
    assert (Str.string_match version_re ver_string 0) ;
    let ver_maj = int_of_string (Str.matched_group 1 ver_string) in
    let pass = match maybe (Str.matched_group 3) ver_string with
    | None -> ver_maj <= major
    | Some ver_min ->
      let ver_min = int_of_string ver_min in
      ver_maj <= major && ver_min <= minor
    in
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
