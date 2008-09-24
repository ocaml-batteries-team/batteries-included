open IO
open ListLabels

(*** Permissions *)
type permission = int
    (**Internally, permissions are represented in Unix-style
       octal.*)

let default_permission = 0o000
let user_read          = 0o400
let user_write         = 0o200
let user_exec          = 0o100
let group_read         = 0o040
let group_write        = 0o020
let group_exec         = 0o010
let other_read         = 0o004
let other_write        = 0o002
let other_exec         = 0o001

let perm l =
  fold_left l ~init:default_permission
    ~f:(fun acc x -> acc lor x)

let unix_perm i = 
  if 0<= i && i <= 511 then i
  else raise (Invalid_argument (Printf.sprintf "Unix permission %o " i))

(*** Opening *)
type open_in_flag =
  [ `create
  | `excl     (**Fail if the file exists and [`create] is set               *)
  | `text     (**Open in ascii mode -- if this flag is not specified or if the
		 operating system does not perform conversions, the file is
		 opened in binary mode.                                     *)
  | `nonblock (**Open in non-blocking mode                                  *)]

type open_out_flag =
  [ `append   (**Start writing at the end of the file rather than the start *)
  | `create   (**Create the file if it does not exist                       *)
  | `trunc    (**Empty the file if it already already exists                *)
  | `excl     (**Fail if the file exists and [`create] is set               *)
  | `text     (**Open in ascii mode -- if this flag is not specified or if the
		 operating system does not perform conversions, the file is
		 opened in binary mode.                                     *)
  | `nonblock (**Open in non-blocking mode                                  *) ]


let open_file_out ?mode ?(perm=default_permission) f =
  let mode_to_open_flag l =
    let rec aux acc is_binary = function
    | []           -> if is_binary then Open_binary::acc 
      else           Open_text  ::acc
    | `append::t   -> aux (Open_append::acc)   is_binary t
    | `trunc::t    -> aux (Open_trunc::acc)    is_binary t
    | `create::t   -> aux (Open_creat::acc)    is_binary t
    | `excl::t     -> aux (Open_excl::acc)     is_binary t
    | `text::t     -> aux acc false t
    | `nonblock::t -> aux (Open_nonblock::acc) is_binary t
    in aux [] true l
  in
  let chan_mode = match mode with
    | None   -> [Open_wronly; Open_binary]
    | Some l -> mode_to_open_flag l
  in
  output_channel (open_out_gen chan_mode perm f)

let open_file_in ?mode ?(perm=default_permission) f =
  let mode_to_open_flag l =
    let rec aux acc is_binary = function
    | []           -> if is_binary then Open_binary::acc 
      else           Open_text  ::acc
    | `create::t   -> aux (Open_creat::acc)    is_binary t
    | `excl::t     -> aux (Open_excl::acc)     is_binary t
    | `text::t     -> aux acc false t
    | `nonblock::t -> aux (Open_nonblock::acc) is_binary t
    in aux [] true l
  in
  let chan_mode = match mode with
    | None   -> [Open_rdonly; Open_binary]
    | Some l -> mode_to_open_flag l
  in
  input_channel (open_in_gen chan_mode perm f)

let with_do opener closer x f =
  let file = opener x in
    Std.finally (fun () -> closer file) f file
 
let with_file_in  ?mode ?perm  x = with_do (open_file_in  ?mode ?perm) close_in x
let with_file_out ?mode ?perm  x = with_do (open_file_out ?mode ?perm) close_out x

