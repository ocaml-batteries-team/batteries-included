(*
 * File - File manipulation
 * Copyright (C) 2008 David Teller
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version,
 * with the special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

open BatIO
open ListLabels
open Unix

let finally = BatInnerPervasives.finally

(* Permissions *)
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

(* Opening *)
type open_in_flag =
  [ `create
  | `excl     (**Fail if the file exists and [`create] is set               *)
  | `text     (**Open in ascii mode -- if this flag is not specified or if the
				 operating system does not perform conversions, the file is
				 opened in binary mode.                                     *)
  | `nonblock (**Open in non-blocking mode                                  *)
  | `mmap     (**Open in memory-mapped mode (experimental)*)                 ]

type open_out_flag =
  [ `append   (**Start writing at the end of the file rather than the start *)
  | `create   (**Create the file if it does not exist                       *)
  | `trunc    (**Empty the file if it already exists (on by default)        *)
  | `excl     (**Fail if the file exists and [`create] is set               *)
  | `text     (**Open in ascii mode -- if this flag is not specified or if the
				 operating system does not perform conversions, the file is
				 opened in binary mode.                                     *)
  | `nonblock (**Open in non-blocking mode                                  *) ]


(**
   Convert a [open_in_flag list] into a low-level [open_flag list]
*)
let in_chan_mode ?mode binary =
  let mode_to_open_flag  l =
    let rec aux acc is_binary = function
      | []           -> if is_binary then Open_binary::acc
        else           Open_text  ::acc
      | `create::t   -> aux (Open_creat::acc)    is_binary t
      | `excl::t     -> aux (Open_excl::acc)     is_binary t
      | `text::t     -> aux acc false t
      | `nonblock::t -> aux (Open_nonblock::acc) is_binary t
      | _::t         -> aux acc is_binary t (*Allow for future extensions*)
    in aux [] binary l
  in match mode with
  | None   -> [Open_rdonly; Open_binary]
  | Some l -> mode_to_open_flag l


(**
   Convert a [open_out_flag list] into a low-level [open_flag list]
*)
let out_chan_mode ?mode binary =
  let mode_to_open_flag l =
    let rec aux acc is_binary = function
      | []           -> let acc' = if List.mem Open_append acc then acc
        else                             Open_trunc::acc in
        if is_binary then Open_binary::acc'
        else              Open_text  ::acc'
      | `append::t   -> aux (Open_append::acc)   is_binary t
      | `trunc::t    -> aux (Open_trunc::acc)    is_binary t
      | `create::t   -> aux (Open_creat::acc)    is_binary t
      | `excl::t     -> aux (Open_excl::acc)     is_binary t
      | `text::t     -> aux acc false t
      | `nonblock::t -> aux (Open_nonblock::acc) is_binary t
      | _::t         -> aux acc is_binary t (*Allow for future extensions*)
    in aux [] binary l
  in match mode with
  | None   -> [Open_wronly; Open_binary; Open_creat; Open_trunc]
  | Some l -> Open_wronly :: (mode_to_open_flag l)


let open_out ?mode ?(perm=0o666) name =
  (*  Printf.eprintf "Opening out\n%!";*)
  output_channel ~cleanup:true (open_out_gen (out_chan_mode ?mode true) perm name)

open BatBigarray

let open_in ?mode ?(perm=default_permission) name =
  let unix_mode = in_chan_mode ?mode true in
  match mode with
  | Some l when List.mem `mmap l ->
    let desc = Unix.openfile name [O_RDONLY] 0                      in
    let array= Array1.map_file desc char c_layout (*shared*)false (-1) in
    let pos  = ref 0
    and len  = Array1.dim array                                     in
    create_in
      ~read:(fun () ->
        if !pos >= len then raise No_more_input
        else Array1.get array (BatRef.post_incr pos))
      ~input:(fun sout p l ->
        if !pos >= len then raise No_more_input;
        let n = (if !pos + l > len then len - !pos else l) in
        for i = 0 to n - 1 do
          String.(*unsafe_*)set sout (!pos + i) (Array1.get array i)
        done;
        (*		    String.unsafe_blit s (post pos ( (+) n ) ) sout p n;*)
        pos := !pos + n;
        n
      )
      ~close:(fun () -> Unix.close desc)
  | _ ->
    input_channel ~cleanup:true ~autoclose:false (open_in_gen unix_mode perm name)


let with_do opener closer x f =
  let file = opener x in
  finally (fun () -> closer file) f file

let with_file_in  ?mode ?perm  x = with_do (open_in  ?mode ?perm) close_in x
let with_file_out ?mode ?perm  x = with_do (open_out ?mode ?perm) close_out x

let lines_of file = BatIO.lines_of (open_in file)

let write_lines file lines =
  let mode = [`trunc; `create] in
  with_file_out ~mode file (fun oc -> BatEnum.iter (BatIO.write_line oc) lines)

(**
   {6 Temporary files}
*)

type open_temporary_out_flag =
  [ open_out_flag
  | `delete_on_exit (**Should the file be deleted when program ends?*) ]

let open_temporary_out ?mode ?(prefix="ocaml") ?(suffix="tmp") ?temp_dir () : (_ output * string) =
  let chan_mode = out_chan_mode ?mode true in
  let (name, cout) = Filename.open_temp_file ?temp_dir ~mode:chan_mode prefix suffix in
  let out          = output_channel ~cleanup:true cout   in
  (match mode with
   | Some l when List.mem `delete_on_exit l ->
     Pervasives.at_exit (fun () ->
       try
         BatIO.close_out out;
         Sys.remove name
       with
         _ -> ())
   | _ -> ());
  (out, name)

let with_temporary_out ?mode ?prefix ?suffix ?temp_dir f =
  let (file, name) = open_temporary_out ?mode ?prefix ?suffix ?temp_dir () in
  finally (fun () -> close_out file)
    (fun (file, name) -> f file name)
    (file, name)


(**
   {6 File manipulation}
*)

open Unix
let size_of s = (stat s).st_size
let size_of_big s = (LargeFile.stat s).LargeFile.st_size


let chmod = Unix.chmod
let set_permissions = chmod
