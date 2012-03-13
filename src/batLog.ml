(*
 * BatLog - Simple Logging module
 * Copyright (C) 2011 The Batteries Included Team
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

open BatInnerIO

(** Flags enable features in logging *)
type flag = [
| `Date (** Print the current date as 2011/0628 *)
| `Time (** Print the current time as 01:23:45 *)
| `Filepos (** Print the file and position of this log command (UNIMPLEMENTED) *)
| `Custom of unit -> string (** Print a generated string *)
]

let output = ref stderr
let prefix = ref ""
let flags = ref [`Date; `Time]

(* TODO: make threadsafe? *)
let get_output () = !output
let set_output oc = output := oc

let get_prefix () = !prefix
let set_prefix p = prefix := p

let get_flags () = !flags
let set_flags fs = flags := fs

let print_flag ?fp t oc = function
  | `Date ->
    let {Unix.tm_year=y; tm_mon=m; tm_mday=d} = Lazy.force t in
    BatPrintf.fprintf oc "%4d/%02d%02d" (y + 1900) (m + 1) d
  | `Time ->
    let {Unix.tm_hour=h; tm_min=m; tm_sec=s} = Lazy.force t in
    BatPrintf.fprintf oc "%2d:%02d:%02d" h m s
  | `Filepos ->
    BatOption.may (nwrite oc) fp
  | `Custom gen ->
    nwrite oc (gen ())

let write_flags ?fp oc fs =
  if fs <> [] then
    let t = lazy (Unix.localtime (Unix.time ())) in
    BatList.print ~first:"" ~sep:" " ~last:":" (print_flag ?fp t) oc fs


(*  BatPrintf.fprintf !output "%a%s%s\n" (write_flags ?fp) !flags !prefix s *)
let print ?fp s =
  write_flags ?fp !output !flags;
  nwrite !output !prefix;
  nwrite !output s;
  write !output '\n'

(*  BatPrintf.fprintf !output ("%a%s" ^^ fmt ^^"\n") (write_flags ?fp) !flags !prefix *)
let printf ?fp fmt =
  write_flags ?fp !output !flags;
  nwrite !output !prefix;
  BatPrintf.fprintf !output fmt

(*  BatPrintf.kfprintf (fun _ -> exit 1) !output "%a%s%s\n" (write_flags ?fp) !flags !prefix s *)
let fatal ?fp s =
  write_flags ?fp !output !flags;
  nwrite !output !prefix;
  nwrite !output s;
  write !output '\n';
  exit 1

let fatalf ?fp fmt =
  BatPrintf.kfprintf (fun _ -> exit 1) !output ("%a%s" ^^ fmt ^^ "%!")
    (write_flags ?fp) !flags
    !prefix

module type S = sig
  val out: 'a output
  val prefix: string
  val flags: flag list
end
module Make (S:S) = struct

  let print ?fp s =
    write_flags ?fp S.out S.flags;
    nwrite S.out S.prefix;
    nwrite S.out s;
    write S.out '\n'

  let printf ?fp fmt =
    write_flags ?fp S.out S.flags;
    nwrite S.out S.prefix;
    BatPrintf.fprintf S.out (fmt ^^ "\n")

  let fatal ?fp s =
    write_flags ?fp S.out S.flags;
    nwrite S.out S.prefix;
    nwrite S.out s;
    write S.out '\n';
    exit 1

  let fatalf ?fp fmt =
    BatPrintf.kfprintf (fun _ -> exit 1) S.out ("%a%s" ^^ fmt ^^ "\n%!")
      (write_flags ?fp) S.flags S.prefix

end

let make_logger out prefix flags =
object
  method print ?fp s =
    write_flags ?fp out flags;
    nwrite out prefix;
    nwrite out s;
    write out '\n'
  method printf ?fp fmt =
    write_flags ?fp out flags;
    nwrite out prefix;
    BatPrintf.fprintf out (fmt ^^ "\n")
  method fatal ?fp s =
    write_flags ?fp out flags;
    nwrite out prefix;
    nwrite out s;
    write out '\n';
    exit 1
  method fatalf ?fp fmt =
    BatPrintf.kfprintf (fun _ -> exit 1) out ("%a%s" ^^ fmt ^^ "%!")
      (write_flags ?fp) flags prefix
end

(*$= make_logger & ~printer:identity
  "abcLog1\nabc34\n" \
  (let oc = IO.output_string () in    \
  let l = make_logger oc "abc" [] in \
  l#print "Log1"; l#printf "%d" 34;  \
  IO.close_out oc)
*)
