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

let print_flag ?fp t oc = function
  | `Date ->
    let {Unix.tm_year=y; tm_mon=m; tm_mday=d} = Lazy.force t in
    BatPrintf.fprintf oc "%4d/%02d/%02d" (y + 1900) (m + 1) d
  | `Time ->
    let {Unix.tm_hour=h; tm_min=m; tm_sec=s} = Lazy.force t in
    BatPrintf.fprintf oc "%2d:%02d:%02d" h m s
  | `Filepos ->
    BatOption.may (nwrite oc) fp
  | `Custom gen ->
    nwrite oc (gen ())

let write_flags ?fp oc fs =
  if fs <> [] then
    (* is it better to call time in print_flag? *)
    let t = lazy (Unix.localtime (Unix.time ())) in
    BatList.print ~first:"" ~sep:" " ~last:":" (print_flag ?fp t) oc fs


(*  BatPrintf.fprintf !output "%a%s%s\n" (write_flags ?fp) !flags !prefix s *)
let log ?fp s =
  let oc = !output in (* makes sure all output goes to a single channel when multi-threaded *)
  write_flags ?fp oc !flags;
  nwrite oc !prefix;
  nwrite oc s;
  write oc '\n'

(*  BatPrintf.fprintf !output ("%a%s" ^^ fmt ^^"\n") (write_flags ?fp) !flags !prefix *)
let logf ?fp fmt =
  let oc = !output in
  write_flags ?fp oc !flags;
  nwrite oc !prefix;
  BatPrintf.fprintf oc fmt

(*  BatPrintf.kfprintf (fun _ -> exit 1) !output "%a%s%s\n" (write_flags ?fp) !flags !prefix s *)
let fatal ?fp s =
  let oc = !output in
  write_flags ?fp oc !flags;
  nwrite oc !prefix;
  nwrite oc s;
  write oc '\n';
  exit 1

let fatalf ?fp fmt =
  BatPrintf.kfprintf (fun _ -> exit 1) !output ("%a%s" ^^ fmt ^^ "%!")
    (write_flags ?fp) !flags !prefix

module type Config = sig
  type t
  val out: t output
  val prefix: string
  val flags: flag list
end

module Make (S:Config) = struct
  let log ?fp s =
    write_flags ?fp S.out S.flags;
    nwrite S.out S.prefix;
    nwrite S.out s;
    write S.out '\n'

  let logf ?fp fmt =
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
    method log ?fp s =
      write_flags ?fp out flags;
      nwrite out prefix;
      nwrite out s;
      write out '\n'
    method logf ?fp fmt =
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
  l#log "Log1"; l#logf "%d" 34;  \
  IO.close_out oc)
*)

module type Level_sig = sig
  type t
  val to_string : t -> string
  val default_level : t
  val compare : t -> t -> int
end

module Make_lev(L : Level_sig)(S: Config) = struct
  (* These are threadsafe to get/set, so no setter/getter needed;
     publicly accessible *)
  let level = ref L.default_level
  let output = ref S.out

  (** Main logging function *)
  let log ?fp l m =
    if L.compare l !level >= 0 then
      let oc = !output in
      write_flags ?fp oc S.flags;
      nwrite oc S.prefix;
      nwrite oc (L.to_string l);
      nwrite oc ": ";
      nwrite oc m;
      write oc '\n'

  let logf ?fp l fmt = (* printf-style logging *)
    if L.compare l !level >= 0 then
      let oc = !output in
      write_flags ?fp oc S.flags;
      nwrite oc S.prefix;
      nwrite oc (L.to_string l);
      nwrite oc ": ";
      BatPrintf.fprintf oc (fmt ^^ "\n")
    else
      Printf.ifprintf !output fmt
end

type easy_lev = [ `trace | `debug | `info | `warn | `error | `fatal | `always ]
module Basic = struct
  type t = easy_lev

  let to_string : (t -> string) = function
    | `trace -> "TRACE" | `debug -> "DEBUG" | `info -> "INFO"
    | `warn -> "WARN" | `error -> "ERROR" | `fatal -> "FATAL"
    | `always -> "ALWAYS"

  let to_int : (t -> int) = function
    | `trace -> 0 | `debug -> 1 | `info -> 2 | `warn -> 3
    | `error -> 4 | `fatal -> 5 | `always -> 6

  let default_level = `always

  let compare a b =
    BatInt.compare (to_int a) (to_int b)
end

module Default_config = struct
  type t = unit
  let out    = stderr
  let prefix = ""
  let flags  = [`Date; `Time]
end

module Easy = Make_lev(Basic)(Default_config)
