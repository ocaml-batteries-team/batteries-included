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

open BatIO

type flag = [
| `Date (* Print the current date as 2011/0628 *)
| `Time (* Print the current time as 01:23:45 *)
| `Filepos (* Print the file and position of this log command (UNIMPLEMENTED) *)
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

let print_flag t oc = function
  | `Date -> 
    let {Unix.tm_year=y; tm_mon=m; tm_mday=d} = Lazy.force t in 
    BatPrintf.fprintf oc "%4d/%2d%2d" (y + 1900) (m + 1) d
  | `Time -> 
    let {Unix.tm_hour=h; tm_min=m; tm_sec=s} = Lazy.force t in
    BatPrintf.fprintf oc "%2d:%2d:%2d" h m s
  | `Filepos ->
    failwith "Filepos unimplemented"

let write_flags oc fs = 
  let t = lazy (Unix.localtime (Unix.time ())) in
  BatList.print ~first:"" ~sep:" " ~last:":" (print_flag t) oc fs

let print s = 
  write_flags !output !flags; 
  nwrite !output !prefix;
  nwrite !output s;
  write !output '\n'

let printf fmt = 
  write_flags !output !flags; 
  nwrite !output !prefix;
  BatPrintf.fprintf !output fmt

let fatal s =
  write_flags !output !flags; 
  nwrite !output !prefix;
  nwrite !output s;
  write !output '\n';
  exit 1

let fatalf _ = assert false
(** [fatalf] allows a format string (as [Printf.printf])and the
    arguments to that format string to build the logging message.
    Exits the program with return code 1. *)


module type S = sig
  val out: 'a output
  val prefix: string
  val flags: flag list
end
module Make (S:S) = struct

  let print s = 
    write_flags S.out S.flags; 
    nwrite S.out S.prefix;
    nwrite S.out s;
    write S.out '\n'

  let printf fmt = 
    write_flags S.out S.flags; 
    nwrite S.out S.prefix;
    BatPrintf.fprintf S.out fmt

  let fatal s =
    write_flags S.out S.flags; 
    nwrite S.out S.prefix;
    nwrite S.out s;
    write S.out '\n';
    exit 1

  let fatalf _ = assert false

end

type 'a logger = {
  print : string -> unit;
  printf : 'b. ('b, 'a output, unit) Pervasives.format -> 'b;
  fatal: string -> 'a;
  fatalf: 'b. ('b, 'a output, unit) Pervasives.format -> 'b;
}

let make_logger out prefix flags =
  let print s = 
    write_flags out flags; 
    nwrite out prefix;
    nwrite out s;
    write out '\n'
  in
  let printf fmt = 
    write_flags out flags; 
    nwrite out prefix;
    BatPrintf.fprintf out fmt
  in
  let fatal s =
    write_flags out flags; 
    nwrite out prefix;
    nwrite out s;
    write out '\n';
    exit 1
  in
  { print = print; printf=printf; fatal=fatal; fatalf=(fun _ -> assert false) }

