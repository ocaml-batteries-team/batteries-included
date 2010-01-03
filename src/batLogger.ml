(* -*- Mode: Caml; indent-tabs-mode: nil -*- *)
(******************************************************************************)
(* Copyright (c) 2009, Metaweb Technologies, Inc.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *     * Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *     * Redistributions in binary form must reproduce the above
 *       copyright notice, this list of conditions and the following
 *       disclaimer in the documentation and/or other materials provided
 *       with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY METAWEB TECHNOLOGIES ``AS IS'' AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL METAWEB TECHNOLOGIES BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
 * BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
 * OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
 * IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 ******************************************************************************)

open Printf

type log = {
  name : string;
  mutable level : int;
}

type level = NONE | FATAL | ERROR | WARN | NOTICE | INFO | DEBUG
type event = string * (string * string) list
type formatter = log -> level -> event -> float -> unit

(******************************************************************************)
(** log utilities *)

let int_of_level = function
  | NONE -> 0 | FATAL -> 1 | ERROR -> 2 | WARN -> 3
  | NOTICE -> 4 | INFO -> 5 | DEBUG -> 6

let level_of_int = function
  | 0 -> NONE | 1 -> FATAL | 2 -> ERROR | 3 -> WARN
  | 4 -> NOTICE | 5 -> INFO | 6 -> DEBUG
  | i -> failwith ("invalid level: " ^ string_of_int i)

let name_of_level = function
  | NONE -> "NONE" | FATAL -> "FATAL" | ERROR -> "ERROR" | WARN -> "WARN"
  | NOTICE -> "NOTICE" | INFO -> "INFO" | DEBUG -> "DEBUG"

let level_of_name = function
  | "NONE" -> NONE | "FATAL" -> FATAL | "ERROR" -> ERROR | "WARN" -> WARN
  | "NOTICE" -> NOTICE | "INFO" -> INFO | "DEBUG" -> DEBUG
  | n -> failwith ("invalid level: " ^ n)

let format_timestamp out ts =
  let tm = Unix.gmtime ts in
  let ms, _ = modf ts in
    fprintf out "%04d-%02d-%02dT%02d:%02d:%02d.%06dZ"
      (1900 + tm.Unix.tm_year)
      (1 + tm.Unix.tm_mon)
      (tm.Unix.tm_mday)
      (tm.Unix.tm_hour)
      (tm.Unix.tm_min)
      (tm.Unix.tm_sec)
      (int_of_float (100000. *. ms))

(******************************************************************************)
(** log modules *)

let logs = Hashtbl.create 16

let default_level = ref (int_of_level INFO)

let make_log name =
  try Hashtbl.find logs name
  with Not_found ->
    let lm = { name = name; level = !default_level }
    in Hashtbl.replace logs name lm;
    lm

let log_enable lm lev =  lm.level <- int_of_level lev

let log_enabled lm lev =
  let lev_no = int_of_level lev in
    lev_no <= lm.level

let log_name lm = lm.name
let log_level lm = level_of_int lm.level

(******************************************************************************)
(** log formatters *)

let depth = ref 0
let formatters : (string * formatter) list ref = ref []

let register_formatter name f = formatters := (name, f) :: !formatters
let unregister_formatter name =
  formatters := List.remove_assoc name !formatters

let rec format_kvl oc = function
  | [] -> ()
  | (k, v)::rest ->
      fprintf oc "\t%s:%s" k v;
      format_kvl oc rest

let make_std_formatter oc lm lev (event_name, event_args) timestamp =
  fprintf oc "D:%a\tE:%s.%s\tL:%s%a\n%!"
    (*D:*) format_timestamp timestamp
    (*E:*) lm.name event_name
    (*L:*) (name_of_level lev)
    format_kvl event_args

let stderr_formatter = make_std_formatter stderr

let null_formatter lm lev event timestamp = ()

let format_indent oc depth =
  for i = 0 to depth do
    fprintf oc "| "
  done

let make_dbg_formatter oc lm lev (event_name, event_args) timestamp =
  let indent = try int_of_string (List.assoc "I" event_args) with _ -> 0 in
  let args = List.remove_assoc "I" event_args in
  fprintf oc "### %a%s.%s %a [%s]\n%!" format_indent indent
    (log_name lm) event_name
    format_kvl args (name_of_level lev)

let dbg_formatter lm lev ep ts = make_dbg_formatter stderr lm lev ep ts

(******************************************************************************)
(** log events *)

let log lm lev event_fun =
  if log_enabled lm lev then
    let time = Unix.gettimeofday () in
    let event_name, event_args = event_fun () in
    let event = event_name, ("I", string_of_int !depth) :: event_args in
    List.iter (fun (name, fmt) -> fmt lm lev event time) !formatters

let with_log lm lev event_fun ?result body =
  if log_enabled lm lev then begin
    try
      log lm lev event_fun;
      incr depth;
      let rv = body () in
      decr depth;
      log lm lev (fun () ->
        let event_name, event_args = event_fun () in
        let result_str = match result with
        | Some f -> f rv
        | None -> "-"
        in
        event_name, ("RESULT", result_str) ::event_args);
      rv
    with exn ->
      decr depth;
      log lm lev (fun () ->
        let event_name, event_args = event_fun () in
        event_name, ("EXN", Printexc.to_string exn) :: event_args);
      raise exn
  end else body ()

(******************************************************************************)
(** logger initialization *)

let init name_level_list formatter =
  List.iter (fun (name, level) -> let lm = make_log name in log_enable lm level)
    name_level_list;
  register_formatter "default" formatter

let init_from_string name_level_string formatter =
  let init_key_value ss =
    try
      let name_ss, level_ss = BatSubstring.splitl (fun c -> c <> ':') ss in
      let name = BatSubstring.to_string name_ss in
      let level = level_of_name (BatSubstring.to_string level_ss) in
      let lm = make_log name in
      log_enable lm level
    with Not_found -> try
      let level = level_of_name (BatSubstring.to_string ss) in
      default_level := int_of_level level;
      Hashtbl.iter (fun name lm -> log_enable lm level) logs
    with Failure _ ->
      failwith ("invalid log initialization: " ^ BatSubstring.to_string ss)
  in
  List.iter init_key_value (BatSubstring.split_on_comma (BatSubstring.of_string name_level_string) );
  register_formatter "default" formatter

(******************************************************************************)

let test () =
  let lm = make_log "test" in
  let direct () =
    log lm NOTICE (fun () -> "hello", []);
    log lm DEBUG (fun () -> "debug msg1", []);
    log lm ERROR (fun () -> "error msg1", []);
    log lm ERROR (fun () -> "ok", ["ARG1", string_of_int 234]);
  in
  let rec run () =
    direct ();
    Unix.sleep 3;
    run ()
  in run ()

(******************************************************************************)
