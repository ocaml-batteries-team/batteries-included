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

(** {1 Logging Library}

This module defines functions which implement a flexible error logging
system for applications.

@author Warren Harris, Metaweb Technologies
*)

(******************************************************************************)
(** {6 log modules} *)

type log
type level = NONE | FATAL | ERROR | WARN | NOTICE | INFO | DEBUG

val make_log : string -> log
    (** [make_log name] returns a new logger. *)

val log_name : log -> string
   (** [log_name logger] returns the name of the logger. *)

val log_enable : log -> level -> unit
   (** [log_enable logger level] enables a log level for a logger. *)

val log_level : log -> level
   (** [log_level logger] returns the currently enabled level for a logger. *)

val log_enabled : log -> level -> bool
   (** [log_enabled logger level] returns true if the specified level is
       currently enabled for the logger. *)

(******************************************************************************)
(** {6 log events} *)

type event = string * (string * string) list
   (** A log [event] consists of an event name and a list of key-value
       parameters (an association list). Events are constructed by
       [log] when a log level is enabled and passed to log formatters
       to render them to any logging output stream. *)

val log : log -> level -> (unit -> event) -> unit
   (** [log logger level event_fun] raises a log event if if the
       specified level is currently enabled for the logger. The
       function [event_fun ()] is called to return the event record,
       and is a function in order to delay construction or formatting
       of any event parameters in the case where the specified log
       level is not enabled. For example:

       {[log io_log INFO (fun () -> "connect", ["ADDR", addr])]}

       would only log the ["connect"] event (with the ["ADDR"] string
       parameter) when the [INFO] log level was enabled for the
       [io_log] logger. *)

val with_log : log -> level -> (unit -> event) -> ?result:('a -> string) ->
   (unit -> 'a) -> 'a
   (** [with_log logger level event_fun ?result body] logs an event
       before and after calling [body ()]. The function [event_fun ()]
       is called to return the event record to be logged. After the
       body is evaluated, the [result] function is used to convert the
       body's result value into a string, which is added to the event
       record as a ["RESULT"] parameter (if no [result] function is
       supplied then a ["-"] is used). In the case where the body
       raises an exception, an ["EXN"] parameter is instead added to
       the event containing the name of the exception. In addition, an
       indentation level is maintained throughout the duration of the
       body such that any other log statements occurring inside the
       body will see an incremented indentation level. This is added
       to the event key-value arguments as an additional ["I"]
       parameter. *)

(******************************************************************************)
(** {6 log formatters} *)

type formatter = log -> level -> event -> float -> unit
   (** the type of a log formatter is a function that takes the
       logger, the level of the log statement (which will be the
       currently enabled level or one of its successors), the event
       record, and a unix timestamp indicating the time the event was
       created. *)

val register_formatter : string -> formatter -> unit
   (** [register_formatter name formatter] registers a named log
       formatter. The name is only used for subsequent calls to
       identify the formatter via [unregister_formatter]. *)

val unregister_formatter : string -> unit
   (** [unregister_formatter name] unregisters a named log formatter. *)

val make_std_formatter : out_channel -> formatter
   (** [make_std_formatter oc] constructs a formatter from an output
       channel.  This formatter will format log events as
       tab-separated [<keyword>:<value>] pairs. The resulting
       formatter must be registered via [register_formatter] to be
       used when events are raised. This formatter also always outputs
       special parameters that describe the event timestamp (an
       ISO-8610 timestamp prefixed by ["D"]), the event name (the log
       module name followed by a dot, followed by the event name,
       prefixed by ["E"]), the log level (prefixed by ["L"]), the
       indentation level ( prefixed by ["I"]), followed by any other
       event parameters. For example, the log statement:

       {[log io_log INFO (fun () -> "connect", ["ADDR", addr])]}

       would produce formatted output like the following when the
       [io_log] [INFO] level was enabled:

       {[D:2009-01-26T00:47:45.033329Z   E:io.connect   L:INFO   I:1   ADDR:localhost:8080]} *)

val stderr_formatter : formatter
   (** [stderr_formatter] is a standard formatter that outputs log
       events to stderr using the same format as
       [make_std_formatter]. The resulting formatter must be
       registered via [register_formatter] or supplied to [init] or
       [init_from_string] to be used when events are raised. *)

val null_formatter : formatter
   (** [null_formatter] is a formatter that does not output any
       events, but simply discards them. *)

val make_dbg_formatter : out_channel -> formatter
   (** [make_dbg_formatter oc] constructs a debug formatter from an
       output channel. The debug formatter outputs simplified format
       that is easier to read for debugging purposes and displays
       indentation level. E.g.:

       {[
       with_log io_log DEBUG (fun () -> "listener" ["ADDR", addr])
         accept_connections (* calls other log statements *)
       ]}

       would produce formatted output like the following when the [io_log]
       [DEBUG] level was enabled:

       {[
       ### io.listener ADDR:localhost:8080 [DEBUG]
       ### | io.connected CLIENT_ADDR:192.168.0.23:28303 [DEBUG]
       ### | io.disconnected CLIENT_ADDR:192.168.0.23:28303 [DEBUG]
       ...
       ### io.listener ADDR:localhost:8080 RESULT:- [DEBUG]
       ]} *)

val dbg_formatter : formatter
   (** [dbg_formatter] is a debug formatter that outputs log events to
       stderr using the same format as [make_dbg_formatter]. The
       resulting formatter must be registered via [register_formatter]
       or supplied to [init] or [init_from_string] to be used when
       events are raised. *)

(******************************************************************************)
(** {6 logger initialization} *)

val init : (string * level) list -> formatter -> unit
    (** [init name_level_list formatter] initializes the logging
        system enabling the specified levels for each named
        logger. The formatter is the initial formatter for any log
        events that are output and is registered with the name
        "default" (other formatters may be registered by
        [register_formatter]). *)

val init_from_string : string -> formatter -> unit
   (** [init_from_string name_level_string formatter] initializes the
       logging system enabling the specified levels for each named
       logger. The string must be a comma separated list of [<logger
       name>:<level name>] pairs, e.g.  ["FOO:ERROR,BAR:WARN"]. If a
       un-prefixed level name is specified, then that becomes the
       default log level for all newly created logs, and all currently
       created logs are enabled to that level. If the logger does not
       yet exist, it is created. The formatter is the initial
       formatter for any log events that are output and is registered
       with the name "default" (other formatters may be registered by
       [register_formatter]). *)

(******************************************************************************)
(** {6 log utilities} *)

val level_of_name : string -> level
    (** [level_of_name str] returns the [level] associated with [str]. *)

val name_of_level : level -> string
   (** [name_of_level level] returns the name of the specified [level]. *)

val format_timestamp : out_channel -> float -> unit
   (** [format_timestamp oc timestamp] prints an ISO-8601 formatted
       timestamp (extended to specify higher-resolution seconds) to
       the output channel, [oc]. *)

(******************************************************************************)
