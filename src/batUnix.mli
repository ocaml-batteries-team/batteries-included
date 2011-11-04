(*
 * BatUnix - additional and modified functions for Unix and Unix-compatible systems.
 * Copyright (C) 1996 Xavier Leroy
 * Copyright (C) 2008 David Teller, LIFO, Universite d'Orleans
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

open BatConcurrent

(** Low-level interface to the operating system (both Unix and Windows).

    This module only provides low-level functions and types. Unless you
    know that you need low-level access to the operating system, you
    probably don't. For higher-level functions, see modules {!BatShell},
    {!BatIO}, {!BatFile}.

    {b Note} This module is thread-safe.

    @author Xavier Leroy (Base module)
    @author David Teller

    @documents Unix
*)

  open BatInnerIO
  open Unix

(** {6 Interfacing with the standard input/output library} *)

val input_of_descr: ?autoclose:bool -> ?cleanup:bool -> file_descr -> BatInnerIO.input
(** Create an {!type:input} reading from the given descriptor.
    The {!type: input} is initially in binary mode; use
    [set_binary_mode_in ic false] if text mode is desired. 

    @param autoclose If true (default value), close the input
    automatically once there is no more content to read. Otherwise,
    the input will be closed according to the usual rules of module
    {!BatIO}. Barring very specific needs (e.g. using file descriptors as
    locks), you probably want [autoclose] to be [true].

    @param cleanup If true, close the underlying file descriptor
    when the {!type:input} is closed. If false or unspecified,
    do nothing, in which case you will need to close the underlying
    file descriptor yourself to ensure proper cleanup.
*)

val output_of_descr: ?cleanup:bool -> file_descr -> unit BatInnerIO.output
  (** Create an {!type:output} writing on the given descriptor.
      The {!type:output} is initially in binary mode; use
      [set_binary_mode_out oc false] if text mode is desired. 

      @param cleanup If true, close the underlying file descriptor
      when the {!type:output} is closed. If false or unspecified,
      do nothing, in which case you will need to close the underlying
      file descriptor yourself to ensure proper cleanup.
*)

val descr_of_input : BatInnerIO.input -> file_descr
(** Return the descriptor corresponding to an input.

    Not all inputs have file descriptors. This function works
    only for inputs which have been created using module {!Unix}.

    @raise Invalid_argument "Unix.descr_of_in_channel" if this input
    channel doesn't have a file descriptor
*)

val descr_of_output : unit BatInnerIO.output -> file_descr
  (** Return the descriptor corresponding to an output. 
      
      Not all inputs have file descriptors. This function works
      only for inputs which have been created from module Unix.

      @raise Invalid_arg "Unix.descr_of_out_channel" if this input
      channel doesn't have a file descriptor
  *)



(*val pipeio: unit -> file_descr * file_descr
  (** As {!pipe} but return an input and an output for the
*)*)
  


val open_process_in : ?autoclose: bool -> ?cleanup:bool -> string -> BatInnerIO.input
(** High-level pipe and process management. This function
    runs the given command in parallel with the program.
    The standard output of the command is redirected to a pipe,
    which can be read via the returned input.
    The command is interpreted by the shell [/bin/sh] (cf. [system]). 

    @param autoclose If true (default value), close the input
    automatically once there is no more content to read. Otherwise,
    the input will be closed according to the usual rules of module
    {!BatIO}. Barring very specific needs (e.g. using file descriptors as
    locks), you probably want [autoclose] to be [true].

    @param cleanup If true or unspecified, close the process when the {!type:input}
    is closed. If false, do nothing, in which case you
    will need to close the process yourself to ensure proper cleanup.
*)

val open_process_out : ?cleanup:bool -> string -> unit BatInnerIO.output
  (** 
      Same as {!Unix.open_process_in}, but redirect the standard input of
      the command to a pipe.  Data written to the returned output
      is sent to the standard input of the command.
      
      {b Warning} writes on outputs are buffered, hence be careful
      to call {!Pervasives.flush} at the right times to ensure
      correct synchronization. 

      @param cleanup If true or unspecified, close the process when the {!type:output}
      is closed. If false, do nothing, in which case you
      will need to close the process yourself to ensure proper cleanup.
*)

val open_process : ?autoclose:bool -> ?cleanup:bool -> string -> BatInnerIO.input * unit BatInnerIO.output
  (** 
      Same as {!Unix.open_process_out}, but redirects both the
      standard input and standard output of the command to pipes
      connected to the two returned {!type: input}/{!type: output}.
      The returned {!type: input} is connected to the output of the
      command, and the returned {!type: output} to the input of the
      command. 
      
      @param autoclose If true (default value), close the input
      automatically once there is no more content to read. Otherwise,
      the input will be closed according to the usual rules of module
      {!BatIO}. Barring very specific needs (e.g. using file descriptors as
      locks), you probably want [autoclose] to be [true].

      @param cleanup If true or unspecified, close the process when either the
      {!type:output} or the {!type:output} is closed. If false,
      do nothing, in which case you will need to close
      the process yourself to ensure proper cleanup.
  *)



val open_process_full :
  ?autoclose:bool -> ?cleanup:bool -> string -> string array -> BatInnerIO.input * unit BatInnerIO.output * BatInnerIO.input
  (** Similar to {!Unix.open_process}, but the second argument
      specifies the environment passed to the command.  The result is
      a triple of {!type:input}/{!type:output} connected respectively
      to the standard output, standard input, and standard error of
      the command. 

      @param autoclose If true (default value), close the input
      automatically once there is no more content to read. Otherwise,
      the input will be closed according to the usual rules of module
      {!BatIO}. Barring very specific needs (e.g. using file descriptors as
      locks), you probably want [autoclose] to be [true].

     @param cleanup If true or unspecified, close the process when either the
      {!type:output} or the {!type:output} is closed. If false,
      do nothing, in which case you will need to close
      the process yourself to ensure proper cleanup.
  *)

val close_process_in : BatInnerIO.input -> process_status
  (** Close {!type:input} opened by {!Unix.open_process_in},
      wait for the associated command to terminate,
      and return its termination status.

      @raise Unix_error(EBADF, "close_process_in", "") if the argument
      is not an {!type:input} opened by {!Unix.open_process_in}.
  *)

val close_process_out : unit BatInnerIO.output -> process_status
  (** Close {!type:output} opened by {!Unix.open_process_out},
      wait for the associated command to terminate,
      and return its termination status. 

      @raise Unix_error(EBADF, "close_process_out", "") if the argument
      is not an {!type:output} opened by {!Unix.open_process_out}.
*)

val close_process : BatInnerIO.input * unit BatInnerIO.output -> process_status
  (** Close {!type:input}/{!type:output} opened by {!Unix.open_process},
      wait for the associated command to terminate,
      and return its termination status.
      
      @raise Unix_error(EBADF, "close_process", "") if the argument
      is not pair of {!type:input}/{!type:output} opened by {!Unix.open_process}.
  *)

val close_process_full :
  BatInnerIO.input * unit BatInnerIO.output * BatInnerIO.input -> process_status
  (** Close i/o opened by {!Unix.open_process_full},
      wait for the associated command to terminate,
      and return its termination status. 

      @raise Unix_error(EBADF, "close_process_full", "") if the argument
      is not a triple of {!type:input}/{!type:output} opened by {!Unix.open_process_full}.
*)
  
(** {6 High-level network connection functions} *)


val open_connection : ?autoclose:bool -> sockaddr -> BatInnerIO.input * unit BatInnerIO.output
  (** Connect to a server at the given address.
      Return a pair of input/output connected to the server. The
      connection is closed whenever either the input or the output
      is closed.

      Remember to call {!Pervasives.flush} on the output  at the right
      times to ensure correct synchronization. 

      @param autoclose If true (default value), close the input
      automatically once there is no more content to read. Otherwise,
      the input will be closed according to the usual rules of module
      {!BatIO}. Barring very specific needs (e.g. using file descriptors as
      locks), you probably want [autoclose] to be [true].
  *)

val shutdown_connection : BatInnerIO.input -> unit
  (** 
      ``Shut down'' a connection established with {!Unix.open_connection};
      that is, transmit an end-of-file condition to the server reading
      on the other side of the connection. 

      @deprecated Connections do not require a special function anymore.
      Use regular function {!BatIO.close_in} for closing connections.
*)

val establish_server : ?autoclose:bool -> ?cleanup:bool -> (BatInnerIO.input -> unit BatInnerIO.output -> unit) -> sockaddr -> unit
  (** Establish a server on the given address.

      [establish_server f addr] establishes a server on address
      [addr].  For each connection on this address, function [f] is
      called with two buffered channels connected to the client. A new
      process is created for each connection. The function
      {!Unix.establish_server} never returns normally.

      @param autoclose If true (default value), inputs passed to [f]
      close the input automatically once there is no more content to
      read. Otherwise, the input will be closed according to the usual
      rules of module {!BatIO}. Barring very specific needs (e.g. using
      file descriptors as locks), you probably want [autoclose] to be
      [true].

      @param cleanup If true or unspecified, close the connection when
      the {!type:input} or the {!type:output} is closed or
      garbage-collected. If false, do nothing, in which case you will
      need to shutdown the connection using {!shutdown_connection} to
      ensure proper cleanup.  *)


(** {6 Small tools} *)

val is_directory : string -> bool
(** [is_directory filename] returns true if [filename] refers to a directory (or symlink of a directory) *)

val restart_on_EINTR : ('a -> 'b) -> 'a -> 'b
(** [restart_on_EINTR f x] invokes [f] on [x] repetedly until the function returns
    a value or raise another exception than EINTR. *)


(**
   {6 Thread-safety internals}

   Unless you are attempting to adapt Batteries Included to a new model of
   concurrency, you probably won't need this.
*)

val lock: BatConcurrent.lock ref
(**
   A lock used to synchronize internal operations.

   By default, this is {!BatConcurrent.nolock}. However, if you're
   using a version of Batteries compiled in threaded mode, this uses
   {!BatMutex}. If you're attempting to use Batteries with another
   concurrency model, set the lock appropriately.
*)

(**{6 Obsolete stuff}*)

val in_channel_of_descr: file_descr -> BatInnerIO.input
(** @deprecated use {!input_of_descr}*)

val out_channel_of_descr: file_descr -> unit BatInnerIO.output
(** @deprecated use {!output_of_descr}. *)

val descr_of_in_channel : BatInnerIO.input -> file_descr
(** @deprecated use {!descr_of_input}. *)

val descr_of_out_channel : unit BatInnerIO.output -> file_descr
(** @deprecated use {!descr_of_output}. *)

