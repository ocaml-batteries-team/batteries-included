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

(**
   File manipulation.

   @author David Teller
*)


open BatInnerIO

(** {6 Utilities} *)

val lines_of : string -> string BatEnum.t
(** [line_of name] reads the contents of file [name] as an enumeration of lines.
    The file is automatically closed once the last line has been reached or the
    enumeration is garbage-collected. *)

val write_lines: string -> string BatEnum.t -> unit
(** [write_lines name lines] writes strings given by [lines] to file [name] with newline character appended to each line. *)

val size_of: string -> int
(** [size_of name] returns the size of file [name] in bytes.*)

val size_of_big: string -> Int64.t
(** [size_of_big name] returns the size of file [name] in bytes, as a 64-bit integer.

    This function is provided as the size of a file larger than 1 Gb cannot
    be represented with an [int] on a 32-bit machine.*)

(** {6 File permissions}

    File permissions are used when creating a file to allow controlling which users
    may read, write or open that file. To use a permission, create a value of type
    {!permission} and pass it as argument to {!open_in}, {!open_out}, {!with_file_in} or
    {!with_file_out}.
*)

type permission
(** The list of operations which are permitted on a file.*)

val default_permission: permission
(**Default permissions.*)

val user_read:   permission
(**Give the current user permission to read the file.
   Ignored under Windows.*)

val user_write:  permission
(**Give the current user permission to write the file*)

val user_exec:   permission
(**Give the current user permission to execute the file.
   Ignored under Windows.*)

val group_read:  permission
(**Give the permission to read the file to the group
   containing the user. Ignored under Windows.*)

val group_write: permission
(**Give the permission to write the file to the group
   containing the user. Ignored under Windows.*)

val group_exec:  permission
(**Give the permission to execute the file to the group
   containing the user. Ignored under Windows.*)

val other_read:  permission
(**Give the permission to read the file to the rest
   of the world. Ignored under Windows.*)

val other_write: permission
(**Give the permission to modify the file to the rest
   of the world. Ignored under Windows.*)

val other_exec:  permission
(**Give the permission to execute the file to the rest
   of the world. Ignored under Windows.*)

val perm : permission list -> permission
(**Join permissions*)

val unix_perm : int -> permission
(**Create a permission from a Unix-style octal integer.
   See your favorite Unix documentation on [chmod]
   for more details.
   @raise Invalid_argument if given number outside the [[0, 0o777]] range *)

val set_permissions: string -> permission -> unit
(** Set the permissions on a file.*)

val chmod: string -> permission -> unit
(** As {!set_permissions}*)

(** {6 Opening a file for reading} *)

type open_in_flag =
  [ `create
  | `excl     (**Fail if the file exists and [`create] is set               *)
  | `text     (**Open in ascii mode -- if this flag is not specified or if the
				 operating system does not perform conversions, the file is
				 opened in binary mode.                                     *)
  | `nonblock (**Open in non-blocking mode                                  *)
  | `mmap     (**Open in memory-mapped mode (experimental)*)                 ]

val open_in : ?mode:(open_in_flag list) -> ?perm:permission -> string -> input
(** [open_in file_name] opens the file named [file_name] for reading.

    {b Note} You will need to close the file manually, with
    {!BatIO.close_in}. An alternative is to call [with_file_in] instead
    of [open_in].

    Naming conventions for files are platform-dependent.*)

val with_file_in : ?mode:(open_in_flag list) -> ?perm:permission -> string -> (input -> 'a) -> 'a
(** [with_file_in file_name f] opens the file named [file_name] for reading,
    invokes [f] to process the contents of that file then, once [f] has returned
    or triggered an exception, closes the file before proceeding. *)

(** {6 Opening a file for writing} *)

type open_out_flag =
  [ `append   (**Start writing at the end of the file rather than the start *)
  | `create   (**Create the file if it does not exist                       *)
  | `trunc    (**Empty the file if it already exists; on by default         *)
  | `excl     (**Fail if the file exists and [`create] is set               *)
  | `text     (**Open in ascii mode -- if this flag is not specified or if the
				 operating system does not perform conversions, the file is
				 opened in binary mode.                                     *)
  | `nonblock (**Open in non-blocking mode                                  *) ]
(** Flags governing file output; they correspond to the relevant
    flags to the POSIX [open()] call.  The default flags are
    [[`create; `trunc]]. *)


val open_out : ?mode:(open_out_flag list) -> ?perm:permission -> string -> unit output
(** [open_out file_name] opens the file named [file_name] for writing.

    {b Note} You will need to close the file manually, with
    {!BatIO.close_out}. An alternative is to call [with_file_out]
    instead of [open_out].

    Naming conventions for files are platform-dependent.*)

val with_file_out: ?mode:(open_out_flag list) -> ?perm:permission -> string -> (unit output -> 'a) -> 'a
(** [with_file_out file_name f] opens the file named [file_name] for writing,
    invokes [f] to write onto that file then, once [f] has returned or triggered
    an exception, closes the file before proceeding. *)

(** {6 Opening a temporary file for writing} *)

type open_temporary_out_flag =
  [ open_out_flag
  | `delete_on_exit (**Should the file be deleted when program ends?*) ]

val open_temporary_out: ?mode:(open_temporary_out_flag list) -> ?prefix:string -> ?suffix:string -> ?temp_dir:string -> unit ->
  (unit output * string)
(** [open_temporary_out ()] opens a new temporary file for writing.

    @param prefix a string which should appear at the start of your temporary file name
    (by default ["ocaml"])
    @param suffix a string which should appear at the end of your temporary file name
    (by default ["tmp"])
    @param temp_dir indicates what temp dir to use

    @return The name of the file and the [output] for writing in it.

    {b Note} You will need to close the file manually. An alternative is
    to call [with_temporary_out] instead of [open_out].

    Naming conventions for files are platform-dependent.*)

val with_temporary_out: ?mode:(open_temporary_out_flag list) -> ?prefix:string -> ?suffix:string -> ?temp_dir:string -> (unit output -> string -> 'a) -> 'a
  (** [with_temporary_out f] opens a new temporary file for writing, invokes [f] with
      to write onto that file then, once [f] has returned or triggered an exception,
      closes the file before proceeding.

      @param prefix a string which should appear at the start of your temporary file name
      (by default ["ocaml"])
      @param suffix a string which should appear at the end of your temporary file name
      (by default ["tmp"])
      @param temp_dir indicates what temp dir to use

      @return The name of the file and the [output] for writing in it.

      Naming conventions for files are platform-dependent.*)
