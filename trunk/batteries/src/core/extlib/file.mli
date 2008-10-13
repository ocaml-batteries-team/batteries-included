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

open InnerIO

(**
   File manipulation

   @author David Teller
*)

(** {6 Utilities} *)
val lines_of : string -> string Enum.t

(** {6 File permissions} *)
type permission

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
   for more details.*)

(** {6 Opening a file for reading} *)

type open_in_flag =
  [ `create
  | `excl     (**Fail if the file exists and [`create] is set               *)
  | `text     (**Open in ascii mode -- if this flag is not specified or if the
		 operating system does not perform conversions, the file is
		 opened in binary mode.                                     *)
  | `nonblock (**Open in non-blocking mode                                  *)]

val open_in : ?mode:(open_in_flag list) -> ?perm:permission -> string -> input
(** [open_in file_name] opens the file named [file_name] for reading.

    {b Note} You will need to close the file manually. An alternative is
    to call [with_file_in] instead of [open_in].

    Naming conventions for files are platform-dependent.*)

val with_file_in : ?mode:(open_in_flag list) -> ?perm:permission -> string -> (input -> 'a) -> 'a
(** [with_file_in file_name f] opens the file named [file_name] for reading,
    invokes [f] to process the contents of that file then, once [f] has returned 
    or triggered an exception, closes the file before proceeding. *)

(** {6 Opening a file for writing} *)

type open_out_flag =
  [ `append   (**Start writing at the end of the file rather than the start *)
  | `create   (**Create the file if it does not exist                       *)
  | `trunc    (**Empty the file if it already already exists                *)
  | `excl     (**Fail if the file exists and [`create] is set               *)
  | `text     (**Open in ascii mode -- if this flag is not specified or if the
		 operating system does not perform conversions, the file is
		 opened in binary mode.                                     *)
  | `nonblock (**Open in non-blocking mode                                  *) ]


val open_out : ?mode:(open_out_flag list) -> ?perm:permission -> string -> unit output
(** [open_out file_name] opens the file named [file_name] for writing.

    {b Note} You will need to close the file manually. An alternative is
    to call [with_file_out] instead of [open_out].

    Naming conventions for files are platform-dependent.*)

val with_file_out: ?mode:(open_out_flag list) -> ?perm:permission -> string -> (unit output -> 'a) -> 'a
(** [with_file_out file_name f] opens the file named [file_name] for writing,
    invokes [f] to write onto that file then, once [f] has returned or triggered 
    an exception, closes the file before proceeding. *)

