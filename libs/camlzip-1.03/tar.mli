
(** Reading and writing TAR archives *)

(** This module provides functions for reading and creating tar
  archives. It supports compressed archives, and tries to deal with some
  of the extensions to the tar format that various versions of tar
  have. It can read POSIX and most gnu-tar format archives, and has a
  chance at really old V7-style tar. It cannot yet deal with sparse
  files. The good news is that it does read all the typical tarballs
  I've thrown at it. *)

(** Types of files recognized by tar *)
type file_type = REGULAR | LINK | SYMLINK | CHRSPEC | BLKSPEC | DIRECTORY | FIFO  | CONTIGIOUS | DUMPDIR (** GNU: A dir entry that contains the names of files that were in the dir at the time the dump was made. *) | LONGLINK (** GNU: Identifies the *next* file on the archive as having a long linkname. The body of this entry is the linkname. *)| LONGNAME (** GNU: Identifies the *next* file on the tape as having a long name. The body of this entry is the filename. *) | MULTIVOL (** GNU: The continuation of a file that began on another volume. *) | NAMES (** GNU: For storing filenames that do not fit into the main header. I haven't yet figured out /where/ they're stored. *) | SPARSE (** GNU: Sparse file. *) | VOLHDR (** GNU: This file is a tape/volume header. Ignore it on extraction. *)

(** Types of archives supported by this library. POSIX_FORMAT is the
  best-supported type. It tries to deal with GNU extensions and the
  mish-mash of formats that predate POSIX. If you find something it
  breaks with, please tell me so support for it can be added. *)
type record_type = POSIX_FORMAT | GNU_FORMAT | OLDGNU_FORMAT | V7_FORMAT

(** The metadata for a file in a tar archive *)
type header = {
  t_name: string;
  t_mode: int;
  t_uid: int;
  t_gid: int;
  t_size: int;
  t_mtime: int32;
  t_chksum: int;
  t_typeflag: file_type;
  t_linkname: string;
  t_format: record_type;
  t_uname: string;
  t_gname: string;
  t_devmajor: int;
  t_devminor: int;
  t_prefix: string;
  t_gnu: gnu_extras option; (** These aren't always set to meaningful values even with [OLDGNU_FORMAT] and [GNU_FORMAT] records. *)
}
and gnu_extras = {
  t_atime: int32;
  t_ctime: int32;
  t_offset: int32;
  t_realsize: int32;
}

(** Raised on errors involving not being able to parse the tar format
  being used *)
exception Error of string

(** {1 Reading a tar archive} *)

(** The type of tar files opened for reading *)
type t_in

(** Open a tar file, optionally compressed with gzip or bzip2. The
  default behavior is to guess how the file is compressed based on its
  extension. When in doubt, be specific. *)
val open_in: ?compress:[<`Plain|`Gzip|`Bzip2|`Guess>`Guess] -> string -> t_in

(** Treat an already-opened channel as a tar file, optionally compressed. *)
val open_in_chan: ?compress:[<`Plain|`Gzip|`Bzip2>`Plain] -> in_channel -> t_in

(** Return the header for the next file in the tar archive *)
val read_header: t_in -> header

(** Return the contents of the next file in the tar archive. If the
  file size is 0, returns an empty string. *)
val read_body: t_in -> string

(** Return the next file header and body in the archive *)
val read_entry: t_in -> header * string

(** Clean up and stop processing the archive without closing the input
  channel *)
val dispose: t_in -> unit

(** Clean up and close the archive's input channel. *)
val close_in: t_in -> unit

(** {1 Writing a tar archive} *)

(** The type for a tar file opened for reading *)
type t_out

(** Create a new tar file, optionally compressed with gzip or
  bzip2. *)
val open_out: ?compress:[<`Plain|`Gzip|`Bzip2>`Plain] -> string -> t_out

(** Treat already-opened channel as the sink for a tar file to be
  written to, optionally compressed. *)
val open_out_chan: ?compress:[<`Plain|`Gzip|`Bzip2>`Plain] -> out_channel -> t_out

(** Write a file to the tar archive using the given header and file
  body. [header.t_size] is set based on the length of the string that's
  used as the file. [header.t_chksum] is also filled in
  automatically. *)
val output: t_out -> header -> string -> unit

(** Flush out the tar archive but don't close the underlying
  [out_channel] *)
val flush: t_out -> unit

(** Closes the tar file *)
val close_out: t_out -> unit

