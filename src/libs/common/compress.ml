
open Extlib

exception Error of string * exn option
  (** Error while compressing/decompressing.

      First argument is a human-readable error explanation.  Second
      argument is the low-level exception raised by the underlying
      (de)compression library, if any.*)

module type Decompressor =
sig

  val uncompress: InnerIO.input -> InnerIO.input
    (** Wrap an input channel, decompressing transparently data when
	reading from it.

	Operations performed on the returned channel can raise, in
	addition to their usual exceptions, [Error]. *)

  val open_in: ?mode:File.open_in_flag list -> ?perm:File.permission ->
    string ->
    InnerIO.input
      (** Shorthand: directly open a compressed file to read from it
	  See [File.open_in] *)

end

module type Compressor =
sig

  val compress: 'a InnerIO.output -> 'a InnerIO.output
    (** wrap an output channel, compressing transparently data when
	writing to it.
	
	Operations performed on the returned channel can raise, in
	addition to their usual exceptions, [Error]. *)

  val open_out: ?mode:File.open_out_flag list -> ?perm:File.permission ->
    string ->
    unit InnerIO.output
      (** Shorthand: directly open a compressed file to write to it.
	  See [File.open_out] *)

end
