
open Batteries_core

exception Error of string * exn option
  (** Error while compressing/decompressing.

      First argument is a human-readable error explanation.  Second
      argument is the low-level exception raised by the underlying
      (de)compression library, if any.*)

module type Decompressor =
sig

  val uncompress: System.IO.input -> System.IO.input
    (** Wrap an input channel, decompressing transparently data when
	reading from it.

	Operations performed on the returned channel can raise, in
	addition to their usual exceptions, [Error]. *)

(* val open_in: string -> System.IO.input *)
(*   (\** directly open a compressed file to read from it *\) *)

end

module type Compressor =
sig

  val compress: 'a System.IO.output -> 'a System.IO.output
    (** wrap an output channel, compressing transparently data when
	writing to it.
	
	Operations performed on the returned channel can raise, in
	addition to their usual exceptions, [Error]. *)

(* val open_out: string -> unit System.IO.output *)
(*   (\** directly open a compressed file to write to it *\) *)

end
