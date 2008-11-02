
open Batteries_core

module type Decompressor =
sig

  val uncompress: System.IO.input -> System.IO.input
    (** wrap an input channel, decompressing transparently data when
	reading from it *)

  (* val open_in: string -> System.IO.input *)
  (*   (\** directly open a compressed file to read from it *\) *)

end

module type Compressor =
sig

  val compress: 'a System.IO.output -> 'a System.IO.output
    (** wrap an output channel, compressing transparently data when
	writing to it *)

  (* val open_out: string -> unit System.IO.output *)
  (*   (\** directly open a compressed file to write to it *\) *)

end
