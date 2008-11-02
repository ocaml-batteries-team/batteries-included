
open Batteries_core

include System.Compress.Decompressor
include System.Compress.Compressor

let gzip_compress: ?level:int -> 'a System.IO.output -> 'a System.IO.output
  (** gzip-specific compression function, same as [GZip.compress], but
      enable to specifiy gzip-specific compression parameters

      @param level compression level (an integer between 1 and 9),
      with 1 being the weakest (but fastest) compression and 9 being
      the strongest (but slowest) compression. Default: 6 *)
