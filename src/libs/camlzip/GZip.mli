
(** {1 Common (de)compression interface} *)

(** {2 Decompression} *)

include Common.Compress.Decompressor

(** {2 Compression} *)

include Common.Compress.Compressor

(** {1 Low-level (de)compression interface}

    Give acces to library-specific features *)

(** {2 Compression} *)

val gzip_compress: ?level:int -> 'a InnerIO.output -> 'a InnerIO.output
  (** gzip-specific compression function, same as [GZip.compress], but
      enable to specifiy gzip-specific compression parameters

      @param level compression level (an integer between 1 and 9),
      with 1 being the weakest (but fastest) compression and 9 being
      the strongest (but slowest) compression. Default: 6 *)
