
type bzlib_error = BZ_CONFIG_ERROR | BZ_SEQUENCE_ERROR | BZ_PARAM_ERROR
		   | BZ_MEM_ERROR | BZ_DATA_ERROR | BZ_DATA_ERROR_MAGIC
		   | BZ_UNKNOWN_ERROR

let string_of_error = function
  | BZ_CONFIG_ERROR -> "Configuration Error"
  | BZ_SEQUENCE_ERROR -> "Sequence Error"
  | BZ_PARAM_ERROR -> "Invalid Parameter"
  | BZ_MEM_ERROR -> "Memory Error"
  | BZ_DATA_ERROR -> "Data Error in Bzip2 Stream"
  | BZ_DATA_ERROR_MAGIC -> "Bad Magic Number"
  | BZ_UNKNOWN_ERROR -> "Unknown"

exception Error of string * bzlib_error

let _ =
  Callback.register_exception "Bzlib.Error" (Error("",BZ_CONFIG_ERROR))

type stream

type action = BZ_RUN | BZ_FLUSH | BZ_FINISH

external compress_init: int -> int -> int -> stream = "camlzip_bzCompressInit"
external compress:
  stream -> string -> int -> int -> string -> int -> int -> action
         -> bool * int * int
  = "camlzip_bzCompress_bytecode" "camlzip_bzCompress"
external compress_end: stream -> unit = "camlzip_bzCompressEnd"


external decompress_init: int -> bool -> stream = "camlzip_bzDecompressInit"
external decompress:
  stream -> string -> int -> int -> string -> int -> int -> bool * int * int
  = "camlzip_bzDecompress_bytecode" "camlzip_bzDecompress"
external decompress_end: stream -> unit = "camlzip_bzDecompressEnd"


