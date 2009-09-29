
module type Netchannels =
sig
  (** GENERATED STUFF, DO NOT EDIT
      
      This module exists only to work around an OCaml limitation which
      does not permit to access the module type corresponding to an
      existing module. (http://caml.inria.fr/mantis/view.php?id=3013)

      Hence, here we store a dump of the current signature of
      Netchannels, wrapped into a "module type" declaration. This way
      we can include it from ExtNetchannels to extend Netchannels
      signature.

      The reminder part of this module type has been generated executing:

        $ ocamlfind ocamlc -package netstring -i -c \
            `ocamlc -where`/netstring/netchannels.mli

      DO NOT EDIT BY HAND, rather re-generate it

      XXX: it should be possible to automate the generation of this file
      should be automated and integrated into Batteries' myocamlbuild.ml.
  *)

exception Closed_channel
exception Buffer_underrun
exception Command_failure of Unix.process_status
class type rec_in_channel =
  object
    method close_in : unit -> unit
    method input : string -> int -> int -> int
  end
class type raw_in_channel =
  object
    method close_in : unit -> unit
    method input : string -> int -> int -> int
    method pos_in : int
  end
class type rec_out_channel =
  object
    method close_out : unit -> unit
    method flush : unit -> unit
    method output : string -> int -> int -> int
  end
class type raw_out_channel =
  object
    method close_out : unit -> unit
    method flush : unit -> unit
    method output : string -> int -> int -> int
    method pos_out : int
  end
class type raw_io_channel =
  object
    method close_in : unit -> unit
    method close_out : unit -> unit
    method flush : unit -> unit
    method input : string -> int -> int -> int
    method output : string -> int -> int -> int
    method pos_in : int
    method pos_out : int
  end
class type compl_in_channel =
  object
    method input_byte : unit -> int
    method input_char : unit -> char
    method input_line : unit -> string
    method really_input : string -> int -> int -> unit
  end
class type in_obj_channel =
  object
    method close_in : unit -> unit
    method input : string -> int -> int -> int
    method input_byte : unit -> int
    method input_char : unit -> char
    method input_line : unit -> string
    method pos_in : int
    method really_input : string -> int -> int -> unit
  end
class type compl_out_channel =
  object
    method output_buffer : Buffer.t -> unit
    method output_byte : int -> unit
    method output_channel : ?len:int -> in_obj_channel -> unit
    method output_char : char -> unit
    method output_string : string -> unit
    method really_output : string -> int -> int -> unit
  end
class type out_obj_channel =
  object
    method close_out : unit -> unit
    method flush : unit -> unit
    method output : string -> int -> int -> int
    method output_buffer : Buffer.t -> unit
    method output_byte : int -> unit
    method output_channel : ?len:int -> in_obj_channel -> unit
    method output_char : char -> unit
    method output_string : string -> unit
    method pos_out : int
    method really_output : string -> int -> int -> unit
  end
class type io_obj_channel =
  object
    method close_in : unit -> unit
    method close_out : unit -> unit
    method flush : unit -> unit
    method input : string -> int -> int -> int
    method input_byte : unit -> int
    method input_char : unit -> char
    method input_line : unit -> string
    method output : string -> int -> int -> int
    method output_buffer : Buffer.t -> unit
    method output_byte : int -> unit
    method output_channel : ?len:int -> in_obj_channel -> unit
    method output_char : char -> unit
    method output_string : string -> unit
    method pos_in : int
    method pos_out : int
    method really_input : string -> int -> int -> unit
    method really_output : string -> int -> int -> unit
  end
class type trans_out_obj_channel =
  object
    method close_out : unit -> unit
    method commit_work : unit -> unit
    method flush : unit -> unit
    method output : string -> int -> int -> int
    method output_buffer : Buffer.t -> unit
    method output_byte : int -> unit
    method output_channel : ?len:int -> in_obj_channel -> unit
    method output_char : char -> unit
    method output_string : string -> unit
    method pos_out : int
    method really_output : string -> int -> int -> unit
    method rollback_work : unit -> unit
  end
class input_channel : in_channel -> in_obj_channel
class input_command : string -> in_obj_channel
class input_string : ?pos:int -> ?len:int -> string -> in_obj_channel
val create_input_netbuffer : Netbuffer.t -> in_obj_channel * (unit -> unit)
val lexbuf_of_in_obj_channel : in_obj_channel -> Lexing.lexbuf
val string_of_in_obj_channel : in_obj_channel -> string
val with_in_obj_channel : (#in_obj_channel as 'a) -> ('a -> 'b) -> 'b
class output_channel :
  ?onclose:(unit -> unit) -> out_channel -> out_obj_channel
class output_command : ?onclose:(unit -> unit) -> string -> out_obj_channel
class output_buffer : ?onclose:(unit -> unit) -> Buffer.t -> out_obj_channel
class output_netbuffer :
  ?onclose:(unit -> unit) -> Netbuffer.t -> out_obj_channel
class output_null : ?onclose:(unit -> unit) -> unit -> out_obj_channel
val with_out_obj_channel : (#out_obj_channel as 'a) -> ('a -> 'b) -> 'b
class rec_in_channel_delegation :
  ?close:bool -> rec_in_channel -> rec_in_channel
class raw_in_channel_delegation :
  ?close:bool -> raw_in_channel -> raw_in_channel
class in_obj_channel_delegation :
  ?close:bool -> in_obj_channel -> in_obj_channel
class rec_out_channel_delegation :
  ?close:bool -> rec_out_channel -> rec_out_channel
class raw_out_channel_delegation :
  ?close:bool -> raw_out_channel -> raw_out_channel
class out_obj_channel_delegation :
  ?close:bool -> out_obj_channel -> out_obj_channel
val lift_in :
  ?eol:string list ->
  ?buffered:bool ->
  ?buffer_size:int ->
  [ `Raw of raw_in_channel | `Rec of rec_in_channel ] -> in_obj_channel
val lift_out :
  ?buffered:bool ->
  ?buffer_size:int ->
  [ `Raw of raw_out_channel | `Rec of rec_out_channel ] -> out_obj_channel
class virtual augment_raw_in_channel :
  object
    method virtual close_in : unit -> unit
    method virtual input : string -> int -> int -> int
    method input_byte : unit -> int
    method input_char : unit -> char
    method input_line : unit -> string
    method virtual pos_in : int
    method really_input : string -> int -> int -> unit
  end
class lift_rec_in_channel :
  ?start_pos_in:int -> rec_in_channel -> in_obj_channel
class virtual augment_raw_out_channel :
  object
    method virtual close_out : unit -> unit
    method virtual flush : unit -> unit
    method virtual output : string -> int -> int -> int
    method output_buffer : Buffer.t -> unit
    method output_byte : int -> unit
    method output_channel : ?len:int -> in_obj_channel -> unit
    method output_char : char -> unit
    method output_string : string -> unit
    method virtual pos_out : int
    method really_output : string -> int -> int -> unit
  end
class lift_raw_out_channel : raw_out_channel -> out_obj_channel
class lift_rec_out_channel :
  ?start_pos_out:int -> rec_out_channel -> out_obj_channel
type input_result = [ `Data of int | `Separator of string ]
class type enhanced_raw_in_channel =
  object
    method close_in : unit -> unit
    method private enhanced_input : string -> int -> int -> input_result
    method private enhanced_input_line : unit -> string
    method input : string -> int -> int -> int
    method pos_in : int
  end
class buffered_raw_in_channel :
  ?eol:string list ->
  ?buffer_size:int -> raw_in_channel -> enhanced_raw_in_channel
class buffered_raw_out_channel :
  ?buffer_size:int -> raw_out_channel -> raw_out_channel
class input_descr :
  ?blocking:bool -> ?start_pos_in:int -> Unix.file_descr -> raw_in_channel
class output_descr :
  ?blocking:bool -> ?start_pos_out:int -> Unix.file_descr -> raw_out_channel
class socket_descr :
  ?blocking:bool ->
  ?start_pos_in:int ->
  ?start_pos_out:int -> Unix.file_descr -> raw_io_channel
type close_mode = [ `Commit | `Rollback ]
class buffered_trans_channel :
  ?close_mode:close_mode -> out_obj_channel -> trans_out_obj_channel
val make_temporary_file :
  ?mode:int ->
  ?limit:int ->
  ?tmp_directory:string ->
  ?tmp_prefix:string -> unit -> string * in_channel * out_channel
class tempfile_trans_channel :
  ?close_mode:close_mode ->
  ?tmp_directory:string ->
  ?tmp_prefix:string -> out_obj_channel -> trans_out_obj_channel
class pipe :
  ?conv:(Netbuffer.t -> bool -> Netbuffer.t -> unit) ->
  unit -> io_obj_channel
class output_filter : io_obj_channel -> out_obj_channel -> out_obj_channel
class input_filter : in_obj_channel -> io_obj_channel -> in_obj_channel

end
