(* Batteries Included - (De)Compression modules
 * 
 * Copyright (C) 2008 Stefano Zacchiroli <zack@upsilon.cc>
 * 
 * This library is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation; either version 2.1 of the
 * License, or (at your option) any later version, with the special
 * exception on linking described in file LICENSE.
 * 
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301
 * USA *)

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
