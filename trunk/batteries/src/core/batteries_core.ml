(*
 * BatteriesCore - The core of Batteries Included (non-threaded version)
 * Copyright (C) 2008 David Teller, LIFO, Universite d'Orleans
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

(** Core of Batteries Included. *)

module Control     = struct
  module Concurrency = struct
  end

  module Labels      = Extlib.Labels

  (** Monadic operations. *)
  module Monad = struct
    module type S  = Extlib.Monad.S
  end
end

(** Data structures*)
module Data        = struct


    (** Mutable containers (arrays, stacks...)*)
    module Mutable         = struct
      module Array         = Extlib.ExtArray.Array
      module ArrayLabels   = Batlib_Baselib_ArrayLabels
      module Bigarray      = Batlib_Baselib_Bigarray     (*TODO:make enumerable*)
      module Dynarray      = Extlib.DynArray
      module Enum          = Extlib.Enum
      module Global        = Extlib.Global
      module Hashtbl       = Extlib.ExtHashtbl.Hashtbl
      module HashtblLabels = Batlib_Baselib_HashtblLabels(*TODO:Bring to feature parity with {!Hashtbl}*)
      module Queue         = Batlib_Baselib_Queue        (*TODO:build from enum?*)
      module RefList       = Extlib.RefList
      module Stack         = Batlib_Baselib_Stack        (*TODO:build from enum*)
      module Stream        = Batlib_Baselib_Stream       (*TODO:replace with latest version*)
    end

    (** Persistent containers (lists, sets...)  *)
    module Persistent      = struct
      module Dllist          = Extlib.Dllist
      module Lazy            = Batlib_Baselib_Lazy
      module List            = Extlib.ExtList.List      (*formerly Batlib_Baselib_List*)
      module ListLabels      = Batlib_Baselib_ListLabels(*TODO:Bring to feature parity with {!List}*)
      module Map             = Batlib_Baselib_Map       (*TODO:make enumerable*)
      module MapLabels       = Batlib_Baselib_MapLabels (*TODO:make enumerable*)
      module PMap            = Batlib_Baselib_Map
      module Option          = Extlib.Option
      module Set             = Batlib_Baselib_Set       (*TODO:make enumerable*)
      module SetLabels       = Batlib_Baselib_SetLabels (*TODO:make enumerable*)

(**
   {6 Note} Some mutable containers offer persistent substructures.
   For instance, {!Data.Containers.Mutable.Array.Cap} defines a
   structure [('a, [< `Read | `Write]) t], identical to ['a array]
   but such that elements of [('a, [`Read]) t] may not be modified.
*)

  end
    
  (** Boolean and bit-oriented data structures *)
  module Logical     = struct
    module BitSet = Extlib.BitSet
    module Bool   = Extlib.ExtBool.Bool
  end

  (** Numbers and operations on them.*)    
  module Numeric     = struct
    module Common      = Extlib.Number
    module Big_int     = Extlib.ExtBig_int.Big_int
    module Complex     = Extlib.ExtComplex.Complex
    module Int         = Extlib.ExtInt.Int
    module Int32       = Extlib.ExtInt32.Int32
    module Int64       = Extlib.ExtInt64.Int64
    module Native_int  = Extlib.ExtNativeint.Nativeint
    module Num         = Extlib.ExtNum.Num
    module Safe_int    = Extlib.ExtInt.SafeInt
    module Unit        = Extlib.ExtUnit.Unit
  end
    
  (** Text data structures. *)
  module Text        = struct
    
    (** {6 Latin-1}*)

    module Buffer          = Batlib_Baselib_Buffer
    module Char            = Extlib.ExtChar.Char
    module String          = struct
      include Extlib.ExtString.String
      external length : string -> int = "%string_length"
      external get : string -> int -> char = "%string_safe_get"
      external set : string -> int -> char -> unit = "%string_safe_set"
      external create : int -> string = "caml_create_string"
    end
    module StringLabels    = Batlib_Baselib_StringLabels  (*todo: wrap [Batlib_Extlib_String] with labels*)
  end
end

(**
   Parsing, printing, regular expressions and other transformations from text
   to data, from data to text and from text to text.
*)
module Languages   = struct

  (** {1 Parsing} *)
  
  module Genlex          = Batlib_Baselib_Genlex
  module Lexing          = Batlib_Baselib_Lexing
  module Parsing         = Batlib_Baselib_Parsing
  module Scanf           = Batlib_Baselib_Scanf
  module Str             = Batlib_Baselib_Str
    
  (** {1 Printing}*)
    
  module Format          = Batlib_Baselib_Format
  module Printexc        = Batlib_Baselib_Printexc
  module Printf          = struct
    include Extlib.IO.Printf
    let make_list_printer    = Extlib.IO.make_list_printer
    let lmargin              = Extlib.IO.lmargin

  end
    
    (**/**)
    
    (**
       {1 Note to developers}
       
       This module is meant to contain specifically tools which may be used for parsing
       and for printing. Regular expressions are presented in this module insofar as
       they may be used for both purposes.
       
       Here is a list of other tasks which may be added here
       - serialization-to-human-readable-formats 
       - xml, dom, etc.
       
       Here is a list of tasks which should probably not be added here
       - unicode utilities which have no special relation to parsing or printing (put them in {!Batlib.Data.Text})
       - bindings to other programming languages (no real place to put them yet, for the moment, {!Batlib.Meta})
    *)
end

(** Meta-level operations (marshalling, garbage-collection...) *)
module Meta        = struct
  
  (** {1 Language}*)
  
  module Marshal        = Batlib_Baselib_Marshal
  module Oo             = Batlib_Baselib_Oo
    
  (** {1 Interaction with other languages} *)
    
  module Callback       = Batlib_Baselib_Callback
    
  (** {1 Memory}*)
    
  module Gc             = Batlib_Baselib_Gc
  module Weak           = Batlib_Baselib_Weak
    
  (** {1 Internals}
      Here Be Dragons*)
    
  module Obj            = Batlib_Baselib_Obj
  module CamlinternalMod= Batlib_Baselib_CamlinternalMod
  module CamlinternalOO = Batlib_Baselib_CamlinternalOO
    
end

(** Interactions with the operating system (file manipulation, arguments...) *)
module System      = struct 
  
  (** {1 Environment I/O}*)
  
  module Arg           = Batlib_Baselib_Arg
  module OptParse      = Extlib.OptParse
    
  (** {1 Operations on streams}*)
    
  module IO            = Extlib.IO
  module Unzip         = Extlib.Unzip
    
  (** {1 Actual operating system calls}*)
    
  module File          = Extlib.File
  module Filename      = Batlib_Baselib_Filename
  module Unix          = Batlib_Baselib_Unix
  module UnixLabels    = Batlib_Baselib_UnixLabels
  module Sys           = Batlib_Baselib_Sys
    
  (** {1 Networking}*)
  module Network       = struct
    (** Placeholder *)
  end
end

(** Tools for compiling OCaml, generating documentation, installing libraries. *)
module Toolchain   = struct
  module Execute     = Toolchain.Builtin_tools

  (**Package management with Findlib*)
  module Findlib     = Batlib_Findlib_Findlib
end

(** Miscellaneous utilities *)
module Util        = struct
  module Base64 = Extlib.Base64
  module Digest = Batlib_Baselib_Digest
  module Random = Batlib_Baselib_Random
end

module Standard = struct
  (*include Pervasives*)
  (*include Batlib_Baselib_Pervasives*)
  include Data.Mutable.Enum
  include Extlib.Std
  let (@) = Extlib.ExtList.(@)
end

module Legacy = struct
  (**/**)
  module Array     = Array
  module ArrayLabels= ArrayLabels
  module Bigarray  = Bigarray
  module Hashtbl   = Hashtbl
  module Queue     = Queue
  module Stack     = Stack
  module Stream    = Stream
  module Lazy      = Lazy
  module List      = List
  module ListLabels= ListLabels
  module Map       = Map
  module MoreLabels= MoreLabels
  module Set       = Set
  module Big_int   = Big_int
  module Complex   = Complex
  module Int32     = Int32
  module Int64     = Int64
  module Num       = Num
  module Buffer    = Buffer
  module Char      = Char
  module String    = String
  module StringLabels = StringLabels
  module Genlex    = Genlex
  module Lexing    = Lexing
  module Parsing   = Parsing
  module Scanf     = Scanf
  module Str       = Str
  module Format    = Format
  module Printexc  = Printexc
  module Printf    = Printf
  module Marshal   = Marshal
  module Oo        = Oo
  module Callback  = Callback
  module Gc        = Gc
  module Weak      = Weak
  module Obj       = Obj
  module CamlinternalMod = CamlinternalMod
  module CamlinternalOO  = CamlinternalOO
  module Arg       = Arg
  module Filename  = Filename
  module Unix      = Unix
  module UnixLabels= UnixLabels
  module Sys       = Sys
  module Random    = Random
  (*module Pervasives = Pervasives*)
    (**/**)
end
