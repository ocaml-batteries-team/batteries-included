(*
 * Batteries - Root of Batteries Included hierarchy (threaded version)
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

module Inner = Batteries_core

(** The root of the revised standard library.*)

(** Tools for changing the control flow of a program, from error-management to concurrency.*)
module Control     = struct

  (** Everything related to parallelism and concurrency. *)
  module Concurrency = struct

    module Common = Inner.Control.Concurrency.Common

    (** Concurrency operations as defined by OCaml's base library. *)
    module Threads = struct


      (** {6 Important note} 

	  This module is only defined in multi-threaded versions of Batteries Included.*)

    end
  end
  module Exceptions  = Inner.Control.Exceptions
  module Labels      = Inner.Control.Labels

  (** Monadic operations. *)
  module Monad = Inner.Control.Monad
end

(** Data structures*)
module Data        = struct


    (** Mutable containers (arrays, stacks...)*)
    module Mutable         = struct
      module Array         = Inner.Data.Mutable.Array
      module Bigarray      = Inner.Data.Mutable.Bigarray
      module Dllist        = Inner.Data.Mutable.Dllist
      module Dynarray      = Inner.Data.Mutable.Dynarray
      module Enum          = Inner.Data.Mutable.Enum
      module Global        = Inner.Data.Mutable.Global
      module Hashtbl       = Inner.Data.Mutable.Hashtbl
      module Queue         = Inner.Data.Mutable.Queue
      module Ref           = Inner.Data.Mutable.Ref
      module RefList       = Inner.Data.Mutable.RefList
      module Stack         = Inner.Data.Mutable.Stack
      module Stream        = Inner.Data.Mutable.Stream
    end

    (** Persistent containers (lists, sets...)  *)
    module Persistent      = struct
      module Lazy            = Inner.Data.Persistent.Lazy
      module List            = Inner.Data.Persistent.List
      module Map             = Inner.Data.Persistent.Map
      module MapLabels       = Inner.Data.Persistent.MapLabels (*TODO:make enumerable*)
      module MultiPMap       = Inner.Data.Persistent.MultiPMap
      module PMap            = Inner.Data.Persistent.PMap
      module PSet            = Inner.Data.Persistent.PSet
      module Option          = Inner.Data.Persistent.Option
      module OptionLabels    = Inner.Data.Persistent.OptionLabels
      module Set             = Inner.Data.Persistent.Set

(**
   {6 Note} Some mutable containers offer persistent substructures.
   For instance, {!Data.Containers.Mutable.Array.Cap} defines a
   structure [('a, [< `Read | `Write]) t], identical to ['a array]
   but such that elements of [('a, [`Read]) t] may not be modified.
*)

  end
    
  (** Boolean and bit-oriented data structures *)
  module Logical     = struct
    module BitSet = Inner.Data.Logical.BitSet
    module Bool   = Inner.Data.Logical.Bool
  end

  (** Numbers and operations on them.*)    
  module Numeric     = struct
    open Inner.Data.Numeric
    module Common      = Inner.Data.Numeric.Common
    module Big_int     = Inner.Data.Numeric.Big_int
    module Complex     = Inner.Data.Numeric.Complex
    module Float       = Inner.Data.Numeric.Float
    module Int         = Inner.Data.Numeric.Int
    module Int32       = Inner.Data.Numeric.Int32
    module Int64       = Inner.Data.Numeric.Int64
    module Native_int  = Inner.Data.Numeric.Native_int
    module Num         = Inner.Data.Numeric.Num
    module Safe_int    = Inner.Data.Numeric.Safe_int
    module Unit        = Inner.Data.Numeric.Unit
  end
    
  (** Text data structures. *)
  module Text        = struct

    (** {6 Latin-1}*)

    module Buffer          = Inner.Data.Text.Buffer
    module Char            = Inner.Data.Text.Char
    module String          = Inner.Data.Text.String
    module StringLabels    = Inner.Data.Text.StringLabels  (*todo: wrap [Batlib_Extlib_String] with labels*)

    (** {6 Unicode}*)

    module Rope            = Inner.Data.Text.Rope
    module UChar           = Inner.Data.Text.UChar
    module UTF8            = Inner.Data.Text.UTF8
  end
end

(**
   Parsing, printing, regular expressions and other transformations from text
   to data, from data to text and from text to text.
*)
module Languages   = struct


    (**
       This module contains everything related to transformation from text to data, 
       from data to text and from text to text. As such, it contains parsers, lexers,
       pretty-printers, unparsers, regular expressions, etc.
       
       In the future, it will also contain serialization-to-human-readable-formats
       (e.g. JSON, XML, S-Expressions...), manipulation of language-related data
       structures (more S-Expressions, DOM...), etc.

       This module is not the right place for general text utilites
       not related to parsing, serializing or printing (e.g. Unicode
       transcodings), nor bindings to other programming languages.  *)

  (** {1 Parsing} *)
  
  module Genlex          = Inner.Languages.Genlex
  module Lexing          = Inner.Languages.Lexing
  module Parsing         = Inner.Languages.Parsing
  module Scanf           = Inner.Languages.Scanf
  module Str             = Inner.Languages.Str

  (** {2 Parser combinator library}*)

  module ParserCo        = Inner.Languages.ParserCo
  module CharParser      = Inner.Languages.CharParser
  module UCharParser     = Inner.Languages.UCharParser

  (** {1 Printing}*)
    
  module Format          = Inner.Languages.Format
  module Printf          = Inner.Languages.Printf

  (** {1 Serialization to human-readable formats}
      XML, JSON, S-Expressions ...*)

  module SExpr           = Inner.Languages.SExpr
end

(** Meta-level operations (marshalling, garbage-collection...) *)
module Meta        = struct

  (** {1 Language}*)
  
  module Marshal        = Inner.Meta.Marshal
  module Oo             = Inner.Meta.Oo
    
  (** {1 Interaction with other languages} *)
    
  module Callback       = Inner.Meta.Callback
    
  (** {1 Memory}*)
    
  module Gc             = Inner.Meta.Gc
  module Weak           = Inner.Meta.Weak
    
  (** {1 Internals}
      Here Be Dragons*)
    
  module Obj            = Inner.Meta.Obj
  module CamlinternalMod= Inner.Meta.CamlinternalMod
  module CamlinternalOO = Inner.Meta.CamlinternalOO
    
end

(** Interactions with the operating system (file manipulation, arguments...) *)
module System      = struct 

  (** {1 Environment I/O}*)
  
  module Arg           = Inner.System.Arg
  module OptParse      = Inner.System.OptParse
    
  (** {1 Operations on streams}*)
    
  module IO            = Inner.System.IO

  (** {2 Compression/Decompression} *)
  module Compress      = Libs.Common.Compress
  module GZip          = Libs.GZip
    
  (** {1 Actual operating system calls}*)
    
  module File          = Inner.System.File
  module Filename      = Inner.System.Filename
  module Unix          = Inner.System.Unix
  module UnixLabels    = Inner.System.UnixLabels
  module Sys           = Inner.System.Sys
    
  (** {1 Networking}*)
  module Network       = struct
    (** Placeholder.

	Expect OCamlNet here.*)
  end
end

module Standard = Inner.Standard

(** Tools for compiling OCaml, generating documentation, installing libraries. *)
module Toolchain   = struct

  module Findlib     = Inner.Toolchain.Findlib
  module Execute     = Inner.Toolchain.Execute

end

(** Miscellaneous utilities *)
module Util        = struct
  module Base64 = Inner.Util.Base64
  module Digest = Inner.Util.Digest
  module Random = Inner.Util.Random
end

(** Access to the modules provided by INRIA. 

    For more information, see the documentation of OCaml.
*)
module Legacy = struct
  (**/**)
  module Array     = Array
  module ArrayLabels= Inner.Legacy.ArrayLabels
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
  module Pervasives = Pervasives
    (**/**)
end

(**/**)
module Array     = Data.Mutable.Array
module ArrayLabels=struct include Data.Mutable.Array;; include Data.Mutable.Array.Labels end
module Bigarray  = Data.Mutable.Bigarray
module Enum      = Data.Mutable.Enum
module Hashtbl   = Data.Mutable.Hashtbl
module Queue     = Data.Mutable.Queue
module Stack     = Data.Mutable.Stack
module Stream    = Data.Mutable.Stream
module Lazy      = Data.Persistent.Lazy
module List      = Data.Persistent.List
module ListLabels= struct include Data.Persistent.List;; include Labels end
module Map       = Data.Persistent.Map
module MapLabels = Data.Persistent.MapLabels
module Option    = Data.Persistent.Option
module Set       = Data.Persistent.Set
module SetLabels = struct include Data.Persistent.Set;; include Labels end
module Big_int   = Data.Numeric.Big_int
module Complex   = Data.Numeric.Complex
module Int       = Data.Numeric.Int
module Int32     = Data.Numeric.Int32
module Int64     = Data.Numeric.Int64
module Num       = Data.Numeric.Num
module Buffer    = Data.Text.Buffer
module Char      = Data.Text.Char
module String    = Data.Text.String
module StringLabels = Data.Text.StringLabels
module Genlex    = Languages.Genlex
module Lexing    = Languages.Lexing
module Parsing   = Languages.Parsing
module Scanf     = Languages.Scanf
module Str       = Languages.Str
module Format    = Languages.Format
module Printf    = Languages.Printf
module Marshal   = Meta.Marshal
module Oo        = Meta.Oo
module Callback  = Meta.Callback
module Gc        = Meta.Gc
module Weak      = Meta.Weak
module Obj       = Meta.Obj
module CamlinternalMod = Meta.CamlinternalMod
module CamlinternalOO  = Meta.CamlinternalOO
module Arg       = System.Arg
module Filename  = System.Filename
module Unix      = System.Unix
module UnixLabels= System.UnixLabels
module Sys       = System.Sys
module Random    = Util.Random
module Printexc  = Printexc
module MoreLabels= struct(*For compatibility with the base lib's [MoreLabels]*)
  module HashtblLabels = struct include Data.Mutable.Hashtbl;; include Labels end
  module SetLabels     = struct include Data.Persistent.Set;; include Labels end
end
(**/**)


