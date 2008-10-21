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



(** The root of the revised standard library.*)

(** Tools for changing the control flow of a program, from error-management to concurrency.*)
module Control     = struct

  (** Everything related to parallelism and concurrency. *)
  module Concurrency = struct

    (** Concurrency operations as defined by OCaml's base library. *)
    module Threads = struct


      (** {6 Important note} 

	  This module is only defined in multi-threaded versions of Batteries Included.*)

      module Condition = Batteries_core_threads.Control.Concurrency.Thread.Condition
      module Event     = Batteries_core_threads.Control.Concurrency.Thread.Event
      module Mutex     = Batteries_core_threads.Control.Concurrency.Thread.Mutex
      module Thread    = Batteries_core_threads.Control.Concurrency.Thread.Thread
    end
  end

  module Labels      = Batteries_core_threads.Control.Labels

  module Monad = Batteries_core_threads.Control.Monad
end

(** Data structures*)
module Data        = struct


    (** Mutable containers (arrays, stacks...)*)
    module Mutable         = struct
      module Array         = Batteries_core_threads.Data.Mutable.Array
      module ArrayLabels   = Batteries_core_threads.Data.Mutable.ArrayLabels
      module Bigarray      = Batteries_core_threads.Data.Mutable.Bigarray
      module Dllist        = Batteries_core_threads.Data.Mutable.Dllist
      module Dynarray      = Batteries_core_threads.Data.Mutable.Dynarray
      module Enum          = Batteries_core_threads.Data.Mutable.Enum
      module Global        = Batteries_core_threads.Data.Mutable.Global
      module Hashtbl       = Batteries_core_threads.Data.Mutable.Hashtbl
      module HashtblLabels = Batteries_core_threads.Data.Mutable.HashtblLabels(*TODO:Bring to feature parity with {!Hashtbl}*)
      module Queue         = Batteries_core_threads.Data.Mutable.Queue        (*TODO:build from enum?*)
      module Ref           = Batteries_core_threads.Data.Mutable.Ref
      module RefList       = Batteries_core_threads.Data.Mutable.RefList
      module Stack         = Batteries_core_threads.Data.Mutable.Stack        (*TODO:build from enum*)
      module Stream        = Batteries_core_threads.Data.Mutable.Stream       (*TODO:replace with latest version*)
    end

    (** Persistent containers (lists, sets...)  *)
    module Persistent      = struct
      module Lazy            = Batteries_core_threads.Data.Persistent.Lazy
      module List            = Batteries_core_threads.Data.Persistent.List      (*formerly Batlib_Baselib_List*)
      module ListLabels      = Batteries_core_threads.Data.Persistent.ListLabels(*TODO:Bring to feature parity with {!List}*)
      module Map             = Batteries_core_threads.Data.Persistent.Map       (*TODO:make enumerable*)
      module MapLabels       = Batteries_core_threads.Data.Persistent.MapLabels (*TODO:make enumerable*)
      module MultiPMap       = Batteries_core_threads.Data.Persistent.MultiPMap
      module Option          = Batteries_core_threads.Data.Persistent.Option
      module OptionLabels    = Batteries_core_threads.Data.Persistent.OptionLabels
      module PMap            = Batteries_core_threads.Data.Persistent.PMap
      module PSet            = Batteries_core_threads.Data.Persistent.PSet
      module Set             = Batteries_core_threads.Data.Persistent.Set       (*TODO:make enumerable*)
      module SetLabels       = Batteries_core_threads.Data.Persistent.SetLabels (*TODO:make enumerable*)

(**
   {6 Note} Some mutable containers offer persistent substructures.
   For instance, {!Data.Containers.Mutable.Array.Cap} defines a
   structure [('a, [< `Read | `Write]) t], identical to ['a array]
   but such that elements of [('a, [`Read]) t] may not be modified.
*)

  end
    
  (** Boolean and bit-oriented data structures *)
  module Logical     = struct
    module BitSet = Batteries_core_threads.Data.Logical.BitSet
    module Bool   = Batteries_core_threads.Data.Logical.Bool
  end

  (** Numbers and operations on them.*)    
  module Numeric     = struct
    open Batteries_core_threads.Data.Numeric
    module Common      = Batteries_core_threads.Data.Numeric.Common
    module Big_int     = Batteries_core_threads.Data.Numeric.Big_int
    module Complex     = Batteries_core_threads.Data.Numeric.Complex
    module Float       = Batteries_core_threads.Data.Numeric.Float
    module Int         = Batteries_core_threads.Data.Numeric.Int
    module Int32       = Batteries_core_threads.Data.Numeric.Int32
    module Int64       = Batteries_core_threads.Data.Numeric.Int64
    module Native_int  = Batteries_core_threads.Data.Numeric.Native_int
    module Num         = Batteries_core_threads.Data.Numeric.Num
    module Safe_int    = Batteries_core_threads.Data.Numeric.Safe_int
    module Unit        = Batteries_core_threads.Data.Numeric.Unit
  end
    
  (** Text data structures. *)
  module Text        = struct

    (** {6 Latin-1}*)

    module Buffer          = Batteries_core_threads.Data.Text.Buffer
    module Char            = Batteries_core_threads.Data.Text.Char
    module String          = Batteries_core_threads.Data.Text.String
    module StringLabels    = Batteries_core_threads.Data.Text.StringLabels  (*todo: wrap [Batlib_Extlib_String] with labels*)

    (** {6 Unicode}*)

    module Rope            = Batteries_core_threads.Data.Text.Rope
    module UChar           = Batteries_core_threads.Data.Text.UChar
    module UTF8            = Batteries_core_threads.Data.Text.UTF8
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
  
  module Genlex          = Batteries_core_threads.Languages.Genlex
  module Lexing          = Batteries_core_threads.Languages.Lexing
  module Parsing         = Batteries_core_threads.Languages.Parsing
  module Scanf           = Batteries_core_threads.Languages.Scanf
  module Str             = Batteries_core_threads.Languages.Str

  (** {2 Parser combinator library}*)

  module CharParser      = Batteries_core_threads.Languages.CharParser
  module ParserCo        = Batteries_core_threads.Languages.ParserCo
    
  (** {1 Printing}*)
    
  module Format          = Batteries_core_threads.Languages.Format
  module Printexc        = Batteries_core_threads.Languages.Printexc
  module Printf          = Batteries_core_threads.Languages.Printf

  (** {1 Serialization to human-readable formats}
      XML, JSON, S-Expressions ...*)

  module SExpr           = Batteries_core_threads.Languages.SExpr
end

(** Meta-level operations (marshalling, garbage-collection...) *)
module Meta        = struct

  (** {1 Language}*)
  
  module Marshal        = Batteries_core_threads.Meta.Marshal
  module Oo             = Batteries_core_threads.Meta.Oo
    
  (** {1 Interaction with other languages} *)
    
  module Callback       = Batteries_core_threads.Meta.Callback
    
  (** {1 Memory}*)
    
  module Gc             = Batteries_core_threads.Meta.Gc
  module Weak           = Batteries_core_threads.Meta.Weak
    
  (** {1 Internals}
      Here Be Dragons*)
    
  module Obj            = Batteries_core_threads.Meta.Obj
  module CamlinternalMod= Batteries_core_threads.Meta.CamlinternalMod
  module CamlinternalOO = Batteries_core_threads.Meta.CamlinternalOO
    
end

(** Interactions with the operating system (file manipulation, arguments...) *)
module System      = struct 

  (** {1 Environment I/O}*)
  
  module Arg           = Batteries_core_threads.System.Arg
  module OptParse      = Batteries_core_threads.System.OptParse
    
  (** {1 Operations on streams}*)
    
  module IO            = Batteries_core_threads.System.IO
  module Unzip         = Batteries_core_threads.System.Unzip
    
  (** {1 Actual operating system calls}*)
    
  module File          = Batteries_core_threads.System.File
  module Filename      = Batteries_core_threads.System.Filename
  module Unix          = Batteries_core_threads.System.Unix
  module UnixLabels    = Batteries_core_threads.System.UnixLabels
  module Sys           = Batteries_core_threads.System.Sys
    
  (** {1 Networking}*)
  module Network       = struct
    (** Placeholder.

	Expect OCamlNet here.*)
  end
end

(**
   Automatically opened module
*)
module Standard = Batteries_core_threads.Standard(*This module is actually opened by a Camlp4 trick.*)

(** Tools for compiling OCaml, generating documentation, installing libraries. *)
module Toolchain   = struct
  

  (**Package management with Findlib*)
  module Findlib     = Batteries_core_threads.Toolchain.Findlib
  module Execute     = Batteries_core_threads.Toolchain.Execute

end

(** Miscellaneous utilities *)
module Util        = struct
  module Base64 = Batteries_core_threads.Util.Base64
  module Digest = Batteries_core_threads.Util.Digest
  module Random = Batteries_core_threads.Util.Random
end

(** Access to the modules provided by INRIA. 

    For more information, see the documentation of OCaml.
*)
module Legacy = struct
  (**/**)
  module Condition = Condition
  module Event     = Event
  module Mutex     = Mutex
  module Thread    = Thread
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
  module Pervasives = Pervasives
    (**/**)
end



(**/**)
(**Shortcuts for commonly used modules*)
module Condition = Control.Concurrency.Threads.Condition
module Event     = Control.Concurrency.Threads.Event
module Mutex     = Control.Concurrency.Threads.Mutex
module Thread    = Control.Concurrency.Threads.Thread
module Array     = Data.Mutable.Array
module ArrayLabels=Data.Mutable.ArrayLabels
module Bigarray  = Data.Mutable.Bigarray
module Enum      = Data.Mutable.Enum
module Hashtbl   = Data.Mutable.Hashtbl
module Queue     = Data.Mutable.Queue
module Stack     = Data.Mutable.Stack
module Stream    = Data.Mutable.Stream
module Lazy      = Data.Persistent.Lazy
module List      = Data.Persistent.List
module ListLabels= Data.Persistent.ListLabels
module Map       = Data.Persistent.Map
module MapLabels = Data.Persistent.MapLabels
module Option    = Data.Persistent.Option
module Set       = Data.Persistent.Set
module SetLabels = Data.Persistent.SetLabels
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
module Printexc  = Languages.Printexc
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
(*module Pa_type_conv = Toolchain.Boilerplate.Type_conv*)
(**/**)


