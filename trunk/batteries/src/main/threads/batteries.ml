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

      (** {6 Built-in concurrency}

	  For bytecode programs, this concurrency is implemented as fast user-level threads,
	  while for native programs this concurrency is implemented as slower-but-standard
	  native threads.*)

      open Batteries_core_threads.Control.Concurrency.Thread
      module Condition = Condition
      module Event     = Event
      module Mutex     = Mutex
      module Thread    = Thread
    end
  end

  module Labels      = Batteries_core_threads.Control.Labels

  (** Monadic operations. *)
  module Monad = struct
    module type S  = Batteries_core_threads.Control.Monad.S
  end
end

(** Data structures*)
module Data        = struct


    (** Mutable containers (arrays, stacks...)*)
    module Mutable         = struct
      open Batteries_core_threads.Data.Mutable
      module Array         = Array
      module ArrayLabels   = ArrayLabels
      module Bigarray      = Bigarray
      module Dynarray      = Dynarray
      module Enum          = Enum
      module Global        = Global
      module Hashtbl       = Hashtbl
      module HashtblLabels = HashtblLabels(*TODO:Bring to feature parity with {!Hashtbl}*)
      module Queue         = Queue        (*TODO:build from enum?*)
      module RefList       = RefList
      module Stack         = Stack        (*TODO:build from enum*)
      module Stream        = Stream       (*TODO:replace with latest version*)
    end

    (** Persistent containers (lists, sets...)  *)
    module Persistent      = struct
      open Batteries_core_threads.Data.Persistent
      module Dllist          = Dllist
      module Lazy            = Lazy
      module List            = List      (*formerly Batlib_Baselib_List*)
      module ListLabels      = ListLabels(*TODO:Bring to feature parity with {!List}*)
      module Map             = Map       (*TODO:make enumerable*)
      module MapLabels       = MapLabels (*TODO:make enumerable*)
      module PMap            = Map
      module Option          = Option
      module Set             = Set       (*TODO:make enumerable*)
      module SetLabels       = SetLabels (*TODO:make enumerable*)

(**
   {6 Note} Some mutable containers offer persistent substructures.
   For instance, {!Data.Containers.Mutable.Array.Cap} defines a
   structure [('a, [< `Read | `Write]) t], identical to ['a array]
   but such that elements of [('a, [`Read]) t] may not be modified.
*)

  end
    
  (** Boolean and bit-oriented data structures *)
  module Logical     = struct
    open Batteries_core_threads.Data.Logical
    module BitSet = BitSet
    module Bool   = Bool
  end

  (** Numbers and operations on them.*)    
  module Numeric     = struct
    open Batteries_core_threads.Data.Numeric
    (*module Interfaces  = Batlib_Interfaces_Numeric*)
    module Big_int     = Big_int
    module Complex     = Complex
    module Int         = Int
    module Int32       = Int32
    module Int64       = Int64
    module Native_int  = Nativeint
    module Num         = Num
    module Safe_int    = Safe_int
    module Unit        = Unit
  end
    
  (** Text data structures. *)
  module Text        = struct

    open Batteries_core_threads.Data.Text

    (** {6 Latin-1}*)

    module Buffer          = Buffer
    module Char            = Char
    module String          = String
    module StringLabels    = StringLabels  (*todo: wrap [Batlib_Extlib_String] with labels*)
  end
end

(**
   Parsing, printing, regular expressions and other transformations from text
   to data, from data to text and from text to text.
*)
module Languages   = struct
  open Batteries_core_threads.Languages

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
  
  module Genlex          = Genlex
  module Lexing          = Lexing
  module Parsing         = Parsing
  module Scanf           = Scanf
  module Str             = Str
    
  (** {1 Printing}*)
    
  module Format          = Format
  module Printexc        = Printexc
  module Printf          = Printf

end

(** Meta-level operations (marshalling, garbage-collection...) *)
module Meta        = struct
  open Batteries_core_threads.Meta

  (** {1 Language}*)
  
  module Marshal        = Marshal
  module Oo             = Oo
    
  (** {1 Interaction with other languages} *)
    
  module Callback       = Callback
    
  (** {1 Memory}*)
    
  module Gc             = Gc
  module Weak           = Weak
    
  (** {1 Internals}
      Here Be Dragons*)
    
  module Obj            = Obj
  module CamlinternalMod= CamlinternalMod
  module CamlinternalOO = CamlinternalOO
    
end

(** Interactions with the operating system (file manipulation, arguments...) *)
module System      = struct 
  open Batteries_core_threads.System

  (** {1 Environment I/O}*)
  
  module Arg           = Arg
  module OptParse      = OptParse
    
  (** {1 Operations on streams}*)
    
  module IO            = IO
  module Unzip         = Unzip
    
  (** {1 Actual operating system calls}*)
    
  module File          = File
  module Filename      = Filename
  module Unix          = Unix
  module UnixLabels    = UnixLabels
  module Sys           = Sys
    
  (** {1 Networking}*)
  module Network       = struct
    (** Placeholder.

	Expect 
	- {{:http://www.camlcity.org/archive/programming/ocamlnet.html}OCamlNet}
	- {{:http://sourceforge.net/projects/ocnae/}OCamlNAE} *)
  end
end

(** Tools for compiling OCaml, generating documentation, installing libraries. *)
module Toolchain   = struct
  open Batteries_core_threads.Toolchain

  (**Package management with Findlib*)
  module Findlib     = Findlib
  module Execute     = Execute

  module Boilerplate = struct
    (** Placeholder.

	Expect
	- {{:http://www.ocaml.info/home/ocaml_sources.html}Type-conv}
    *)
  end
end

(** Miscellaneous utilities *)
module Util        = struct
  open Batteries_core_threads.Util

  module Base64 = Base64
  module Digest = Digest
  module Random = Random
end

(** Access to the modules provided by INRIA. *)
module Legacy = struct
  (** Provide access to the original version of modules [Condition],
      [Event], [Mutex], [Array], [Bigarray], [Hashtbl], [Queue],
      [Stack], [Stream], [Lazy], [List], [ListLabels], [Map],
      [MoreLabels], [Set], [Big_int], [Complex], [Int32], [Int64],
      [Num], [Buffer], [Char], [String], [StringLabels], [Genlex],
      [Lexing], [Parsing], [Scanf], [Str], [Format], [Printexc],
      [Printf], [Marshal], [Oo], [Callback], [Gc], [Weak], [Obj],
      [CamlinternalMod] [CamlinternalOO] [Arg] [Filename] [Unix]
      [UnixLabels], [Sys], [Random], [Pervasives].

      If you don't know what this means, you don't need it -- in
      Batteries, all these modules are considered obsolete. Module
      [Legacy] is provided for reference, comparaison and for
      short-term work-arounds in case of bugs.

      For more information on these modules, see the documentation of OCaml.*)

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
module Condition = Control.Concurrency.Threads.Condition
module Event     = Control.Concurrency.Threads.Event
module Mutex     = Control.Concurrency.Threads.Mutex
module Thread    = Control.Concurrency.Threads.Thread
module Array     = Data.Mutable.Array
module ArrayLabels=Data.Mutable.ArrayLabels
module Bigarray  = Data.Mutable.Bigarray
module Hashtbl   = Data.Mutable.Hashtbl
module Queue     = Data.Mutable.Queue
module Stack     = Data.Mutable.Stack
module Stream    = Data.Mutable.Stream
module Lazy      = Data.Persistent.Lazy
module List      = Data.Persistent.List
module ListLabels= Data.Persistent.ListLabels
module Map       = Data.Persistent.Map
module MapLabels = Data.Persistent.MapLabels
module Set       = Data.Persistent.Set
module SetLabels = Data.Persistent.SetLabels
module Big_int   = Data.Numeric.Big_int
module Complex   = Data.Numeric.Complex
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
(**/**)


(*include Batteries_core_threads.Standard*)
include Data.Mutable.Enum
include Extlib.Std
let (@) = Extlib.ExtList.(@)
