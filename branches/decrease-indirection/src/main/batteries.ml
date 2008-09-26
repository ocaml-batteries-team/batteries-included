(*
 * Batlib - Root of Batteries Included hierarchy
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

    (** Concurrency operations as defined by OCaml's base library *)
    module Threads = struct
      module Condition = Condition
      module Event     = Event
      module Mutex     = Mutex
      module Thread    = Thread
    end
  end

(*  module Labels      = Batlib_Misc_Labels*)
end

(** Data structures*)
module Data        = struct


    (** Mutable containers (arrays, stacks...)*)
    module Mutable         = struct
      module Array         = Extlib.ExtArray.Array
      module ArrayLabels   = BatteriesGlue.BaselibGlue.ArrayLabels
      module Bigarray      = BatteriesGlue.BaselibGlue.Bigarray     (*TODO:make enumerable*)
      module Dynarray      = Extlib.DynArray
      module Enum          = Extlib.Enum
      module Global        = Extlib.Global
      module Hashtbl       = Extlib.ExtHashtbl.Hashtbl       (*formerly BatteriesGlue.BaselibGlue.Hashtbl*) 
	(* ?? HashedType in Batlib_Extlib_Hashtbl *)
      module HashtblLabels = BatteriesGlue.BaselibGlue.HashtblLabels(*TODO:Bring to feature parity with {!Hashtbl}*)
      module Queue         = BatteriesGlue.BaselibGlue.Queue        (*TODO:build from enum?*)
      module RefList       = Extlib.RefList
      module Stack         = BatteriesGlue.BaselibGlue.Stack        (*TODO:build from enum*)
      module Stream        = BatteriesGlue.BaselibGlue.Stream       (*TODO:replace with latest version*)
    end

    (** Persistent containers (lists, sets...)  *)
    module Persistent      = struct
      module Dllist          = Extlib.Dllist
      module Lazy            = BatteriesGlue.BaselibGlue.Lazy
      module List            = Extlib.ExtList.List      (*formerly BatteriesGlue.BaselibGlue.List*)
      module ListLabels      = BatteriesGlue.BaselibGlue.ListLabels(*TODO:Bring to feature parity with {!List}*)
      module Map             = BatteriesGlue.BaselibGlue.Map       (*TODO:make enumerable*)
      module MapLabels       = BatteriesGlue.BaselibGlue.MapLabels (*TODO:make enumerable*)
      module PMap            = BatteriesGlue.BaselibGlue.Map
      module Option          = Extlib.Option
      module Set             = BatteriesGlue.BaselibGlue.Set       (*TODO:make enumerable*)
      module SetLabels       = BatteriesGlue.BaselibGlue.SetLabels (*TODO:make enumerable*)

(**
   {6 Note} Some mutable containers offer persistent substructures.
   For instance, {!Data.Containers.Mutable.Array.Cap} defines a
   structure [('a, [< `Read | `Write]) t], identical to ['a array]
   but such that elements of [('a, [`Read]) t] may not be modified.
*)

  end
    
  (** Boolean and bit-oriented data structures *)
  module Logical     = struct
    module Bitset = Extlib.BitSet
    module Bool   = Extlib.ExtBool.Bool
  end

  (** Numbers and operations on them.*)    
  module Numeric     = struct
    (*module Interfaces  = Batlib_Interfaces_Numeric*)
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

    module Buffer          = BatteriesGlue.BaselibGlue.Buffer
    module Char            = Extlib.ExtChar.Char
    module String          = struct
      include Extlib.ExtString.String
      external length : string -> int = "%string_length"
      external get : string -> int -> char = "%string_safe_get"
      external set : string -> int -> char -> unit = "%string_safe_set"
      external create : int -> string = "caml_create_string"
    end
    module StringLabels    = BatteriesGlue.BaselibGlue.StringLabels  (*todo: wrap [Batlib_Extlib_String] with labels*)
  end
end

(**
   Parsing, printing, regular expressions and other transformations from text
   to data, from data to text and from text to text.
*)
module Languages   = struct

  (** {1 Parsing} *)
  
  module Genlex          = BatteriesGlue.BaselibGlue.Genlex
  module Lexing          = BatteriesGlue.BaselibGlue.Lexing
  module Parsing         = BatteriesGlue.BaselibGlue.Parsing
  module Scanf           = BatteriesGlue.BaselibGlue.Scanf
  module Str             = BatteriesGlue.BaselibGlue.Str
    
  (** {1 Printing}*)
    
  module Format          = BatteriesGlue.BaselibGlue.Format
  module Printexc        = BatteriesGlue.BaselibGlue.Printexc
  module Printf          = struct
    include Extlib.IO.Printf
    let make_list_printer    = Extlib.IO.make_list_printer
    let lmargin              = Extlib.IO.lmargin
    type ('a, 'b, 'c) format = ('a, 'b, 'c) Pervasives.format

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
  
  module Marshal        = BatteriesGlue.BaselibGlue.Marshal
  module Oo             = BatteriesGlue.BaselibGlue.Oo
    
  (** {1 Interaction with other languages} *)
    
  module Callback       = BatteriesGlue.BaselibGlue.Callback
    
  (** {1 Memory}*)
    
  module Gc             = BatteriesGlue.BaselibGlue.Gc
  module Weak           = BatteriesGlue.BaselibGlue.Weak
    
  (** {1 Internals}
      Here Be Dragons*)
    
  module Obj            = BatteriesGlue.BaselibGlue.Obj
  module CamlinternalMod= BatteriesGlue.BaselibGlue.CamlinternalMod
  module CamlinternalOO = BatteriesGlue.BaselibGlue.CamlinternalOO
    
end

(*(** Automatically opened module. *) 
module Standard    = struct
  include BatteriesGlue.BaselibGlue.Pervasives
  include Extlib.Std
  let (@) = Extlib.ExtList.(@)
(* Thelema: Why not just include these in the toplevel batteries module? *)
(* Yoric: We also do that. Probably not a good idea to have both, though.*)
end*)

(** Interactions with the operating system (file manipulation, arguments...) *)
module System      = struct 
  
  (** {1 Environment I/O}*)
  
  module Arg           = BatteriesGlue.BaselibGlue.Arg
  module OptParse      = Extlib.OptParse
    
  (** {1 Operations on streams}*)
    
  module IO            = Extlib.IO
  module Unzip         = Extlib.Unzip
    
  (** {1 Actual operating system calls}*)
    
  module File          = Extlib.File
  module Filename      = BatteriesGlue.BaselibGlue.Filename
  module Unix          = BatteriesGlue.BaselibGlue.Unix
  module UnixLabels    = BatteriesGlue.BaselibGlue.UnixLabels
  module Sys           = BatteriesGlue.BaselibGlue.Sys
    
  (** {1 Networking}*)
  module Network       = struct
    (** Placeholder *)
  end
end

(** Tools for compiling OCaml, generating documentation, installing libraries. *)
module Toolchain   = struct
  
  (**Accessing information on source files from OCamlDoc*)
  (*module Odoc_info   = Batlib_Ocamldoc_Odoc_info -- removed for now*)
  
  (**Package management with Findlib*)
  module Findlib     = Batlib_Findlib_Findlib
end

(** Miscellaneous utilities *)
module Util        = struct
  module Base64 = Extlib.Base64
  module Digest = BatteriesGlue.BaselibGlue.Digest
  module Random = BatteriesGlue.BaselibGlue.Random
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


include BatteriesGlue.BaselibGlue.Pervasives
include Extlib.Enum
include Extlib.Std
let (@) = Extlib.ExtList.(@)
