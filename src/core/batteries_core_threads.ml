(*
 * BatteriesCore - The core of Batteries Included (threaded version)
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

(** Threaded version of Batteries Core. *)

(*Note: for the moment, we need to copy the whole module structure.
  This is due to a limitation of ocamldoc.

  In a future version, we should be able to get away with something
  simpler -- and faster to ocamldoc-ify.*)

(*[Control] is [Batteries_core.Control] + [Control.Concurrency.Threads] *)
module Control = struct
  module Concurrency = struct
    module Thread = struct
      module Condition = Batlib_Baselib_Condition
      module Event     = Batlib_Baselib_Event
      module Mutex     = Batlib_Baselib_Mutex
      module Thread    = Batlib_Baselib_Thread
    end
  end

  module Exceptions    = Batteries_core.Control.Exceptions
  module Labels = Batteries_core.Control.Labels
  module Monad  = Batteries_core.Control.Monad

end

(*[Data] is [Batteries_core.Data]*)
module Data      = 
struct
  
  module Mutable         = struct
    module Array         = Batteries_core.Data.Mutable.Array
    module ArrayLabels   = Batteries_core.Data.Mutable.ArrayLabels
    module Bigarray      = Batteries_core.Data.Mutable.Bigarray
    module Dllist        = Batteries_core.Data.Mutable.Dllist
    module Dynarray      = Batteries_core.Data.Mutable.Dynarray
    module Enum          = Batteries_core.Data.Mutable.Enum
    module Global        = Batteries_core.Data.Mutable.Global
    module Hashtbl       = Batteries_core.Data.Mutable.Hashtbl
    module HashtblLabels = Batteries_core.Data.Mutable.HashtblLabels
    module Queue         = Batteries_core.Data.Mutable.Queue
    module Ref           = Batteries_core.Data.Mutable.Ref
    module RefList       = Batteries_core.Data.Mutable.RefList
    module Stack         = Batteries_core.Data.Mutable.Stack
    module Stream        = Batteries_core.Data.Mutable.Stream
  end

  module Persistent      = struct
    module Lazy            = Batteries_core.Data.Persistent.Lazy
    module List            = Batteries_core.Data.Persistent.List
    module ListLabels      = Batteries_core.Data.Persistent.ListLabels
    module Map             = Batteries_core.Data.Persistent.Map
    module MapLabels       = Batteries_core.Data.Persistent.MapLabels
    module MultiPMap       = Batteries_core.Data.Persistent.MultiPMap
    module PMap            = Batteries_core.Data.Persistent.PMap
    module PSet            = Batteries_core.Data.Persistent.PSet
    module Option          = Batteries_core.Data.Persistent.Option
    module OptionLabels    = Batteries_core.Data.Persistent.OptionLabels
    module Set             = Batteries_core.Data.Persistent.Set
    module SetLabels       = Batteries_core.Data.Persistent.SetLabels
  end
    
  (** Boolean and bit-oriented data structures *)
  module Logical     = struct
    module BitSet = Batteries_core.Data.Logical.BitSet
    module Bool   = Batteries_core.Data.Logical.Bool
  end

  (** Numbers and operations on them.*)    
  module Numeric     = struct
    module Common      = Batteries_core.Data.Numeric.Common
    module Big_int     = Batteries_core.Data.Numeric.Big_int
    module Complex     = Batteries_core.Data.Numeric.Complex
    module Float       = Batteries_core.Data.Numeric.Float
    module Int         = Batteries_core.Data.Numeric.Int
    module Int32       = Batteries_core.Data.Numeric.Int32
    module Int64       = Batteries_core.Data.Numeric.Int64
    module Native_int  = Batteries_core.Data.Numeric.Native_int
    module Num         = Batteries_core.Data.Numeric.Num
    module Safe_int    = Batteries_core.Data.Numeric.Safe_int
    module Unit        = Batteries_core.Data.Numeric.Unit
  end
    
  (** Text data structures. *)
  module Text        = struct
    
    (** {6 Latin-1}*)

    module Buffer          = Batteries_core.Data.Text.Buffer
    module Char            = Batteries_core.Data.Text.Char
    module String          = Batteries_core.Data.Text.String
    module StringLabels    = Batteries_core.Data.Text.StringLabels

    (** {6 Unicode}*)
    module Rope            = Batteries_core.Data.Text.Rope
    module UChar           = Batteries_core.Data.Text.UChar
    module UTF8            = Batteries_core.Data.Text.UTF8
  end
end

(* [Languages] is [Batteries_core.Languages] *)
module Languages = struct
  
  (** {1 Parsing} *)
  
  module Genlex          = Batteries_core.Languages.Genlex
  module Lexing          = Batteries_core.Languages.Lexing
  module Parsing         = Batteries_core.Languages.Parsing
  module Scanf           = Batteries_core.Languages.Scanf
  module Str             = Batteries_core.Languages.Str

  (** {2 Parser combinator library}*)

  module CharParser      = Batteries_core.Languages.CharParser
  module ParserCo        = Batteries_core.Languages.ParserCo

  (** {1 Printing}*)
    
  module Format          = Batteries_core.Languages.Format
  module Printf          = Batteries_core.Languages.Printf
    
  (** {1 Serialization to human-readable formats}
      XML, JSON, S-Expressions ...*)

  module SExpr           = Batteries_core.Languages.SExpr

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


(*[Meta] is [Batteries_core.Meta]*)
module Meta      = 
struct    
  (** {1 Language}*)
  
  module Marshal        = Batteries_core.Meta.Marshal
  module Oo             = Batteries_core.Meta.Oo
    
  (** {1 Interaction with other languages} *)
    
  module Callback       = Batteries_core.Meta.Callback
    
  (** {1 Memory}*)
    
  module Gc             = Batteries_core.Meta.Gc
  module Weak           = Batteries_core.Meta.Weak
    
  (** {1 Internals}
      Here Be Dragons*)
    
  module Obj            = Batteries_core.Meta.Obj
  module CamlinternalMod= Batteries_core.Meta.CamlinternalMod
  module CamlinternalOO = Batteries_core.Meta.CamlinternalOO
    
end

(*[System] is [Batteries_core.System]*)
module System    = 
struct
    
  (** {1 Environment I/O}*)
  
  module Arg           = Batteries_core.System.Arg
  module OptParse      = Batteries_core.System.OptParse
    
  (** {1 Operations on streams}*)
    
  module IO            = Batteries_core.System.IO
  (* module Unzip         = Batteries_core.System.Unzip *)
    
  (** {1 Actual operating system calls}*)
    
  module File          = Batteries_core.System.File
  module Filename      = Batteries_core.System.Filename
  module Unix          = Extlib_threads.ExtUnixThreads.Unix
  module UnixLabels    = Batteries_core.System.UnixLabels
  module Sys           = Batteries_core.System.Sys
    
  module Network       = Batteries_core.System.Network
end

(*[Toolchain] is [Batteries_core.Toolchain]*)
module Toolchain = struct

  module Sysconfig     = Batteries_core.Toolchain.Sysconfig
  module Execute       = Batteries_core.Toolchain.Execute
  module Findlib       = Batteries_core.Toolchain.Findlib
end

(*[Util] is [Batteries_core.Util]*)
module Util      = struct
  module Base64 = Batteries_core.Util.Base64
  module Digest = Batteries_core.Util.Digest
  module Random = Batteries_core.Util.Random
end

module Standard  = Batteries_core.Standard

module Legacy    = struct
  module Condition = Batlib_Baselib_Condition
  module Event     = Batlib_Baselib_Event
  module Mutex     = Batlib_Baselib_Mutex
  module Thread    = Batlib_Baselib_Thread
  include Batteries_core.Legacy
end

