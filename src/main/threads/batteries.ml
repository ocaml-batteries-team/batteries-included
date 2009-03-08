(*
 * Batteries - The complete Batteries Included (threaded version)
 * Copyright (C) 2009 David Rajchenbach-Teller, LIFO, Universite d'Orleans
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

(**
   The libraries provided as part of OCaml Batteries Included

   @author David Rajchenbach-Teller
*)

(**
   Automatically opened module.

   @topic IO
   @topic Printing
   @topic Files

*)
module Standard = Extlib.ExtPervasives.Pervasives

(** Access to the modules provided by INRIA. 

    For more information, see the documentation of OCaml.

    @topic Legacy
*)
module Legacy = struct
  (**/**)
  module Array     = Array
  module ArrayLabels= ArrayLabels
  module Bigarray  = Bigarray
  module Digest    = Digest
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


        
(**
   {3 Control}

   {topic Control}
*)

module  Exceptions= Extlib.ExtPrintexc.Printexc
module  Return    = Extlib.Return
module  Monad     = Extlib.Monad

(**
   {4 Concurrency}

   {topic Concurrency}
*)

module  Concurrency = Extlib.Concurrent

(**
   {5 Built-in threads}

   These modules are only defined in multi-threaded versions of OCaml Batteries
   Included. To use a multi-threaded version, please see {{:???}the documentation}.
*)

module  Condition = Batlib_Baselib_Condition
module  Event     = Batlib_Baselib_Event
module  Mutex     = Extlib_threads.ExtMutex.Mutex
module  RMutex    = Extlib_threads.RMutex
module  Thread    = Batlib_Baselib_Thread

(**
   All the definitions of built-in concurrency primitives.

   These definitions are grouped as one module to allow simple replacement of these
   primitives by other implementations of concurrency, such as coThreads.

   @inline none
*)
module  Threads   =
struct
  module Condition = Condition
  module Event     = Event
  module Mutex     = Mutex
  module RMutex    = RMutex
  module Thread    = Thread
end

(*
  {5 coThreads}

  Not implemented yet.

module  CoCondition = CoThread.Condition
module  CoEvent     = CoThread.Event
module  CoMutex     = CoThread.Mutex
module  CoRMutex    = CoThread.RMutex
module  CoThread    = CoThread.Thread
module  Threads   =
struct
  module Condition = CoCondition
  module Event     = CoEvent
  module Mutex     = CoMutex
  module RMutex    = CoRMutex
  module Thread    = CoThread
end
*)

(*
  {5 Shared memory}
  Not implemented yet
*)

(**{3 Input/Output}

   {topic IO}*)

module  IO = Extlib.IO

(** @topic Network*)
module Net_channels = Libs.ExtNetchannels.Netchannels



(**{4 Compression / decompression}

   {topic Compression}
   {topic Decompression}
*)

module  Codec = Libs.Common.Compress
module  GZip  = Libs.GZip
(*
module  Bz2
module  Zip
module  Transcode  (*Unicode transcoding*)
*)

(**{3 Data containers}

   {topic Data}
   {topic Container}*)

module Data = Extlib.Interfaces

(**
   {4 Mutable data containers}

   {topic Mutable}
*)

module  Array    = Extlib.ExtArray.Array
module  Big_array= Extlib.ExtBigarray.Bigarray
(**/**)
(**For compatibility purposes with the base library*)
module  Bigarray = Big_array
(**/**)
module  Dllist   = Extlib.Dllist
module  Dyn_array= Extlib.DynArray
(**/**)
(**For compatibility purposes with Extlib*)
module  DynArray  = Dyn_array
(**/**)
module  Enum     = Extlib.Enum
module  Global   = Extlib.Global
module  Hashtbl  = Extlib.ExtHashtbl.Hashtbl
module  Ref_list = Extlib.RefList 
(**/**)
(**For compatibility purposes with Extlib*)
module  RefList  = Ref_list
(**/**)

module  Queue    = Extlib.ExtQueue.Queue
module  Ref      = Extlib.Ref
module  Stack    = Extlib.ExtStack.Stack
module  Stream   = Extlib.ExtStream.Stream
module  Vect     = Extlib.Vect
  
(**
   {4 Persistent data containers}

   {topic Persistent}
*)      

module  Lazy     = Batlib_Baselib_Lazy
module  Lazy_list= Extlib.LazyList
module  List     = Extlib.ExtList.List
module  Map      = Extlib.ExtMap.Map
module  Multi_pmap= Extlib.MultiPMap
module  Option   = Extlib.Option
module  PMap     = Extlib.PMap
module  PSet     = Extlib.PSet
module  Set      = Extlib.ExtSet.Set

(**{3 Data}

   {topic Data}
*)

module  Unit     = Extlib.ExtUnit.Unit

(**{4 Logical data}

   {topic Logical}
   {topic Boolean}
*)

module  Bool     = Extlib.ExtBool.Bool
module  Bit_set  = Extlib.BitSet
(**/**)
(**For compatibility purposes with Extlib*)
module  BitSet  = Bit_set
(**/**)

(**{4 Numeric data}

   {topic Numeric}
*)

module  Numeric  = Extlib.Number 
module  Big_int  = Extlib.ExtBig_int.Big_int
module  Complex  = Extlib.ExtComplex.Complex
module  Float    = Extlib.ExtFloat.Float
module  Int      = Extlib.ExtInt.Int
module  Int32    = Extlib.ExtInt32.Int32
module  Int64    = Extlib.ExtInt64.Int64
module  Native_int=Extlib.ExtNativeint.Native_int
(**/**)
(**For compatibility purposes with the base library*)
module  Nativeint = Native_int
(**/**)
module  Num      = Extlib.ExtNum.Num
module  Safe_float= Extlib.ExtFloat.Safe_float
module  Safe_int = Extlib.ExtInt.Safe_int

(**{4 Textual data}

   {topic Textual}*)


(*module  Text (*Definition of text-related interfaces*)*)
module  Buffer  = Extlib.ExtBuffer.Buffer
module  Char    = Extlib.ExtChar.Char
module  UTF8    = Extlib.ExtUTF8.UTF8
module  Rope    = Extlib.Rope
module  UChar   = Extlib.ExtUChar.UChar
module  String  = Extlib.ExtString.String
module  Str     = Extlib.ExtStr.Str
(*module  StringText (A module containing aliases to String and modified   Char)*)
(*module  RopeText (As StringText but with implementations from Rope and  UChar)*)
(*module  UTF8Text (As StringText but with implementations from UTF8 and UChar)*)
module Char_encodings = Extlib.CharEncodings

(**{3 Tools included in the distribution}

   {topic Distribution}
*)

(**{4 External tools}

   {topic Externals}
*)


module  Packages = Libs.ExtFindlib.Findlib
module  Compilers= Libs.Compilers

(**{4 Language internals}

   Here be dragons.

   {topic Internals}
*)

module  Callback = Batlib_Baselib_Callback
module  Gc       = Extlib.ExtGc.Gc
module  Marshal  = Extlib.ExtMarshal.Marshal
module  Modules  = Batlib_Baselib_CamlinternalMod
module  Obj      = Batlib_Baselib_Obj
module  Oo       = Extlib.ExtOo.Oo
module  Weak     = Batlib_Baselib_Weak

         
(*
              =====module  Network (*placeholders*) =====
module  URL
module  Netencoding 
module  Base64
module  QuotedPrintable
module  Q
module  URL
module  Html
         
              ====module module  Http ====
module  Http
module  Http_client
module  Cgi_*
module  Httpd_*
module  MIME  
              ====module module  Ftp ====
module  Ftp_client  
              ====module module  Mail ====
module  Netmail
module  Pop
module  Sendmail
module  Smtp  
              ====module module  Generic server ====
module  Netplex_*  
              ====module module  RPC ====
module  Rpc_*  
              ====module module  Languages ====*)


module  Lexing = Batlib_Baselib_Lexing
module  Parsing= Batlib_Baselib_Parsing
module  Format = Extlib.ExtFormat.Format
module  Printf = Extlib.ExtPrintf.Printf
module  Print  = Extlib.Print
(* module  PCRE (*placeholder*)*)
module  Scanf  = Extlib.ExtScanf.Scanf
module  SExpr  = Libs.ExtSexp_Conv.Sexp_conv


(**{3 Operations on the system}

   {topic System}
*)

module Arg      = Extlib.ExtArg.Arg
module File     = Extlib.File
module Opt_parse= Extlib.OptParse
(**/**)
(**For compatibility purposes with the base library*)
module  OptParse  = Opt_parse
(**/**)
module Path     = Batlib_Baselib_Filename
module Shell    = Extlib.ExtSys.Sys
module Unix     = Extlib.ExtUnix.Unix
(*module  Equeue:placeholder*)


(**{3 Unclassified}

   {topic Unclassified}
*)

module Base64           = Extlib.Base64
module Batteries_config = Batteries_config
module MD5              = Extlib.ExtDigest.Digest
module Random           = Extlib.ExtRandom.Random
module Date             = Libs.ExtNetdate.Netdate


(**
   Preview of future modules.

   @topic Future
*)
module  Future =
struct
(*  module Lexers = Extlib.ExtGenlex.Languages*)
  module Genlex      = Extlib.ExtGenlex.Genlex
  module Char_parser  = Extlib.CharParser
  module UChar_parser = Extlib.UCharParser
  module Parser_co    = Extlib.ParserCo
  module Path        = Extlib.PathGen.OfString
  module PathGen     = Extlib.PathGen
  module Result      = Extlib.Result
  module Logger      = Extlib.Logger
end
