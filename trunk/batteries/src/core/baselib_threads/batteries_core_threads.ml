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

(*Firstly, add threads to [Control.Concurrency.Threads]*)
module Control = struct
  module Concurrency = struct
    module Thread = struct
      module Condition = Batlib_Baselib_Condition
      module Event     = Batlib_Baselib_Event
      module Mutex     = Batlib_Baselib_Mutex
      module Thread    = Batlib_Baselib_Thread
    end
  end

  module Labels = Batteries_core.Control.Labels
  module Monad  = Batteries_core.Control.Monad
end

open Batteries_core

module Data      = Data
module Languages = Languages
module Meta      = Meta
module System    = System
module Toolchain = Toolchain
module Util      = Util
module Standard  = Standard
module Legacy    = struct
  include Legacy
  module Condition = Batlib_Baselib_Condition
  module Event     = Batlib_Baselib_Event
  module Mutex     = Batlib_Baselib_Mutex
  module Thread    = Batlib_Baselib_Thread
end

