(*
 * Batlib_Findlib_Findlib - Importing Findlib module Findlib
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

include Findlib

let list_packages ?tab ?descr out =
  (*Read to a temporary file*)
  let (file_name, cout) = Filename.open_temp_file "ocaml" "tmp" in
    list_packages ?tab ?descr cout;
    close_out cout;
    (*Extract contents of temporary file *)
    File.with_file_in file_name (fun inp -> IO.copy inp out);
    (*Clean-up*)
    Sys.remove file_name


