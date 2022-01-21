(*
 * BatFilename - Extended Filename module
 * Copyright (C) 1996 Xavier Leroy
 *               2008 David Teller, LIFO, Universite d'Orleans
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

include Filename

##V<4.4## let is_dir_sep name i =
##V<4.4##   try
##V<4.4##     for j = 0 to String.length dir_sep - 1 do
##V<4.4##       if i + j >= String.length name ||
##V<4.4##          name.[i + j] != dir_sep.[j] then raise Exit
##V<4.4##     done;
##V<4.4##     true
##V<4.4##   with Exit ->
##V<4.4##     false
##V<4.4##
##V<4.4## let extension_len name =
##V<4.4##   let rec check i0 i =
##V<4.4##     if i < 0 || is_dir_sep name i then 0
##V<4.4##     else if name.[i] = '.' then check i0 (i - 1)
##V<4.4##     else String.length name - i0
##V<4.4##   in
##V<4.4##   let rec search_dot i =
##V<4.4##     if i < 0 || is_dir_sep name i then 0
##V<4.4##     else if name.[i] = '.' then check i (i - 1)
##V<4.4##     else search_dot (i - 1)
##V<4.4##   in
##V<4.4##   search_dot (String.length name - 1)
##V<4.4##
##V<4.4## let remove_extension name =
##V<4.4##   let l = extension_len name in
##V<4.4##   if l = 0 then name else String.sub name 0 (String.length name - l)
##V<4.4##
##V<4.4## let extension name =
##V<4.4##   let l = extension_len name in
##V<4.4##   if l = 0 then "" else String.sub name (String.length name - l) l

let split_extension s =
  remove_extension s, extension s

(*$= split_extension & ~printer:(IO.to_string (Tuple2.print String.print String.print))
  ("/foo/bar", ".baz") (split_extension "/foo/bar.baz")
  ("/foo/bar", "")    (split_extension "/foo/bar")
  ("/foo/bar", ".")   (split_extension "/foo/bar.")
  ("/foo/.rc", "")    (split_extension "/foo/.rc")
  ("", "")            (split_extension "")
*)

let with_temp_file ?temp_dir prfx sfx f =
  let tmp_fn = Filename.temp_file ?temp_dir prfx sfx in
  BatPervasives.finally (fun () -> Sys.remove tmp_fn) f tmp_fn

(* the temp file is expected to exist during f's lifetime, but not after *)
(*$T with_temp_file
  let fn = with_temp_file "batFilename_with_temp_file" ".tmp" \
      (fun fn -> assert(Sys.file_exists fn); fn) in \
  not (Sys.file_exists fn)
*)
