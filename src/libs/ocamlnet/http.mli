(* Batteries Included - Http
 *
 * Copyright (C) 2009 David Rajchenbach-Teller
 * 
 * This library is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation; either version 2.1 of the
 * License, or (at your option) any later version, with the special
 * exception on linking described in file LICENSE.
 * 
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301
 * USA
 *)

(**
   Http client code.

   Use this module to send or download web messages/pages/etc. from a client application.

   @author David Teller
*)


open Extlib.IO

(**
   {6 Simple interface}
*)

(**
   Download the contents at a http url.

   Use [document_at uri] to download the contents of uri. The url must be valid and start
   with "http://". By default, documents are not cached. If the resource requires authentification,
   the url may contain user and password information.
*)
val document_at: string -> input


(**
   {6 Detailed interface}
*)
type param =
{
  arg_name:     string;
  arg_charset:  string;
  arg_value:    string;
  arg_language: string
} 
type mime_type=
{
  type_main: string;
  type_args: param list
} 
type mime_disposition =
{
  disposition_main: string;
  disposition_args: param list
} 

type response =
{
  http_method:                string;
  uri:                        string;
  content_length:             int option;
  content_type:               mime_type option;
  content_disposition:        mime_disposition option;
  content_transfer_encoding:  string option;
  header_fields:              (string * string) list;
  content:                    unit -> input(**The contents of the response.
					      This content is automatically decoded.*)
}


(**
   Send a GET request
   
   Usage: [get uri]
*)
val get: string -> response

(**
   Send a [PUT] request.

   Usage: [put uri content].
*)
val put:string -> string -> response

(**
   Send a [POST] request.

   Usage: [post uri name_value_list].
*)
val post: string -> (string * string) list -> response


(**
   Send a [OPTIONS] request.

   [OPTIONS] request represent requests for information regarding the communication facilities
   available on the server.

   Usage: [options uri].
*)
val options: string -> response

