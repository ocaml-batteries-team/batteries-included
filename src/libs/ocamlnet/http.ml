TYPE_CONV_PATH "Http"

open Extlib.IO
open Sexplib
open Http_client.Convenience
open ExtNetchannels.Netchannels

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
  content:                    unit -> input
} 

let response_of_call (call: #Http_client.http_call) =
begin
  let param_of_received_param (key, param) = 
    { arg_name    = key;
      arg_charset = Mimestring.param_charset param;
      arg_value   = Mimestring.param_value param;
      arg_language= Mimestring.param_language param } in
    prerr_endline "response_of_call 1.1";
  let headers = call#request_header `Effective in
  let content_length = try Some (headers#content_length ()) with _ -> None in
  let content_type   = 
    try
    let (ct_main, ctargs) = headers#content_type () in
      Some {
	type_main = ct_main;
	type_args = List.map param_of_received_param ctargs
      }
    with _ -> None
  and content_disposition   = 
    try
    let (cd_main, cdargs) = headers#content_disposition () in
      Some {
	disposition_main = cd_main;
	disposition_args = List.map param_of_received_param cdargs
      }
    with _ -> None
  and content_transfer_encoding = try Some (headers#content_transfer_encoding ()) with _ -> None
  and fields                    = headers#fields
  and http_method               = call#request_method
  and uri                       = call#request_uri in
  let c = (call#response_body#open_value_rd () :> Netchannels.rec_in_channel) in
  let content                   = fun () -> input_of_channel c
  in
  {
    http_method               = http_method;
    uri                       = uri;
    content_length            = content_length;
    content_type              = content_type;
    content_disposition       = content_disposition;
    content_transfer_encoding = content_transfer_encoding;
    header_fields             = fields;
    content                   = content
  }
end
let get uri =
  response_of_call (http_get_message uri)

let post uri content =
  response_of_call (http_post_message uri content)

let put uri content =
  response_of_call (http_put_message uri content)

let options uri =
  response_of_call (new Http_client.options uri)

let document_at uri =
  (get uri).content ()
