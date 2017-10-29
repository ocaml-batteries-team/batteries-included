(* This compatible module contains compatibility versions of stdlib
   functions that are commonly used when porting code to the
   (string / bytes) separation, but are not available in older OCaml
   versions that Batteries support.

   We could push each function in the corresponding Batteries module
   (Buffer.add_subbtypes into BatBuffer, etc.), but this would have
   the effect of turning dependencies on the stdlib into
   inter-Batteries-module dependencies: any module using
   Buffer.add_subbtypes would then depend on the whole BatBuffer,
   increasing binary sizes and risk of cycles.
*)

##V>=4.2##let string_init = String.init
##V<4.2##let string_init len f =
##V<4.2##  let s = Bytes.create len in
##V<4.2##  for i = 0 to len - 1 do
##V<4.2##    Bytes.unsafe_set s i (f i)
##V<4.2##  done;
##V<4.2##  Bytes.unsafe_to_string s

##V>=4.2##let buffer_add_subbytes = Buffer.add_subbytes
##V<4.2##let buffer_add_subbytes = Buffer.add_substring

##V>=4.2##let buffer_to_bytes = Buffer.to_bytes
##V<4.2##let buffer_to_bytes = Buffer.contents
