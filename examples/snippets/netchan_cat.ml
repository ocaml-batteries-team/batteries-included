(* Yet another (slower) "cat" implementation, it is just meant to be a
   showcase for integration with ocamlnet's Netchannels. *)

let oc =
  Netchannels.lift_out
    (`Rec (new Netchannels.channel_of_output IO.stdout :>
	     Netchannels.rec_out_channel))
let _ =
  Netchannels.with_in_obj_channel
    (Netchannels.lift_in (`Rec (new Netchannels.channel_of_input IO.stdin)))
    (fun ic ->
       try
	 while true do
	   oc # output_string (ic # input_line () ^ "\n");
	   oc # flush ()
	 done
       with End_of_file -> ())

