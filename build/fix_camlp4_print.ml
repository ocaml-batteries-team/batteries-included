open Format
open Camlp4

let pp = fprintf

module Id = struct
  let name = "fix for Camlp4.Printers.OCaml"
  let version = "0.1"
end

module Make (Syntax : Sig.Camlp4Syntax) = struct
  open Syntax
  include Camlp4.Printers.OCaml.Make(Syntax)

  module CommentFilter = Struct.CommentFilter.Make(Token);;
  let comment_filter = CommentFilter.mk ();;
  CommentFilter.define (Gram.get_filter ()) comment_filter;;

  class extprinter ?curry_constr ?comments () =
  object (self)
    inherit printer ?curry_constr ?comments () as super

    method print_comments_before loc f =
      let rec aux __strm =  match Stream.peek __strm with
	| Some ((comm, comm_loc)) when Loc.strictly_before comm_loc loc ->
            (Stream.junk __strm;
             let () = pp f "%s@ \n" comm in aux __strm)
	| _ -> ()
      in
	aux (CommentFilter.take_stream comment_filter)

    method flush_rest_of_comments f =
      Stream.iter (fun (comm, _) -> pp f "%s@ \n" comm) (CommentFilter.take_stream comment_filter)


    method sig_item f sg =
      match sg with
	| Ast.SgVal (_, s, t) ->
	     self#node f sg Ast.loc_of_sig_item;
	     pp f "%s %a :%a"
               value_val self#var s self#ctyp t;
	| _ -> super#sig_item f sg

    method interf f sg =
      pp f "@[<v0>%a@]@." self#sig_item sg;
      self#flush_rest_of_comments f

(*      Stream.iter (fun (text, lloc) ->
		     Printf.eprintf "\n%S\n" text
		  )
	(CommentFilter.take_stream comment_filter);
      super#print_comments_before loc f*)
(*      Stream.iter (fun (text, lloc) -> pp f "%s@ zz" text) (CommentFilter.take_stream comment_filter)*)
(*      pp_open_box f 10;*)
(*      super#print_comments_before loc f;
      pp_print_newline f ();
      pp_print_string  f "(*after comment*)"*)
(*      pp_close_box f ()*)

(*      pp_print_newline f ()*)
(*      pp_print_string  f "(*after comment*)"*)


  end

  let print output_file fct =
    let o = new extprinter () in
      with_outfile output_file (fct o)

  let print_interf ?input_file:(_) ?output_file sg =
    print output_file (fun o -> o#interf) sg

  let print_implem ?input_file:(_) ?output_file st =
    print output_file (fun o -> o#implem) st

end;;




module MakeMore (Syntax : Sig.Camlp4Syntax)= struct

  include Make(Syntax);;

  let semisep : sep ref = ref ("@\n":sep);;
  let margin = ref 78;;
  let comments = ref true;;
  let locations = ref false;;
  let curry_constr = ref false;;

  let print output_file fct =
    let o = new extprinter ~comments:!comments
                        ~curry_constr:!curry_constr () in
    let o = o#set_semisep !semisep in
    let o = if !locations then o#set_loc_and_comments else o in
    with_outfile output_file
      (fun f ->
        let () = Format.pp_set_margin f !margin in
        Format.fprintf f "@[<v0>%a@]@." (fct o));;

  let print_interf ?input_file:(_) ?output_file sg =
    print output_file (fun o -> o#interf) sg;;

  let print_implem ?input_file:(_) ?output_file st =
    print output_file (fun o -> o#implem) st;;

  let check_sep s =
    if String.contains s '%' then failwith "-sep Format error, % found in string"
    else (Obj.magic (Struct.Token.Eval.string s : string) : sep);;

  Options.add "-l" (Arg.Int (fun i -> margin := i))
    "<length> line length for pretty printing.";;

  Options.add "-ss" (Arg.Unit (fun () -> semisep := ";;;;"))
    " Print double semicolons.";;

  Options.add "-no_ss" (Arg.Unit (fun () -> semisep := ""))
    " Do not print double semicolons (default).";;

  Options.add "-sep" (Arg.String (fun s -> semisep := check_sep s))
    " Use this string between phrases.";;

  Options.add "-curry-constr" (Arg.Set curry_constr) "Use currified constructors.";;

  Options.add "-no_comments" (Arg.Clear comments) "Do not add comments.";;

  Options.add "-add_locations" (Arg.Set locations) "Add locations as comment.";;

end;;


module M = Camlp4.Register.OCamlPrinter(Id)(MakeMore);;
