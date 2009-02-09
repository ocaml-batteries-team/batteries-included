open Testing

let tests =
  [ (Test_digest.name, Test_digest.test) ];;

init ();;

List.iter 
    (fun (name, go) -> 
       try  result name (go ())
       with e -> result name (Err (Printf.sprintf "Cannot run test:%s\n" (Printexc.to_string e))))
tests;;
    

finish ()
