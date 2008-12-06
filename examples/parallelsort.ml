open Threads, Event

let tasks = 5

let main = 
  let input      = Sys.argv in
  let input_len  = Array.length input in
  let channels   = Array.init tasks (fun _ -> new_channel ()) in
  let part_size  = input_len / tasks in
  let gen_part i = 
    let len = if i=tasks-1 then (input_len) - (i * part_size) else part_size in
    Array.sub input (i*part_size) len
  in
  let partitions   = Array.init tasks gen_part in
  let task (c,arr) = Array.sort compare arr; send c arr |> sync in
  let make_thread c arr = ignore (Thread.create task (c,arr)) in
  Array.iter2 make_thread channels partitions;
  let get_print c = c |> receive |> sync |> Array.iter print_endline in
  Array.iter get_print channels
