(*
 * Threadpool - Threading with bounded number of threads
 * Copyright (C) 2009 David Rajchenbach-Teller
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
open Event
open Cell

type thread_inner =
    {
      implementation: Thread.t option ref;(*We need the [option ref] for init purposes*)
      code:           (unit -> unit) ref;
      start:          unit Event.channel;
      iteration:      int ref
    }


type 'a thread = (thread_inner * 'a cell)

type t = {
  queue:     thread_inner Queue.t;
  mutex:     Mutex.t;
  condition: Condition.t
}

let log = ignore
(*  let mutex = Mutex.create () in
    fun s ->
      Mutex.lock mutex;
      Printf.fprintf stderr "[%i]%s\n%!" (Thread.id (Thread.self ())) s;
      Mutex.unlock mutex*)




(*The control loop of one thread*)
let in_thread_loop ~pool ~(thread:thread_inner) ~(send_result:'a -> unit) f x () : unit =
  log ("Entering thread loop "^ (string_of_int !(thread.iteration)));
  incr thread.iteration;
  (*Do the work and store the result*)
  let result = f x in
    log "Sending result";
    send_result result;
  log "Result sent";
  (*Return to the queue of ready threads*)
  Mutex.lock pool.mutex;
  if Queue.is_empty pool.queue then (*Here, we're the only available thread*)
    begin
      log "I'm the only available thread";
      log "Getting back on the thread queue";
      Queue.push thread pool.queue;
      Condition.signal pool.condition(*Tell the others, there's someone ready*)
    end
  else
    begin
      log "Getting back on the thread queue";
      Queue.push thread pool.queue
    end;
  Mutex.unlock pool.mutex;
  (*Wait until some more work is available.*)
  log "Waiting for work";
  sync (receive thread.start);
  (*Loop*)
  log "Resume work";
  (fun () -> !(thread.code) ()) ()

let create size =
  if size <= 0 then raise (Invalid_argument "ThreadPool.create");
  let pool =
    {
      mutex     = Mutex.create ();
      condition = Condition.create ();
      queue     = Queue.create ()
    }
  in
  let create_one_thread i =
    log ("Creating outer thread "^(string_of_int i));
    let thread =  {
      implementation = ref None;
      code           = ref ignore;
      start          = Event.new_channel ();
      iteration      = ref 0
    } in
    let thread_fun : unit -> unit = in_thread_loop ~pool ~thread ~send_result:ignore ignore () in
      thread.implementation := Some (Thread.create thread_fun ())
  in for i = 1 to size do create_one_thread i done;
    log "Threads created";
    Mutex.lock pool.mutex;
    log "Starting pool";
    Condition.signal pool.condition;
    Mutex.unlock pool.mutex;
    log "Pool started";
    pool
      



let spawn t f x =
  log "Spawning a task";
  Mutex.lock t.mutex; (*Wait for a thread to be available*)
  while Queue.is_empty t.queue do
    log "Waiting for an inner thread to be available";
    Condition.wait t.condition t.mutex 
  done;
  let thread = Queue.pop t.queue 
  and cell   = Cell.make () in
    if (Queue.is_empty t.queue) then 
	log "I'm taking the last one"
    else
      begin
	log "I'm not taking the last one, letting the others work";
	Condition.signal t.condition;(*Queue not empty, let the next one use it*)
      end;
    thread.code := in_thread_loop ~pool:t ~thread 
      ~send_result:(fun x -> Cell.post cell x) f x;              (*Prepare code*)
    log "Preparing to launch the task";
    Mutex.unlock t.mutex;
    log "Launching the task";
    sync (send thread.start ());                                   (*Start*)
    log "Task launched";
    (thread, cell)

let join (thread, result) =
  match !(thread.implementation) with 
      None -> assert false
    | Some t ->
	log ("Waiting for result of thread "^(string_of_int (Thread.id t)));
	Cell.get result
