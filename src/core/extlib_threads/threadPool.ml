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
open ExtMutex
open Cell
open Event


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
  condition: Condition.t;
  size:      int
}

let size {size = s} = s

let available t =
  Mutex.lock t.mutex;
  let result = t.size - (Queue.length t.queue) in
    Mutex.unlock t.mutex;
    result

let with_available t (f:int -> 'a) =
  Mutex.synchronize ~lock:t.mutex f (t.size - (Queue.length t.queue))
  

let log = 
  let mutex = Mutex.create () in
    fun s ->
      Mutex.lock mutex;
      Printf.fprintf stderr "[%i]%s\n%!" (Thread.id (Thread.self ())) s;
      flush_all ();
      Mutex.unlock mutex




      
(*The control loop of one thread*)
let in_thread_loop 
    ?(work_load: ('a cell * ('b -> 'a) * 'b) option)
    ~pool 
    ~(thread:thread_inner) 
    () : unit = 
  begin
    match work_load with
      | Some (cell, f, x) ->
	  begin
	    log ("Entering thread loop "^ (string_of_int !(thread.iteration)));
	    incr thread.iteration;
	    (*Do the work and store the result*)
	    try
	      let result = f x in
		log "Sending result";
		Cell.post cell result
	    with e -> 
	      begin
		log "Sending failure";
		Cell.fail cell e
	      end;
	      log "Result sent"
	  end
      | None -> log "Initializing thread loop"
	  (*Return to the queue of ready threads*)
  end;
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
      queue     = Queue.create ();
      size      = size
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
    let thread_fun : unit -> unit = fun () -> in_thread_loop ~pool ~thread () in
      thread.implementation := Some (Thread.create thread_fun ())
  in for i = 1 to size do create_one_thread i done;
    log "Threads created";
    Mutex.lock pool.mutex;
    log "Starting pool";
    Condition.signal pool.condition;
    Mutex.unlock pool.mutex;
    log "Pool started";
    pool
      
let spawn_immediately t f x =
  Mutex.lock t.mutex;
  if Queue.is_empty t.queue then None
  else
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
      ~work_load:(cell, f, x);
    log "Preparing to launch the task";
    Mutex.unlock t.mutex;
    log "Launching the task";
    sync (send thread.start ());                                   (*Start*)
    log "Task launched";
    Some (thread, cell)

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
      ~work_load:(cell, f, x);
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
