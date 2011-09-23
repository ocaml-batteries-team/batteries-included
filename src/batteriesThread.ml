module Mutex = BatMutex
module RMutex = BatRMutex

let () =
  BatUnix.lock := RMutex.make ();
  BatIO.lock := RMutex.make ();
  BatIO.lock_factory := RMutex.make;
  BatPervasives.lock := RMutex.make ();
  ()
