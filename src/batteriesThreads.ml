include Batteries (* the meat of this module is in batteries.ml *)
module Mutex = BatMutex;;
module RMutex = BatRMutex;;

Unix.lock := RMutex.make ();;
IO.lock := RMutex.make ();;
IO.lock_factory := RMutex.make;;
lock := RMutex.make ();;
