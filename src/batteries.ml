include Batteries_uni (* the meat of this module is in batteries_uni.mli *)
module Mutex = BatMutex;;
module RMutex = BatRMutex;;

Unix.lock := Mutex.Mutex.make ();;
IO.lock := Mutex.Mutex.make ();;
IO.lock_factory := Mutex.Mutex.make;;
lock := Mutex.Mutex.make ();;
