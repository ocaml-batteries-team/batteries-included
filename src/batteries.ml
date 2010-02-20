include Batteries_uni
module Mutex = BatMutex;;
module RMutex = BatRMutex;;

Unix.lock := Mutex.Mutex.make ();;
IO.lock := Mutex.Mutex.make ();;
IO.lock_factory := Mutex.Mutex.make;;
lock := Mutex.Mutex.make ();;
