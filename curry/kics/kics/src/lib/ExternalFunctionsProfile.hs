module ExternalFunctionsProfile (module ExternalFunctionsProfile) where

import Curry
import CurryPrelude hiding (return)
import InstancesProfile

import System.Mem (performGC)
import System.CPUTime

getProcessInfos :: State -> C_IO (List (T2 C_ProcessInfo C_Int))
getProcessInfos _ = C_IO (\_-> do
  t <- getCPUTime 
  return (IOVal (T2 C_RunTime (toCurry (div t (10^9))) :< List)))

garbageCollectorOff :: State -> C_IO T0
garbageCollectorOff  = ioFunc0 (return ())

garbageCollectorOn :: State -> C_IO T0
garbageCollectorOn  = ioFunc0 (return ())

garbageCollect :: State -> C_IO T0
garbageCollect  = ioFunc0 performGC




