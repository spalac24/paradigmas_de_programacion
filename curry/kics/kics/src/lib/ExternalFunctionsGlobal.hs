module ExternalFunctionsGlobal (module ExternalFunctionsGlobal) where

import Curry
import CurryPrelude
import ExternalFunctionsIOExts
import InstancesGlobal

import Data.IORef
import System.IO.Unsafe


global :: (Curry t0) => t0 -> C_GlobalSpec -> Result (C_Global t0)
global x spec = ref `seq` (\ _ -> PrimValue ref)
  where ref = unsafePerformIO (Data.IORef.newIORef x) 



prim_readGlobal :: (Curry t0) => C_Global t0 -> Result (C_IO t0)
prim_readGlobal  = prim_readIORef



prim_writeGlobal :: (Curry t0) => C_Global t0 -> t0 -> Result (C_IO T0)
prim_writeGlobal  = prim_writeIORef


