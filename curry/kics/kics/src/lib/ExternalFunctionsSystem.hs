module ExternalFunctionsSystem where

import Curry
import CurryPrelude hiding (return,(>>=))

import qualified System.Environment as SE
import qualified System.CPUTime as SC
import System.Cmd
import System.Exit
import qualified Network.BSD as NB
import System.Posix.Process

instance ConvertCH C_Int ExitCode where
   toCurry ExitSuccess = toCurry (0::Integer)
   toCurry (ExitFailure i) = toCurry i

   fromCurry i = if hi Prelude.== 0 then ExitSuccess else ExitFailure hi
     where hi = fromCurry i

getCPUTime :: Result (C_IO C_Int)
getCPUTime = ioFunc0 (SC.getCPUTime >>= return . (`div` 1000000000))

getElapsedTime :: Result (C_IO C_Int)
getElapsedTime = error "getElapsedTime not provided"

getArgs :: Result (C_IO (List (List C_Char)))
getArgs = ioFunc0 SE.getArgs

prim_getEnviron :: (List C_Char) -> Result (C_IO (List C_Char))
prim_getEnviron = 
  ioFunc1 (\s -> SE.getEnvironment >>= (maybe (return "") return . lookup s))

getHostname :: Result (C_IO (List C_Char))
getHostname = ioFunc0 NB.getHostName

getPID :: Result (C_IO C_Int)
getPID = ioFunc0 (getProcessID >>= return . toInteger)

getProgName :: Result (C_IO (List C_Char))
getProgName = ioFunc0 (Curry.getProgName)
  -- conform with haskell would be: SE.getProgName

prim_system :: (List C_Char) -> Result (C_IO C_Int)
prim_system = ioFunc1 system

prim_sleep :: C_Int -> Result (C_IO T0)
prim_sleep = ioFunc1 (\t->system ("sleep "++show (t::Integer)) >> return ())

prim_exitWith :: Curry a => C_Int -> Result (C_IO a)
prim_exitWith e _ = C_IO (\ _ -> exitWith (fromCurry e) >>= return . IOVal)

