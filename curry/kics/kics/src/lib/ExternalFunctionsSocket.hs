module ExternalFunctionsSocket (module ExternalFunctionsSocket) where

import Curry
import CurryPrelude hiding (return, (>>=))
import ExternalDataSocket
import CurryIO

import Network
import Network.Socket 
import Control.Concurrent
import System.IO (Handle)

instance ConvertCH C_Int PortID where
  toCurry (PortNumber i) = toCurry (toInteger i)
  fromCurry i = PortNumber (fromInteger (fromCurry i))

prim_listenOn :: C_Int -> Result (C_IO C_Socket)
prim_listenOn  = CurryPrelude.ioFunc1 listenOn 

listenOnFresh :: Result (C_IO (T2 C_Int C_Socket))
listenOnFresh  = CurryPrelude.ioFunc0 listenOnFreshPort

listenOnFreshPort :: IO (PortID,Socket)
listenOnFreshPort = do
  s <- listenOn (PortNumber aNY_PORT)
  p <- Network.socketPort s
  return (p,s)


prim_socketListen :: C_Socket -> C_Int -> Result (C_IO T0)
prim_socketListen  = CurryPrelude.ioFunc2 listen



prim_socketAccept :: C_Socket -> Result (C_IO (T2 (List C_Char) C_Handle))
prim_socketAccept  = ioFunc1 (\ s -> Network.accept s >>= \ (h,s,_) -> return (s,One h))



prim_waitForSocketAccept :: C_Socket -> C_Int -> Result (C_IO (C_Maybe (T2 (List C_Char) C_Handle)))
prim_waitForSocketAccept  = CurryPrelude.ioFunc2 wait

wait :: Socket -> Int -> IO (Maybe (String,IOHandle))
wait s t = do
  mv <- newEmptyMVar
  tacc <- forkIO (Network.accept s >>= \ (h,s,_) ->  putMVar mv (Just (s,One h)))
  ttim <- forkIO (threadDelay (t*1000) >> putMVar mv Nothing)
  res <- takeMVar mv
  maybe (killThread tacc) (\_ -> killThread ttim) res
  return res




prim_connectToSocket :: List C_Char -> C_Int -> Result (C_IO C_Handle)
prim_connectToSocket  = ioFunc2 (\ s i -> connectTo s i >>= return . One)


