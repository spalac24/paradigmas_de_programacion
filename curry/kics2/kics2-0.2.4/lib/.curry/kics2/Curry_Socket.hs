{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Curry_Socket (C_Socket (..), d_C_listenOn, d_C_socketAccept, d_C_waitForSocketAccept, d_C_sClose, d_C_connectToSocket, d_C_listenOnFresh) where

import Basics
import qualified Curry_IO
import qualified Curry_Prelude
import qualified Curry_System
import Network
import Network.Socket
import Control.Concurrent
import Control.Concurrent.Thread.Delay(delay)
import qualified Curry_Prelude as CP


d_C_listenOn :: Curry_Prelude.C_Int -> ConstStore -> Curry_Prelude.C_IO C_Socket
d_C_listenOn x1 x3500 = Curry_Prelude.d_OP_dollar_hash d_C_prim_listenOn x1 x3500

d_C_socketAccept :: C_Socket -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_IO.C_Handle)
d_C_socketAccept x1 x3500 = Curry_Prelude.d_OP_dollar_hash_hash d_C_prim_socketAccept x1 x3500

d_C_waitForSocketAccept :: C_Socket -> Curry_Prelude.C_Int -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Maybe (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_IO.C_Handle))
d_C_waitForSocketAccept x1 x2 x3500 = Curry_Prelude.d_OP_dollar_hash (Curry_Prelude.d_OP_dollar_hash_hash (acceptCs id d_C_prim_waitForSocketAccept) x1 x3500) x2 x3500

d_C_sClose :: C_Socket -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_sClose x1 x3500 = Curry_Prelude.d_OP_dollar_hash_hash d_C_prim_sClose x1 x3500

d_C_connectToSocket :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Int -> ConstStore -> Curry_Prelude.C_IO Curry_IO.C_Handle
d_C_connectToSocket x1 x2 x3500 = Curry_Prelude.d_OP_dollar_hash (Curry_Prelude.d_OP_dollar_hash_hash (acceptCs id d_C_prim_connectToSocket) x1 x3500) x2 x3500

d_C_prim_listenOn :: Curry_Prelude.C_Int -> ConstStore -> Curry_Prelude.C_IO C_Socket
d_C_prim_listenOn x1 x3500 = external_d_C_prim_listenOn x1 x3500

d_C_listenOnFresh :: ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int C_Socket)
d_C_listenOnFresh x3500 = external_d_C_listenOnFresh x3500

d_C_prim_socketAccept :: C_Socket -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_IO.C_Handle)
d_C_prim_socketAccept x1 x3500 = external_d_C_prim_socketAccept x1 x3500

d_C_prim_waitForSocketAccept :: C_Socket -> Curry_Prelude.C_Int -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Maybe (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_IO.C_Handle))
d_C_prim_waitForSocketAccept x1 x2 x3500 = external_d_C_prim_waitForSocketAccept x1 x2 x3500

d_C_prim_sClose :: C_Socket -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_prim_sClose x1 x3500 = external_d_C_prim_sClose x1 x3500

d_C_prim_connectToSocket :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Int -> ConstStore -> Curry_Prelude.C_IO Curry_IO.C_Handle
d_C_prim_connectToSocket x1 x2 x3500 = external_d_C_prim_connectToSocket x1 x2 x3500
type C_Socket = PrimData Socket

instance ConvertCurryHaskell CP.C_Int PortID where
  toCurry (PortNumber i) = toCurry (toInteger i)
  fromCurry i = PortNumber (fromInteger (fromCurry i))

external_d_C_prim_listenOn :: CP.C_Int -> ConstStore -> CP.C_IO C_Socket
external_d_C_prim_listenOn i _ = toCurry listenOn i

external_d_C_listenOnFresh :: ConstStore -> CP.C_IO (CP.OP_Tuple2 CP.C_Int C_Socket)
external_d_C_listenOnFresh _ = toCurry listenOnFreshPort
  where
  listenOnFreshPort :: IO (PortID,Socket)
  listenOnFreshPort = do
    s <- listenOn (PortNumber aNY_PORT)
    p <- Network.socketPort s
    return (p,s)

external_d_C_prim_socketAccept :: C_Socket
  -> ConstStore -> CP.C_IO (CP.OP_Tuple2 CP.C_String Curry_IO.C_Handle)
external_d_C_prim_socketAccept socket _ =
 toCurry (\s -> Network.accept s >>= \ (h,s,_) -> return (s,OneHandle h)) socket


external_d_C_prim_waitForSocketAccept :: C_Socket -> CP.C_Int
 -> ConstStore -> CP.C_IO (CP.C_Maybe (CP.OP_Tuple2 (CP.OP_List CP.C_Char) Curry_IO.C_Handle))
external_d_C_prim_waitForSocketAccept s i _ = toCurry wait s i

wait :: Socket -> Int -> IO (Maybe (String,CurryHandle))
wait s t =
  if t<0
  then Network.accept s >>= \ (h,s,_) -> return (Just (s,OneHandle h))
  else do
    mv <- newEmptyMVar
    tacc <- forkIO (Network.accept s >>= \ (h,s,_) ->
                    putMVar mv (Just (s,OneHandle h)))
    ttim <- forkIO (delay ((fromIntegral t :: Integer)*1000)
                    >> putMVar mv Nothing)
    res <- takeMVar mv
    maybe (killThread tacc) (\_ -> killThread ttim) res
    return res

external_d_C_prim_sClose :: C_Socket -> ConstStore -> CP.C_IO CP.OP_Unit
external_d_C_prim_sClose s _ = toCurry sClose s

external_d_C_prim_connectToSocket :: CP.C_String -> CP.C_Int
                                  -> ConstStore -> CP.C_IO Curry_IO.C_Handle
external_d_C_prim_connectToSocket str i _ =
  toCurry (\ s i -> connectTo s i >>= return . OneHandle) str i

