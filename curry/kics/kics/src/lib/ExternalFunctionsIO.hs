module ExternalFunctionsIO where

import Curry
import CurryPrelude hiding (return,(>>=))
import InstancesIO

import qualified System.IO as SI
import Control.Concurrent
import qualified Control.Exception as CE

instance ConvertCH C_IOMode SI.IOMode where
  toCurry SI.ReadMode = C_ReadMode
  toCurry SI.WriteMode = C_WriteMode
  toCurry SI.AppendMode = C_AppendMode

  fromCurry C_ReadMode = SI.ReadMode
  fromCurry C_WriteMode = SI.WriteMode
  fromCurry C_AppendMode = SI.AppendMode

instance ConvertCH C_SeekMode SI.SeekMode where
  toCurry SI.AbsoluteSeek = C_AbsoluteSeek
  toCurry SI.RelativeSeek = C_RelativeSeek
  toCurry SI.SeekFromEnd  = C_SeekFromEnd

  fromCurry C_AbsoluteSeek = SI.AbsoluteSeek
  fromCurry C_RelativeSeek = SI.RelativeSeek
  fromCurry C_SeekFromEnd  = SI.SeekFromEnd

stdin :: Result C_Handle
stdin _ = PrimValue (One SI.stdin)

stdout :: Result C_Handle
stdout _ = PrimValue (One SI.stdout)

stderr :: Result C_Handle
stderr _ = PrimValue (One SI.stderr)

prim_openFile :: List C_Char -> C_IOMode -> Result (C_IO C_Handle)
prim_openFile = ioFunc2 (\ s m -> do
  h <- SI.openFile s m
  return (One h))

prim_hClose :: C_Handle -> Result (C_IO T0)
prim_hClose = ioFunc1 (\ eh -> case eh of
  One h     -> SI.hClose h
  Two h1 h2 -> SI.hClose h1 >> SI.hClose h2)

prim_hFlush :: C_Handle -> Result (C_IO T0)
prim_hFlush = ioFunc1 (SI.hFlush . outputHandle)

prim_hIsEOF :: C_Handle -> Result (C_IO C_Bool)
prim_hIsEOF = ioFunc1 (SI.hIsEOF . inputHandle)

prim_hSeek :: C_Handle -> C_SeekMode -> C_Int -> Result (C_IO T0)
prim_hSeek = ioFunc3 (\ h -> SI.hSeek (inputHandle h))

prim_hWaitForInput ::  C_Handle -> C_Int -> Result (C_IO C_Bool)
prim_hWaitForInput = ioFunc2 (\ h -> myhWaitForInput (inputHandle h))

myhWaitForInput :: SI.Handle -> Int -> IO Bool
myhWaitForInput h i =
  if i < 0 
  then SI.hIsEOF h >>= return . not
  else SI.hWaitForInput h i 

selectHandle :: [IOHandle] -> Int -> IO Int
selectHandle handles t = do
  mvar <- newEmptyMVar
  threads <- mapM (\ (i,h) -> forkIO (waitOnHandle (inputHandle h) i t mvar)) 
                  (zip [0..] handles)
  inspectRes (length handles) mvar threads

inspectRes :: Int -> MVar (Maybe Int) -> [ThreadId] ->  IO Int
inspectRes 0 _    _       = return (-1)
inspectRes n mvar threads = do
  res <- readMVar mvar
  case res of 
    Nothing -> inspectRes (n-1) mvar threads
    Just v  -> mapM_ killThread threads >> return v

waitOnHandle :: SI.Handle -> Int -> Int -> MVar (Maybe Int) -> IO ()
waitOnHandle h v t mvar = do
   	    ready <- myhWaitForInput h t
  	    putMVar mvar (if ready then Just v else Nothing)

prim_hWaitForInputs :: List C_Handle -> C_Int -> Result (C_IO C_Int)
prim_hWaitForInputs  = ioFunc2 selectHandle

prim_hGetChar :: C_Handle -> Result (C_IO C_Char)
prim_hGetChar = ioFunc1 (SI.hGetChar . inputHandle)

prim_hPutChar :: C_Handle -> C_Char -> Result (C_IO T0)
prim_hPutChar = ioFunc2 (SI.hPutChar . outputHandle)

prim_hIsReadable :: C_Handle -> Result (C_IO C_Bool)
prim_hIsReadable = ioFunc1 (SI.hIsReadable . inputHandle)

prim_hIsWritable :: C_Handle -> Result (C_IO C_Bool)
prim_hIsWritable = ioFunc1 (SI.hIsWritable . outputHandle)

