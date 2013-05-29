module ExternalFunctionsIOExts where

import Curry
import CurryPrelude hiding (return,(>>=))
import CurryIO
import ExternalDataIOExts

import qualified Data.IORef as Ref
import System.Process
import Network
import qualified Network.Socket as SO
import System.IO.Unsafe
import Control.Concurrent
import System.IO

instance Eq (List C_Char) where
  List == List = True
  List == (_ :< _) = False
  (_ :< _) == List = False
  (C_Char c :< xs) == (C_Char c' :< ys) = c Prelude.== c' && xs Prelude.== ys


type Assocs = [(C_String,C_String)]

assocs :: Ref.IORef Assocs
assocs = unsafePerformIO (Ref.newIORef [])

getAssocs :: IO Assocs
getAssocs = Ref.readIORef assocs

setAssocs :: Assocs -> IO ()
setAssocs as = Ref.writeIORef assocs as

prim_execCmd :: List C_Char -> Result (C_IO (T3 C_Handle C_Handle C_Handle))
prim_execCmd = ioFunc1 (\s -> do
     (h1,h2,h3,_) <- runInteractiveCommand s
     return (One h1,One h2,One h3))

prim_connectToCmd :: List C_Char -> Result (C_IO C_Handle)
prim_connectToCmd = ioFunc1 (\s -> do
  (hin,hout,herr,_) <- runInteractiveCommand s
  forkIO (forwardError herr)
  return (Two hout hin))

forwardError :: Handle -> IO ()
forwardError h = do
   eof <- hIsEOF h 
   if eof then return ()
          else hGetLine h >>= hPutStrLn System.IO.stderr >> forwardError h

prim_setAssoc :: List C_Char -> List C_Char -> Result (C_IO T0)
prim_setAssoc key val = ioFunc0 (do 
                    as <- getAssocs 
                    setAssocs ((key,val):as))


prim_getAssoc :: List C_Char -> Result (C_IO (C_Maybe (List C_Char)))
prim_getAssoc key _ = C_IO (\_ -> do 
         as <- getAssocs
         return (IOVal (maybe C_Nothing C_Just (lookup key as))))

 

newIORef :: Curry t0 => t0 -> Result (C_IO (C_IORef t0))
newIORef x = ioFunc0 (Ref.newIORef x) 

prim_readIORef :: Curry t0 => C_IORef t0 -> Result (C_IO t0)
prim_readIORef (PrimValue ref) _ = 
   C_IO (\ _ -> do 
           v <- Ref.readIORef ref 
           return (IOVal v))

prim_writeIORef :: Curry t0 => C_IORef t0 -> t0 -> Result (C_IO T0)
prim_writeIORef (PrimValue ref) x = ioFunc0 (Ref.writeIORef ref x) 