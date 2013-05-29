
import qualified Prelude as P (return,seq) 
import System.IO.Unsafe
import Data.IORef
import Control.Monad.State (liftIO)

-- compare to ExternalDataIOExts.hs
instance Show (IORef a) where 
  show _ = "IOREF"

global :: (ShowTerm x0) => x0 -> GlobalSpec -> (Debug (Global x0)) 
global x _ = ref `P.seq` (do
                            let sx = showTerm x
                            P.return (Global (toprim (consTerm "global" [sx]) ref)))
  where ref = unsafePerformIO (Data.IORef.newIORef x) 

prim_readGlobal :: (ShowTerm x0) => (Global x0) -> (Debug (IO x0)) 
prim_readGlobal = 
  ioFunc1 "prim_readGlobal" (\ (Global g) -> liftIO (readIORef (unprim g)))

prim_writeGlobal :: (ShowTerm x0) => (Global x0) -> x0 -> (Debug (IO T1)) 
prim_writeGlobal = 
  ioFunc2 "prim_writeGlobal" (\ (Global g) x -> coerce0 (liftIO (writeIORef (unprim g) x)))

showConsGlobal :: Global a -> Term
showConsGlobal (Global g) = showTerm g

instance Generator a => Generator (Global a) where
  generator i = do {x<-generator i; P.return (Global x)}
