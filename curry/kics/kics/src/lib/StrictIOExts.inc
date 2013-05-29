
import qualified Prelude (error)
import qualified Data.IORef as I
import Control.Monad.State 

prim_execCmd ::  List Char -> (Debug (IO (T3 Handle Handle Handle))) 
prim_execCmd = Prelude.error "prim_execCmd"

prim_connectToCmd ::  List Char -> (Debug (IO Handle)) 
prim_connectToCmd = Prelude.error "prim_connectToCmd"

prim_setAssoc ::  List Char -> List Char -> (Debug (IO T1)) 
prim_setAssoc = Prelude.error "prim_setAssoc"

prim_getAssoc ::  List Char -> (Debug (IO (Maybe (List Char)))) 
prim_getAssoc = Prelude.error "prim_getAssoc"

newIORef :: (ShowTerm x0) => x0 -> (Debug (IO (IORef x0))) 
newIORef = Prelude.error "newIORef"

prim_readIORef :: (ShowTerm x0) => (IORef x0) -> (Debug (IO x0)) 
prim_readIORef = Prelude.error "prim_readIORef"

prim_writeIORef :: (ShowTerm x0) => (IORef x0) -> x0 -> (Debug (IO T1)) 
prim_writeIORef = Prelude.error "prim_writeIORef"

showConsIORef :: IORef a -> Term
showConsIORef (IORef x) = showTerm x

instance ShowTerm a => Generator (IORef a) where
  generator = P.error "free variable of type IORef"