
import Control.Monad.State (liftIO)
import qualified Prelude as P

printTerm :: (ShowTerm x0) => x0 -> (Debug (IO T1)) 
printTerm = ioFunc1 "printTerm" pt
  where
    pt :: (ShowTerm x0) => x0 -> Debug T1
    pt x = coerce0 (P.mapM_ addConsole (P.show (showTerm x)))

