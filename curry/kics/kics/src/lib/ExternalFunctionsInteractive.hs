module ExternalFunctionsInteractive where

import Curry
import CurryPrelude hiding (return)

printTerm :: (Show t0,Curry t0) => t0 -> Result (C_IO T0)
printTerm x _ = C_IO (\ _ -> print x >> return (IOVal T0))
  
