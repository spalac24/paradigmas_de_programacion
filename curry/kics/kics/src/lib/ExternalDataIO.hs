module ExternalDataIO (module ExternalDataIO) where

import Curry
import CurryPrelude
import System.IO

-- somehow using an either type did not get the curry class for prim through.
data IOHandle = One Handle | Two Handle Handle deriving (Show,Eq)
type C_Handle = Prim IOHandle

inputHandle, outputHandle :: IOHandle -> Handle
inputHandle  (One h)   = h
inputHandle  (Two h _) = h
outputHandle (One h)   = h
outputHandle (Two _ h) = h


instance Read IOHandle where
  readsPrec = error "reading Handle"

instance Generate IOHandle where
  genFree    = error "free variable of type IO-Handle"
  maxArity _ = error "free variable of type IO-Handle"



