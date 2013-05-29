module ExternalDataIOExts where

import Curry
import CurryPrelude

import Data.IORef

type C_IORef a = Prim (IORef a)

instance Show (IORef a) where
  show _ = "IOREF"

instance Read (IORef a) where
  readsPrec = error "reading IOREF"

instance Generate (IORef a) where
  genFree    = error "free variable of type IOExts.IORef"
  maxArity _ = error "free variable of type IOExts.IORef"

