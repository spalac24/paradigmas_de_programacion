module ExternalDataGlobal (module ExternalDataGlobal) where

import Curry
import CurryPrelude
import Data.IORef

type C_Global t0 = Prim (IORef t0)


