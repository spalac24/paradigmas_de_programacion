module ExternalDataSocket (module ExternalDataSocket) where

import Curry
import CurryPrelude

import Network

type C_Socket = Prim Socket

instance Read Socket where

instance Generate Socket where
  genFree  = error "no random sockets"
  maxArity = error "no narrowing on sockets"


