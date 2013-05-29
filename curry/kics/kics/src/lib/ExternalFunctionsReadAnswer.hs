module ExternalFunctionsReadAnswer (module ExternalFunctionsReadAnswer) where

import Curry
import CurryPrelude
import System.IO

prim_readAnswer :: (List C_Char) -> State -> C_IO C_Char
prim_readAnswer  = error "read answer not yet provided" --ioFunc1(undefined)



stdinUnbuffered :: State -> C_IO T0
stdinUnbuffered  = ioFunc0(hSetBuffering stdin NoBuffering)



stdinBuffered :: State -> C_IO T0
stdinBuffered  = ioFunc0(hSetBuffering stdin LineBuffering)