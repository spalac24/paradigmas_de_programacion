
import qualified Prelude as P

getCPUTime ::  (Debug (IO Int)) 
getCPUTime = ioFunc0 "getCPUTime" getNextExtVal

getElapsedTime ::  (Debug (IO Int)) 
getElapsedTime = ioFunc0 "getElapsedTime" getNextExtVal

getArgs ::  Debug (IO (List (List Char))) 
getArgs = ioFunc0 "getArgs" (coerce0 (getNextExtVal::Debug [[P.Char]]))

prim_getEnviron ::  List Char -> (Debug (IO (List Char))) 
prim_getEnviron = ioFunc1 "prim_getEnviron" (\_ -> coerce0 (getNextExtVal::Debug [P.Char]))

getHostname ::  Debug (IO (List Char))
getHostname = ioFunc0 "getHostname" (coerce0 (getNextExtVal::Debug [P.Char]))

getPID ::  (Debug (IO Int)) 
getPID = ioFunc0 "getPID" (coerce0 (getNextExtVal::Debug P.Int))

getProgName ::  Debug (IO (List Char)) 
getProgName = ioFunc0 "getProgName" (coerce0 (getNextExtVal::Debug [P.Char]))

prim_system ::  List Char -> (Debug (IO Int)) 
prim_system = ioFunc1 "prim_system" (\_ -> coerce0 (getNextExtVal::Debug P.Int))

prim_exitWith :: ShowTerm x0 => Int -> (Debug (IO x0)) 
prim_exitWith = ioFunc1 "prim_exitWith" (\_ -> Prelude.return underscore)

prim_sleep ::  Int -> (Debug (IO T1)) 
prim_sleep = ioFunc1 "prim_sleep" (\ _ -> P.return (coerce ()))

