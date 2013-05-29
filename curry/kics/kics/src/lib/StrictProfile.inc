
import Prelude (Read(..))

instance Read ProcessInfo where
  readsPrec _ ('R':'u':'n':'T':'i':'m':'e':s) = [(ProcessInfo C0_1,s)]
  readsPrec _ ('E':'l':'a':'p':'s':'e':'d':'T':'i':'m':'e':s) = [(ProcessInfo C0_2,s)]
  readsPrec _ ('M':'e':'m':'o':'r':'y':s) = [(ProcessInfo C0_3,s)]
  readsPrec _ ('C':'o':'d':'e':s) = [(ProcessInfo (C0_ 4),s)]
  readsPrec _ ('S':'t':'a':'c':'k':s) = [(ProcessInfo (C0_ 5),s)]
  readsPrec _ ('H':'e':'a':'p':s) = [(ProcessInfo (C0_ 6),s)]
  readsPrec _ ('C':'h':'o':'i':'c':'e':'s':s) = [(ProcessInfo (C0_ 7),s)]
  readsPrec _ ('G':'a':'r':'b':'a':'g':'e':'C':'o':'l':'l':'e':'c':'t':'i':'o':'n':'s':s) = [(ProcessInfo (C0_ 8),s)]

getProcessInfos :: Debug (IO (List (T2 ProcessInfo Int)))
getProcessInfos = ioFunc0 "getProcessInfos" (coerce0 (getNextExtVal::Debug [(ProcessInfo,Int)]))

garbageCollectorOff ::  (Debug (IO T1)) 
garbageCollectorOff = ioFunc0 "garbageCollectorOff" (P.return t1)

garbageCollectorOn ::  (Debug (IO T1)) 
garbageCollectorOn = ioFunc0 "garbageCollectorOn" (P.return t1)

garbageCollect ::  (Debug (IO T1)) 
garbageCollect = ioFunc0 "garbageCollect" (P.return t1)

t1 :: T1
t1 = coerce ()

