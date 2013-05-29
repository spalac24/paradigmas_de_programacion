module DebuggerMonad where

import Prelude hiding (catch)
import Term
import Control.Exception
import Control.Monad.Error
import Control.Monad.State
import System.Process
import Data.IORef
import System.IO 
import Unsafe.Coerce
import Store

-----------------------------------------------------------
--   Term - Representation of observable data 
--          (from module Term)
--
--   ShowTerm - Class to obtain Term representation
----------------------------------------------------------- -}

--  consTerm, consUnderscore - werden anstelle der
--  Konstruktoren exportiert (good for what?)
consTerm :: String -> [Term] -> Term
consTerm = Term

consUnderscore :: Term
consUnderscore = Underscore

consFailed :: String -> Term
consFailed = FailTerm

--  The class to obtain Term representation. 

class (Eq a,Generator a) => ShowTerm a where
  showCons :: a -> Term
  
showTerm :: ShowTerm a => a -> Term
showTerm x = case toData x of
  Uneval -> Underscore
  Fail s -> FailTerm s
  Or r xs  -> OrTerm r (map showTerm (tail (x:map fromData xs)))
  _      -> showCons x

underscore :: Convert a => a
underscore = fromData Uneval

failure :: String -> Data
failure s = Fail s

---------------------------------------------------------
--   BoolStack - a List of boolean values is efficiently
--   coded as a list of Integers (arbitrary size). 
--   Assumption is that there will not be many Falses in 
--   sequence.
--   If this assumption is wrong, we could alternatively 
--   change to no. of Trues,no. of False, no of Trues...
--   Another alternativ could be a fraction of which
--   the slist is the representation as a fractional chain, 
--   cf. jan christiansen.
--   Nice for these alternatives: keep implementation
--   abstract by push/pop/empty.
----------------------------------------------------------- 


type BoolStack = [Integer]

emptyBoolStack :: BoolStack
emptyBoolStack = [0]

---  implementation of push/pop 
--- makes sure that this is an infinite list of Trues:

allTrue :: BoolStack
allTrue = []


--- popping from a BoolStack

pop :: BoolStack -> (BoolStack, Bool)
pop []     = ([],True)
pop [0]    = error "pop: Stack underflow"
pop (0:os) = (os, False)
pop (n:os) = (n-1 : os, True)

--- pushing to a BoolStack

push :: BoolStack -> Bool -> BoolStack
push []     True  = []
push (b:bs) True  = b+1 : bs
push bs     False = 0 : bs


{-
  Ein Berechnungsschritt ordnet einem Debugger-Zustand
  entweder den Nachfolgezustand und das Ergebnis der
  Auswertung oder den bei der interaktiven
  Auswertung gefundenen Bug zu.

  Bei der Berechnung werden

  - Orakeleinträge konsumiert,
  - eventuell das verbose-Flag geändert,
  - Einträge zu past hinzugefügt und
  - Einträge von future konsumiert
-}


type Debug a = StateT DebugState (ErrorT BugReport Prelude.IO) a

-- a BugReport is the left hand side + result

data BugReport = BugReport { lhs :: Term,
                             rhs :: Term }
               | Back 
               | Quit

-- BugReport is treated as an error in the error monad
instance Error BugReport where
    noMsg = Quit 

---------------------------------------------------------
-- Debugstate - the intrnal state of the debugger
--------------------------------------------------------- 


--  The Orakel is a list of boolean values encoded as a
--  BoolStack.

type Oracle = BoolStack



-- Display mode of the debugger

data DisplayMode = DisplayMode 
    { verbose :: Bool,       -- verbose status information
      optionalResult:: Bool, -- do not inspect results 
      depth :: Maybe Int,    -- show terms up to certain depth 
      rewind :: String,      -- to step back 
      rewinding :: Bool      -- also to step back
      
    }


--  The working modes of the debugger:
--  
--  The debugger is either interactive, asking the user
--  for his opinion of the next step or silent. 
--  In silent mode there are two kinds of information:
--  a) are we in the main thread or do we inspect a result
--  b) is the current subcomputation correct or yet unrated
--
--  The combinations mean:
--
--  Main+Unrated
--    the user skipped. We may perform io in this subcomputation.
--    Old ratings are kept.
--  Main+Correct
--    the user decided the current sub computation to be correct.
--    We may perform io.
--  Inspect+Skipped
--    The user wants to know a result of a currently future
--    subcomputation. Old ratings are kept. No io.
--  Inspect+Unrated
--    The user wants to know a result of a currently future
--    subcomputation. No io.
-- The last Boolean value is the spy mode

--  the internal state of the debugger

data DebugState = DebugState {
      interactive, correct, mainthread, spy 
                   :: Bool,              -- several debug modes, see below
      oracle       :: Oracle,            -- the current oracle
      displayMode  :: IORef DisplayMode, -- why is this an ioref?
      past, future :: BoolStack,         -- Both stacks:
                                         -- True: no rating yet, False rated.
                                         -- past: computation up to current point
                                         -- future: remaining computation 
      gui         :: Maybe (Handle,ProcessHandle),
                                         -- our link to the biotope
      extValues   :: [String],           -- the values from external io functions
      history     :: String,             -- to step backwards
      refCounter :: Int,                 -- for counting or references
      store :: Store,                    -- for call-time choice

      fontSettings :: [String]           -- for colors, bold etc.
    } 


{- ---------------------------------------------------------
   Debugger - Monade

   Kombiniert das Debugging von Argument und Funktion zu
   einem Berechnungsschritt. 

   - Das Orakel steuert, ob das Argument ausgewertet oder
     durch den Platzhalter underscore ersetzt wird.

   - wenn im Argument ein Bug gefunden wurde, wird die 
     Auswertung abgebrochen
--------------------------------------------------------- -}

------------------------------------------
-- some auxiliaries to manage the state
------------------------------------------

setDepth :: Maybe Int -> DisplayMode -> DisplayMode
setDepth d m = m{depth=d}

getDisplayMode :: Debug DisplayMode
getDisplayMode = do 
  state <- get
  liftIO $ readIORef (displayMode state)

modifyDisplayMode :: (DisplayMode -> DisplayMode) -> Debug ()
modifyDisplayMode f = do
  state <- get
  liftIO $ modifyIORef (displayMode state) f

initRefCounterVal :: Int
initRefCounterVal = 0

nextDeRef :: Int -> Debug Int
nextDeRef i = do 
  state <- get
  let ref = refCounter state
  put (state {refCounter=ref+i+1})
  return ref

nextRef :: Maybe Int -> Int -> Debug OrRef
nextRef isGen i = do
  ref <- nextDeRef i
  return $ maybe (mkRefWithGenInfo NoGenerator ref) (mkRef ref i) isGen

isLogProg :: Debug Bool
isLogProg = do
  state <- get
  return (refCounter state>initRefCounterVal)

getStore :: Debug Store
getStore = get >>= return . store

putStore :: Store -> Debug ()
putStore st = do
  state <- get 
  put state{store=st}


eval :: Convert a => Debug a -> Debug a
eval act = do 
          printOrc
          state <- get
          let (orc, needed) = pop (oracle state)
          --when (orc==[0]) (liftIO (putStrLn "empty!"))
          put (state {oracle = orc})
          if needed then act else return underscore

printOrc :: Debug ()
printOrc = do
  st <- get
  dm <- getDisplayMode
  if verbose dm
   then liftIO $ do
                  putStrLn ""
                  putStr $ "oracle: "  ++ (show $ take 5 $ oracle st)
                  putStr $ " store: "  ++ (show $ store st)

   else return ()

setFont :: FontSetting -> Debug ()
setFont mode = do
  st <- get
  put st{fontSettings=mode:fontSettings st}

unsetFont :: Debug ()
unsetFont = do
  st <- get
  put st{fontSettings=tail (fontSettings st)}

---------------------------------------------------------------
-- representation of internal data types 
---------------------------------------------------------------

data Data = C0_1 | C0_2 | C0_3 
          | C1_1 Data
          | C1_2 Data
          | C1_3 Data 
          | C2_1 Data Data 
          | C2_2 Data Data 
          | C2_3 Data Data 
          | C3_1 Data Data Data
          | C3_2 Data Data Data
          | C3_3 Data Data Data
          | PrimChar Char
          | PrimFloat Float
          | Prim Term ()
          | PrimFunc Term (Data -> Debug Data)
          | C0_ Int 
          | C1_ Int Data 
          | C2_ Int Data Data 
          | C3_ Int Data Data Data
          | C Int Data Data Data Data [Data]
          | Uneval
          | Fail String
          | Or OrRef [Data] deriving (Show,Eq)

 
class Convert a where
  toData   :: a -> Data
  fromData :: Data -> a

instance Convert Data where
  toData = id
  fromData = id

instance Generator Data where
  generator = error "untyped generator"

instance ShowTerm Data where
  showCons (Prim t _)     = t
  showCons (PrimFunc t _) = t
  showCons Uneval         = Underscore
  showCons (Or r ds)      = OrTerm r (map showTerm ds)
  showCons (PrimChar c)   = Term [c] []
  showCons (PrimFloat f)  = Term (show f) []
  showCons c              = case sel c of
    (i,args) -> Term "C" (Term (show i) []:map showCons args)

---------------------------------------------
-- uniform constructors and selectors
---------------------------------------------

c0 1 = C0_1 
c0 2 = C0_2
c0 3 = C0_3
c0 i = C0_ i
    
c1 1 = C1_1
c1 2 = C1_2
c1 3 = C1_3
c1 i = C1_ i

c2 1 = C2_1
c2 2 = C2_2
c2 3 = C2_3
c2 i = C2_ i

c3 1 = C3_1
c3 2 = C3_2
c3 3 = C3_3
c3 i = C3_ i

cons j []           = c0 j
cons j [x]          = c1 j x 
cons j [x,y]        = c2 j x y 
cons j [x,y,z]      = c3 j x y z 
cons j (a:b:c:d:xs) = C j a b c d xs 

sel C0_1             = (1,[])
sel C0_2             = (2,[])
sel C0_3             = (3,[])
sel (C0_ i)          = (i,[])
sel (C1_1 x)         = (1,[x])
sel (C1_2 x)         = (2,[x])
sel (C1_3 x)         = (3,[x])
sel (C1_ i x)        = (i,[x])
sel (C2_1 x y)       = (1,[x,y])
sel (C2_2 x y)       = (2,[x,y])
sel (C2_3 x y)       = (3,[x,y])
sel (C2_ i x y)      = (i,[x,y])
sel (C3_1 x y z)     = (1,[x,y,z])
sel (C3_2 x y z)     = (2,[x,y,z])
sel (C3_3 x y z)     = (3,[x,y,z])
sel (C3_ i x y z)    = (i,[x,y,z])
sel (C i a b c d xs) = (i,a:b:c:d:xs)
sel x = error ("sel "++show x)
  
---------------------------------------------------------------
-- generator variables
---------------------------------------------------------------

class Convert a => Generator a where
  generator :: Int -> Debug a

---------------------------------------------------------------
-- representation of external data types 
---------------------------------------------------------------

data Func a b = Func Term (a -> Debug b)
              | FuncUneval
              | FuncFail String
              | FuncOr OrRef [Func a b]

instance Eq (Func a b) where
  Func t _   == Func t' _  = t==t'
  FuncUneval == FuncUneval = True
  FuncFail x == FuncFail y = x==y
  FuncOr r xs== FuncOr s ys= r==s && xs==ys
  _          == _          = False


unprim :: Data -> a
unprim (Prim _ x) = unsafeCoerce x

toprim :: Term -> a -> Data
toprim t x = Prim t (unsafeCoerce x)


newtype World = World Data deriving Eq

instance Convert World where
  toData (World d) = d
  fromData = World

world = World C0_1

instance ShowTerm World where
  showCons w = Term (show w) []

instance Show World where
  show (World C0_1) = "#world"

instance Generator World where
  generator _ = eval (return world)

instance Convert () where
  toData _   = C0_1
  fromData _ = ()

-- these must be conform with ExternalInstancesPrelude.hs
instance Show (a->b) where
  show _ = "FUNCTION"

instance Eq (a->b) where
  (==) = error "comparing FUNCTION"

instance (Convert a,Convert b) => Convert (Func a b) where
  toData (Func t f)   = PrimFunc t (\ x -> f (fromData x) >>= return . toData)
  toData FuncUneval   = Uneval
  toData (FuncFail s) = Fail s
  toData (FuncOr r xs)  = Or r (map toData xs)
  
  fromData (PrimFunc t f) = Func t (\ x -> f (toData x) >>= return . fromData)
  fromData Uneval         = FuncUneval
  fromData (Fail s)       = FuncFail s
  fromData (Or r xs)      = FuncOr r (map fromData xs)

