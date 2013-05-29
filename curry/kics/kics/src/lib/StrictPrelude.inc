
-- #---------------------------------------------------
-- include for debugging into Prelude
------------------------------------------------------

import qualified Prelude as P

import StrictSteps
import Term (Term(..))
import qualified Char
import Control.Monad.State hiding (ap)

-----------------------------------------
-- abbreviations for qualified prelude
-----------------------------------------

idp :: Data -> Data
idp = P.id

mapp = P.map
dot = (P..)
perror = P.error

--------------------------------
-- apply functions
--------------------------------

infixr 0 $!
infixr 0 $!!
infixr 0 $#
infixr 0 $##

ap :: Func a b -> a -> Debug b
ap (Func _ f) x = f x 

($!),($!!),($#),($##),apply ::(ShowTerm a,ShowTerm b)=> Func a b -> a -> Debug b
f $! x  = hnf  (app f) x
f $# x  = ghnf (app f) x
f $!! x = nf   (app f) x
f $## x = gnf  (app f) x

apply = app 


----------------------------
-- non-io external functions
----------------------------

prim_error :: ShowTerm a => List Char -> Debug a
prim_error = trace1 failure

failed :: ShowTerm a => Debug a
failed = trace0 (failure "failed")

prim_ord :: Char -> Debug Int
prim_ord = trace1 Char.ord

prim_chr :: Int -> Debug Char
prim_chr = trace1 Char.chr 

(===) :: ShowTerm a => a -> a -> Debug Bool
x === y = eval (do
  z <- eqData P.False (toData x) (toData y) 
  P.return (fromData z))

(==) :: ShowTerm a => a -> a -> Debug Bool
x==y = eval (do
  z <- eqData P.True (toData x) (toData y) 
  P.return (fromData z))

(&) :: Success -> Success -> Debug Success
(&) = trace2 ((\ x y -> y)::Data -> Data -> Data)

andBreadth :: [Bool] -> Debug Bool
andBreadth = trace1 P.and

prim_show :: ShowTerm a => a -> Debug (List Char)
prim_show x = eval (P.return (coerce (P.show (showTerm x))))

----------------------------
-- io external functions
----------------------------

type IO a = Func World (IOVal a)

newtype IOVal a = IOVal a deriving (Show,Eq)

instance ShowTerm a => ShowTerm (IOVal a) where
  showCons (IOVal x) = show1 "IO" x

instance Generator a => Generator (IOVal a) where
  generator i = do {x <- generator i; P.return (IOVal x)}

instance Convert a => Convert (IOVal a) where
  toData (IOVal a) = toData a
  fromData x = IOVal (fromData x)

vreturn :: a -> Debug (IOVal a)
vreturn x = P.return (IOVal x)

(>>=) :: (ShowTerm a,ShowTerm b) => IO a -> Func a (IO b) -> Debug (IO b)
act >>= cont = 
  eval (do {x <-ap (pc3 (consTerm ">>=" []) bind) act; ap x cont})

bind :: (ShowTerm a,ShowTerm b) => IO a -> Func a (IO b) -> World -> Debug (IOVal b)
bind act cont w = eval (do
  vx <- apply act w
  x <- eval (case vx of IOVal x -> P.return x)
  cont' <- apply cont x
  apply cont' w)

return :: ShowTerm a => a -> Debug (IO a) 
return x = 
  eval (P.return (pc1 (consTerm "return" [showTerm x])
                         (lambda_world (P.return x))))
      

  
extVal :: P.Read a => (a -> Debug ()) -> Debug a
extVal act = do
  v <- getNextExtVal
  act v 
  P.return v

prim_putChar :: Char -> Debug (IO T1)
prim_putChar = ioFunc1 "prim_putChar" (coerce1 addConsole)

prim_readFile :: List Char -> Debug (IO (List Char))
prim_readFile = ioFunc1 "prim_readFile" (coerce1 (extVal `dot` addReadFile))

prim_writeFile :: List Char -> List Char -> Debug (IO T1)
prim_writeFile = ioFunc2 "prim_writeFile" (coerce2 addWrittenFile)

prim_appendFile :: List Char -> List Char -> Debug (IO T1)
prim_appendFile = ioFunc2 "prim_appendFile" (coerce2 addAppendedFile)

catchFail :: ShowTerm a => IO a -> IO a -> Debug (IO a)
catchFail act alt = eval (do
  let sx = showTerm act
      sy = showTerm alt
  P.return (pc1 (consTerm "catch" [sx,sy]) (catch act alt)))

catch :: ShowTerm a => IO a -> IO a -> World -> Debug (IOVal a)
catch act alt world = 
  eval
    (do x <- apply act world
        y <- apply alt world 
        b <- getNextExtVal
        P.return (if b then x else y))

getSearchTree :: ShowTerm a => a -> Debug (IO (SearchTree a))
getSearchTree = ioFunc1 "getSearchTree" (coerce1 (P.return `dot` C1_1))

getChar :: Debug (IO Char)
getChar = ioFunc0 "getChar" (coerce0 (extVal addConsole))

------------------------------------------------------
-- functions of internal use in Curry run-time system
------------------------------------------------------

cond :: (ShowTerm b) => Success -> b -> Debug b
cond x y = eval (hnf' P.True (\ _ -> P.return y) x)

commit :: ShowTerm a => a -> Debug a
commit = trace1 idp

(=:<=) :: ShowTerm a => a -> a -> Debug Success
(=:<=) = trace2 (\ x y -> boolToSuccess "=:<=" (x `peq` y))

peq :: Data -> Data -> P.Bool
peq x y = x P.== y

-------------------------------
-- generic 1,2,3... funtions
-------------------------------

show0 :: String -> Term
show0 s = consTerm s []

show1 :: ShowTerm a => String -> a -> Term
show1 s a = let sa = showTerm a in consTerm s [sa]

show2 :: (ShowTerm a,ShowTerm b) => String -> a -> b -> Term
show2 s a b = let {sa=showTerm a; sb=showTerm b} in consTerm s [sa,sb]

show3 :: (ShowTerm a,ShowTerm b, ShowTerm c) => 
         String -> a -> b -> c -> Term
show3 s a b c = let {sa=showTerm a; sb=showTerm b; sc=showTerm c}
                 in consTerm s [sa,sb,sc]

-- trusted functions 
trace0 :: (Convert res',Convert res) => res' -> Debug res
trace0 res = eval (P.return (coerce res))

trace1 :: (Convert a,Convert a',Convert res',Convert res) => (a' -> res') -> a -> Debug res
trace1 f x = eval (P.return (coerce (f (coerce x))))

trace2 :: (Convert a,Convert a',Convert b,Convert b',Convert res',Convert res) => 
  (a' -> b' -> res') -> a -> b -> Debug res
trace2 f x y = eval (P.return (coerce (f (coerce x) (coerce y))))

-- untrusted functions 
bangTrace0 :: ShowTerm res => String -> res -> Debug res
bangTrace0 s res = traceFunCall (show0 s) (P.return res)

bangTrace1 :: (ShowTerm a, ShowTerm res) => String -> (a -> res) -> a -> Debug res
bangTrace1 s f x = traceFunCall (show1 s x) (P.return (f x))

bangTrace2 :: (ShowTerm a, ShowTerm b, ShowTerm res) => 
          String -> (a -> b -> res) -> a -> b -> Debug res
bangTrace2 s f x y = traceFunCall (show2 s x y) (P.return (f x y))


-- trusted io functions
coerce :: (Convert a,Convert b) => a -> b
coerce x = fromData (toData x)

coerce0 :: (Convert a,Convert res) => (Debug a) -> Debug res
coerce0 f = do { y<-f; P.return (coerce y)}

coerce1 :: (Convert a,Convert b,Convert c,Convert d) => (a -> Debug b) -> c -> Debug d
coerce1 f x = do { y<-f (coerce x); P.return (coerce y)}

coerce2 :: (Convert a,Convert b,Convert res,Convert a',Convert b',Convert res') => 
 (a -> b -> Debug res) -> a' -> b' -> Debug res'
coerce2 f x y = do { res<-f (coerce x) (coerce y); P.return (coerce res)}

lambda_world :: Convert a => Debug a -> World -> Debug (IOVal a)
lambda_world act w = eval (do {v <- act; P.return (IOVal v)})

ioaction :: (ShowTerm a) => Debug a -> Term -> Debug (IO a)
ioaction act t = eval (P.return (pc1 t (lambda_world act)))


ioFunc0 :: (ShowTerm res) => String -> (Debug res) -> Debug (IO res)
ioFunc0 s act = ioaction act (show0 s)

ioFunc1 :: (ShowTerm a, ShowTerm b) => 
  String -> (a -> Debug b) -> a -> Debug (IO b)
ioFunc1 s act x = ioaction (act x) (show1 s x)

ioFunc2 :: (ShowTerm a1, ShowTerm a2, ShowTerm res) => 
           String -> (a1 -> a2 -> Debug res) -> a1 -> a2 -> Debug (IO res)
ioFunc2 s act x1 x2 = ioaction (act x1 x2) (show2 s x1 x2)

ioFunc3 :: (ShowTerm a1, ShowTerm a2, ShowTerm a3, ShowTerm res) => 
           String -> (a1 -> a2 -> a3 -> Debug res) -> a1 -> a2 -> a3 -> Debug (IO res)
ioFunc3 s act x1 x2 x3 = ioaction (act x1 x2 x3) (show3 s x1 x2 x3)

-- not trusted io functions

bangLambda_world :: ShowTerm a => Term -> Debug a -> World -> Debug (IOVal a)
bangLambda_world sx act w = traceFunCall 
  (let {sw=showTerm w} in (consTerm "lambda_world" [sx,sw]))
  (do {v <- act; P.return (IOVal v)})

bangIOaction :: ShowTerm res => Debug res -> Term -> Debug (IO res)
bangIOaction act t = do
  traceFunCall t 
    (P.return (pc1 (consTerm "lambda_world" [t]) (bangLambda_world t act)))

bangIOFunc0 :: (ShowTerm res) => String -> (Debug res) -> Debug (IO res)
bangIOFunc0 s act = bangIOaction act (show0 s)

bangIOFunc1 :: (ShowTerm a, ShowTerm b) => String -> (a -> Debug b) -> a -> Debug (IO b)
bangIOFunc1 s act x = bangIOaction (act x) (show1 s x)

bangIOFunc2 :: (ShowTerm a1, ShowTerm a2, ShowTerm res) => 
           String -> (a1 -> a2 -> Debug res) -> a1 -> a2 -> Debug (IO res)
bangIOFunc2 s act x1 x2 = bangIOaction (act x1 x2) (show2 s x1 x2)

bangIOFunc3 :: (ShowTerm a1, ShowTerm a2, ShowTerm a3, ShowTerm res) => 
           String -> (a1 -> a2 -> a3 -> Debug res) -> a1 -> a2 -> a3 -> Debug (IO res)
bangIOFunc3 s act x1 x2 x3 = bangIOaction (act x1 x2 x3) (show3 s x1 x2 x3)

------------------------------------------------------
-- some type conversions
------------------------------------------------------

dataIntToNum, dataNatToNum :: P.Integral a => Data -> a
dataIntToNum (C1_1 i) = P.negate (dataNatToNum i)
dataIntToNum C0_1     = 0
dataIntToNum (C1_2 i) = dataNatToNum i

dataNatToNum (C1_2 bs) = 2 P.* dataNatToNum bs P.+ 1
dataNatToNum (C1_1 bs) = 2 P.* dataNatToNum bs
dataNatToNum C0_1      = 1

numToDataInt, numToDataNat ::  P.Integral a => a -> Data
numToDataInt n = case P.compare n 0 of
  P.LT -> C1_1 (numToDataNat (P.abs n))
  P.EQ -> C0_1
  P.GT -> C1_2 (numToDataNat (P.abs n))

numToDataNat n = case P.mod n 2 of
              1 -> if m P.== 0 then C0_1 else C1_2 (numToDataNat m)
              0 -> C1_1 (numToDataNat m)
      where m = P.div n 2


instance Convert P.Int where
  toData   = numToDataInt
  fromData = dataIntToNum

instance Convert P.Integer where
  toData   = numToDataInt
  fromData = dataIntToNum

instance Convert P.Float where
  toData   i = PrimFloat i
  fromData (PrimFloat i) = i

instance Convert P.Char where
  toData = PrimChar
  fromData (PrimChar c) = c
  fromData (C _ c1 c2 c3 c4 _) = 
    P.either (P.error "Convert.Char.fromData") P.id (scToTerm c1 c2 c3 c4)

instance Convert a => Convert [a] where
  toData []     = C0_1
  toData (x:xs) = C2_1 (toData x) (toData xs)
  fromData C0_1 = []
  fromData (C2_1 x xs) = fromData x : fromData xs

boolToSuccess :: String -> P.Bool -> Success
boolToSuccess _ P.True  = Success C0_1
boolToSuccess s P.False = Success (failure s)

instance (Convert a,Convert b) => Convert (a,b) where
  toData (x,y) = C2_1 (toData x) (toData y)
  fromData (C2_1 x y) = (fromData x,fromData y)


instance Generator Char where
  generator i = eval (do {r  <- childRef 4;
                          c1 <- genFour r;
                          c2 <- genFour (r P.+ 1);
                          c3 <- genFour (r P.+ 2);
                          c4 <- genFour (r P.+ 3);
                          P.return (Char (C 0 c1 c2 c3 c4 []))})

genFour :: P.Int -> Debug Data
genFour i = eval (do {c1 <- eval (P.return C0_1);
                      c2 <- eval (P.return C0_2);
                      c3 <- eval (P.return C0_3);
                      c4 <- eval (P.return (C0_ 4));
                      mkGenOr i 0 i [c1,c2,c3,c4]})

instance Generator Float where
  generator = P.error "free variable of type Float"

instance (ShowTerm a,Generator a,Generator b) => Generator (Func a b) where
  generator i = P.error "guessing function"

instance (ShowTerm a,Generator b) => ShowTerm (Func a b) where
  showCons (Func t _)    = t
  showCons FuncUneval    = Underscore
  showCons (FuncFail s)  = FailTerm s
  showCons (FuncOr r xs) = OrTerm r (P.map showTerm xs)

-- for untrusted applications
bangApp :: (ShowTerm a,ShowTerm b) => String -> Func a b -> a -> Debug b
bangApp s f x = traceFunCall (consTerm s [showTerm f,showTerm x])
                             (app f x)

-------------------------------------------
-- ShowTerm instances for external types
-------------------------------------------

instance P.Read Int where
  readsPrec p s = [(Int (toData (i::P.Integer)),s') | (i,s') <- P.readsPrec p s ]

instance P.Show Int where
  showsPrec d (Int x) = showsPrec d (fromData x::P.Integer)

instance P.Show Nat where
  showsPrec d (Nat x) = showsPrec d (fromData x::P.Integer)

showConsFloat (Float (PrimFloat f)) = Term (P.show f) []
showConsIO    (Func t _)       = t

fourToInt :: Data -> P.Either [Term] P.Int
fourToInt  C0_1    = P.Right 0
fourToInt  C0_2    = P.Right 1
fourToInt  C0_3    = P.Right 2
fourToInt  (C0_ 4) = P.Right 3
fourToInt  x       = P.Left [showTerm x]

scToTerm :: Data -> Data -> Data -> Data -> P.Either [Term] P.Char
scToTerm f1 f2 f3 f4 
  = chr' ((fourToInt f1**64)+++(fourToInt f2**16)+++(fourToInt f3**4)+++fourToInt f4)
  where 
    P.Left s  ** _  = P.Left s
    P.Right i ** j  = P.Right (i P.* j)
    
    P.Left xs +++ P.Left ys = P.Left (xs P.++ ys)
    P.Right i +++ P.Left s  = P.Left s
    P.Right i +++ P.Right j = P.Right (i P.+ j)
    chr' (P.Right i) = P.Right (Char.chr i)
    chr' (P.Left s)  = P.Left s

showConsChar (Char (PrimChar c)) = Term (P.show c) []
showConsChar (Char (C _ c1 c2 c3 c4 _)) =
  P.either (Term "C") (\c -> Term (P.show c) []) (scToTerm c1 c2 c3 c4)
  

---------------------------------------------------------------------------------------
-- end of include
-- #-----------------------------------------------------------------------------------