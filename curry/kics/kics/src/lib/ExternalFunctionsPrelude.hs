{-# OPTIONS -cpp  #-} 
{-# LANGUAGE RankNTypes, 
             ScopedTypeVariables, 
             MultiParamTypeClasses, 
             FlexibleInstances #-}

module ExternalFunctionsPrelude where

import Prelude hiding ((==),(>>=),return,catch)
import qualified Prelude ((==),(>>=),return) 
import Data.Char (ord,chr)
import Curry
import List
import DataPrelude 
--import BaseCurry (op_61_58_61,op_38,op_61_61,c_error)
import System.IO.Unsafe
import System.IO
import InstancesPrelude
import Data.IORef

#if __GLASGOW_HASKELL__ >= 610
import Control.OldException (catch)
#else
import Control.Exception (catch)
#endif

infix  4 ===
infixr 0 & 

-----------------------------------------------------------------------
-- IO starter
-----------------------------------------------------------------------

optChangeStore :: a -> (b -> Store -> a) -> ((Int -> Store) -> a) 
               -> OrRef -> Branches b -> Store -> a
optChangeStore err det br = 
  manipulateStore err det (\ _ -> br) (\ _ -> det)

curryIO :: Curry a => (Result (C_IO a)) -> IO a
curryIO x = let st = emptyStore in ioStart st (x st)

curryIOVoid :: Curry a => (Result (C_IO a)) -> IO ()
curryIOVoid x = curryIO x >> Prelude.return ()

ioStart :: Curry a => Store -> C_IO a -> IO a
ioStart st (C_IO act)            = act st Prelude.>>= curryDo st
ioStart _  (C_IOFail es)         = printExceptions es
ioStart st (C_IOOr ref bs)       =
  optChangeStore 
    (printExceptions (curryError "ioStart"))
    (flip ioStart)
    (\st -> searchValC_IO [] (zipWith (mkChoice st) [0..] bs))
    ref 
    bs 
    st

curryDo :: Curry a => Store -> IOVal a -> IO a
curryDo _  (IOVal x)        = Prelude.return x
curryDo _  (IOValFail es)   = printExceptions es
curryDo st (IOValOr ref bs) =     
  optChangeStore 
    (printExceptions (curryError "curryDo")) 
    (\ x st -> x Prelude.>>= curryDo st)
    (\st -> searchIOVal [] (zipWith (mkChoice st) [0..] bs))
    ref 
    bs
    st

mkChoice :: BaseCurry a => (Int -> Store) -> Int -> a -> (Store,a)
mkChoice st i x = (st i,x)

searchValC_IO :: Curry a => [C_Exceptions] -> [(Store,C_IO a)] -> IO a
searchValC_IO es []     = 
  mapM_ printException es >> error "no solution in branching io value"
searchValC_IO _ ((st,C_IO act)   : _)  = act st Prelude.>>= curryDo st
searchValC_IO es ((_ ,C_IOFail e@(ErrorCall _)) : xs) = 
  searchValC_IO (e:es) xs
searchValC_IO es ((_ ,C_IOFail e) : xs) = searchValC_IO es xs
searchValC_IO es ((st,C_IOOr ref bs) : xs) =  
  optChangeStore
    (searchValC_IO es xs)
    (\ x st -> case x of
        C_IO act   -> act st Prelude.>>= curryDo st
        C_IOOr _ _ -> searchValC_IO es ((st,x):xs)
        C_IOFail _ -> searchValC_IO es xs)
    -- switch arguments of (++) for breadth first (bad.), cf. also below
    (\ st -> searchValC_IO es (zipWith (mkChoice st) [0..] bs ++ xs))
    ref bs st

searchIOVal :: Curry a => [C_Exceptions] -> [(Store,IO (IOVal a))] -> IO a
searchIOVal es []                = 
  mapM_ printException es >> error "no solution in branching io value"
searchIOVal es ((st,act) : stacts) = do
  x <- act
  case x of
    IOVal a        -> Prelude.return a
    IOValFail e@(ErrorCall _) -> searchIOVal (e:es) stacts
    IOValFail _    -> searchIOVal es stacts
      -- switch arguments of (++) for breadth first (bad.)
    IOValOr ref bs -> 
      optChangeStore 
        (searchIOVal (curryError "inconsistent Store":es) stacts)
        (\ x st -> searchIOVal es ((st,x):stacts))
        (\st -> searchIOVal es (zipWith (mkChoice st) [0..] bs ++ stacts))
        ref bs st

-- this is the place to change for implicit breadth first search
searchVal :: (Store -> a -> b) -> Store -> OrRef -> Branches a -> b
searchVal cont store ref [] =  error "top io failed"
searchVal cont store ref (x:bs) = cont store x

printException :: C_Exceptions -> IO ()
printException (PatternMatchFail s) = 
  hPutStrLn stderr ("non-exhaustive patterns in function "++s)
printException (AssertionFailed s) = 
  hPutStrLn stderr ("assertion failed with label "++s)
printException (IOException s) = 
  hPutStrLn stderr ("io exception: " ++ s)
printException (ErrorCall s) = 
  hPutStrLn stderr ("error : " ++s)
printException PreludeFailed = hPutStrLn stderr "Prelude.failed"

printExceptions :: C_Exceptions -> IO a
printExceptions e = 
  printException e >> error "program error"

-----------------------------------------------------------------------
-- Int and Float
-----------------------------------------------------------------------


instance Eq C_Int where
  x == y = (fromCurry x::Integer) Prelude.== fromCurry y

instance Num C_Int where
  fromInteger x = toCurry x
  x + y = toCurry ((fromCurry x::Integer) + fromCurry y)
  x * y = toCurry ((fromCurry x::Integer) * fromCurry y)
  
  abs (C_Neg x) = C_Pos x
  abs x = x

  signum (C_Pos _) = C_Pos C_IHi
  signum (C_Neg _) = C_Neg C_IHi
  signum x = x

instance Eq a => Eq (Prim a) where
  (PrimValue x) == (PrimValue y) = x Prelude.== y

instance (Num a) => Num (Prim a) where
  (PrimValue x) + (PrimValue y) = PrimValue (x+y)
  (PrimValue x) - (PrimValue y) = PrimValue (x-y)
  (PrimValue x) * (PrimValue y) = PrimValue (x*y)
  negate (PrimValue x) = PrimValue (negate x)
  abs    (PrimValue x) = PrimValue (abs x)
  signum (PrimValue x) = PrimValue (signum x)
  fromInteger x = PrimValue (fromInteger x)

instance Enum a => Enum (Prim a) where 
    toEnum i = PrimValue (toEnum i)
    fromEnum (PrimValue x) = fromEnum x

instance Real a => Real (Prim a) where 
    toRational (PrimValue x) = toRational x

instance Integral a => Integral (Prim a) where 
    quotRem (PrimValue x) (PrimValue y) = let (x',y') = quotRem x y in 
                                           (PrimValue x', PrimValue y')
    toInteger (PrimValue x) = toInteger x

instance Ord a => Ord (Prim a) where
   (PrimValue x) <= (PrimValue y) = x<=y

-----------------------------------------------------------------------
-- T0 is unit (), needed for IO primitives
-----------------------------------------------------------------------

instance ConvertCH T0 () where
  toCurry () = T0
  fromCurry T0 = () 

instance (ConvertCH a ha, ConvertCH b hb) => ConvertCH (T2 a b) (ha,hb) where
  toCurry (x,y) = T2 (toCurry x) (toCurry y)
  fromCurry (T2 x y) = (fromCurry x, fromCurry y) 

instance (ConvertCH a ha, ConvertCH b hb, ConvertCH c hc) =>
         ConvertCH (T3 a b c) (ha,hb,hc) where
  toCurry (x,y,z) = T3 (toCurry x) (toCurry y) (toCurry z)
  fromCurry (T3 x y z) = (fromCurry x, fromCurry y, fromCurry z) 

-----------------------------------------------------------------------
-- Maybe
-----------------------------------------------------------------------

instance (ConvertCH a b) => ConvertCH (C_Maybe a) (Maybe b) where
  fromCurry C_Nothing  = Nothing
  fromCurry (C_Just x) = Just (fromCurry x)

  toCurry Nothing  = C_Nothing
  toCurry (Just x) = C_Just (toCurry x)


---------------------------------------------------------------------------------
-- external functions for Prelude
---------------------------------------------------------------------------------

($#) :: (Curry a, Curry b) => Prim (a -> Result b) -> a -> Result b
($#) cont x = prepApply ghnfCTC x cont 

($!) :: (Curry a,Curry b) => Prim (a -> Result b) -> a -> Result b
($!) cont x = prepApply hnfCTC x cont

($!!) :: (Curry a, Curry b) => Prim (a -> Result b) -> a -> Result b
($!!) cont x = prepApply nfCTC x cont

($##) :: (Curry a, Curry b) => Prim (a -> Result b) -> a -> Result b
($##) cont x = prepApply gnfCTC x cont

prim_error :: Curry a => C_String -> Result a
prim_error s _ = Curry.failed (ErrorCall (fromCurry s))

failed :: Curry a => Result a
failed _ = Curry.failed PreludeFailed 

(==) :: Curry a => a -> a -> Result C_Bool
(==) = genEq 

prim_ord :: C_Char -> Result C_Int
prim_ord cc _ = toCurry (ord (fromCurry cc))

prim_chr :: C_Int -> Result C_Char
prim_chr ci _ = toCurry (chr (fromCurry ci))

(===) :: Curry a => a -> a -> Result C_Bool --C_Success
(===) = genStrEq
 
success :: C_Success
success = C_Success

--concAnd' x y st = startBreadth [x,y] st

(&) :: C_Success -> C_Success -> Result C_Success
 -- (&) x y st = boolToSuccess (concAnd' (successToBool x) (successToBool y) st)
(&) x y st = boolToSuccess 
               (concAnd (successToBool x st) 
                        (successToBool y st) st) st

boolToSuccess C_True            _  = C_Success
boolToSuccess C_False           _  = C_SuccessFail (ErrorCall "&")
boolToSuccess (C_BoolFail e)    _  = C_SuccessFail e
boolToSuccess (C_BoolOr r xs)   st = mapOr boolToSuccess r xs st


successToBool :: C_Success -> Result C_Bool
successToBool C_Success                _  = C_True
successToBool (C_SuccessFail e)        _  = C_BoolFail e
successToBool (C_SuccessOr r xs)       st = mapOr successToBool r xs st

--andBreadth :: List C_Bool -> Result C_Bool
--andBreadth xs st = startBreadth (toHaskellList xs) st

-- TODO: C_IO without State??? also other io-functions.
(>>=) :: (Curry a,Curry b) => C_IO a -> Prim (a -> Result (C_IO b)) -> Result (C_IO b)
(>>=) m f _ = C_IO (hnfCTC (exec f) m)

exec :: (Curry a,Curry b) => Prim (a -> Result (C_IO b)) -> C_IO a -> Result (IO (IOVal b))
exec f (C_IO m) st = m st Prelude.>>= \ x -> prim_do f x st

-- if it wasn't io, we could just write 
--exec f st (C_IO m) = m st Prelude.>>= hnfCTC (fromIOVal f) st
-- with fromIOVal simply being
--fromIOVal::(Curry a,Curry b)=>Prim(a->C_IO b)->State->IOVal a->IO(IOVal b)
--fromIOVal f st (IOVal res) = hnfCTC exec2 st (apply f res)
-- and everything would work fine. But then for the susp and or cases
-- we would use unsafe io...
-- Thus, prim_do has to copy the code of ctcStore False
-- IMPORTANT: This code should correspond to BaseCurry.ctcStore

prim_do ::  (Curry a,Curry b) => 
            Prim (a -> Result (C_IO b)) -> IOVal a ->  Result (IO (IOVal b))
prim_do f x state = case x of
  IOVal res      -> hnfCTC exec2 (apply f res state) state
  IOValFail es   -> Prelude.return (IOValFail es)
  IOValOr ref bs -> 
    optChangeStore
       (Curry.failed $ curryError "prim_do")
       (\ x st -> x Prelude.>>= \ x' -> prim_do f x' st)
       (\ st -> Prelude.return (IOValOr ref 
                  (zipWith (\ i x -> x Prelude.>>= \ x' -> cont x' (st i)) 
                           [0..] bs)))
       ref bs state
  where
    cont x st = prim_do f x st

exec2 :: C_IO b -> Result (IO (IOVal b))
exec2 (C_IO f) = f 


return :: a -> Result (C_IO a)
return a _ = C_IO (\ _ -> Prelude.return (IOVal a))

prim_putChar :: C_Char -> Result (C_IO T0)
prim_putChar = ioFunc1 putChar 

getChar :: Result (C_IO C_Char)
getChar = ioFunc0 Prelude.getChar
 
prim_readFile :: C_String -> Result (C_IO C_String)
prim_readFile = ioFunc1 readFile 

prim_writeFile :: C_String -> C_String -> Result (C_IO T0)
prim_writeFile = ioFunc2 writeFile 

prim_appendFile :: C_String -> C_String -> Result (C_IO T0)
prim_appendFile = ioFunc2 appendFile 

catchFail :: Curry a => C_IO a -> C_IO a -> Result (C_IO a)
catchFail (C_IO act) err _ = 
  C_IO (\ st -> catch (act st) (const (hnfCTC exec2 err st)))
catchFail (C_IOFail _) err _ = err
catchFail (C_IOOr ref bs) err st =
  optChangeStore 
    err
    (flip catchFail err)
    (\st -> searchValCatch (zipWith (mkChoice st) [0..] bs) err)
    ref bs st

searchValCatch :: Curry a => [(Store,C_IO a)] -> C_IO a -> C_IO a
searchValCatch []     err = err
searchValCatch ((st,C_IO act)   : _)  err = catchFail (C_IO act) err st
searchValCatch ((_ ,C_IOFail _) : xs) err = searchValCatch xs err
searchValCatch ((st,C_IOOr ref bs) : xs)  err =  
  optChangeStore 
    (searchValCatch xs err)
    (\ x st -> catchFail x err st)
    (\ st -> searchValCatch (zipWith (mkChoice st) [0..] bs ++ xs) err)
    ref bs st





prim_show :: (Show a,Curry a) => a -> Result C_String
prim_show x _ = toCurry (show x)

getSearchTree :: Curry a => a -> Result (C_IO (C_SearchTree a))
getSearchTree x _ = C_IO (\ state -> Prelude.return (IOVal (searchTr x state)))

 
searchTr :: Curry a => a -> Result (C_SearchTree a)
searchTr x state = transVal (nfCTC (nfCTC const) x state)
  where
    transVal x = case consKind x of
                   Val       -> C_Value x
                   Failed    -> C_Fail
                   Branching 
                     | isGenerator (orRef x) -> C_Value x
                     | otherwise -> transBranching (branches x)

    transBranching []         = C_Fail
    transBranching [x]        = transVal x
    transBranching xs@(_:_:_) = C_Choice (fromHaskellList (map transVal xs))

{-
toData :: Curry a => a -> Result C_Data
toData _ st = prim_error (toCurry "toData not implemented") st --ctcStore True (toC_Term True) Nothing


toNumData :: Curry a => a -> Result C_NumData
toNumData _ st = prim_error (toCurry "toNumData not implemented") st
  --ctcStore True (\ store x -> (conv2num (toC_Term True store x))) Nothing



cmap _ List = List
cmap f (x :< xs) = f x :< cmap f xs

fromData :: Curry a => C_Data -> Result a
fromData _ st = prim_error (toCurry "fromData not implemented") st --fromC_Term
-}

prepApply :: (BaseCurry a,BaseCurry b) => 
  ((b -> Result a) -> b -> Result a) -> b -> (Prim (b -> Result a)) -> Result a
prepApply  prep x (PrimValue f)     st = prep f x st
prepApply  prep x (PrimOr r bs)     st = mapOr (prepApply prep x) r bs st
prepApply  _    _  cont             _  = patternFail "Prelude.prepApply" cont

--apply :: (Curry b, Curry (Prim (a -> b))) => Prim (a -> b) -> a -> b
apply (PrimValue f)     x st = f x st
apply (PrimOr r bs)     x st = mapOr (\ f -> apply f x) r bs st
apply cont              _ st = patternFail "Prelude.apply" cont

-- these functions are employed for higher order
pf :: Curry b => (a -> Result b) -> Prim (a -> Result b)
pf = PrimValue 

pc :: Curry b => (a -> b) -> (Prim (a -> Result b))
pc f = PrimValue (\ x _  -> f x)

pa :: Curry c => (a -> Prim (b -> Result c)) -> Prim (a -> Result (Prim (b -> Result c)))
pa f = PrimValue (\ x _  -> f x)

cp :: (b -> c) -> (a -> b) -> a -> c
cp f g x = f (g x)


cond :: Curry a => C_Success -> a -> Result a
cond C_Success  x _ = x
cond (C_SuccessOr r bs)     x st = mapOr (\ c -> cond c x) r bs st
cond x _ _ = patternFail "Prelude.cond" x


ifVar :: (Curry a,Curry b) => b -> a -> a -> a
ifVar = error "ifVar not implemented"

---------------------------------------------
-- to ease connecting external functions 
---------------------------------------------

extFunc1 :: (Curry a,Curry d,ConvertCH a b,ConvertCH d c) => (b->c) -> a -> Result d
extFunc1 f = gnfCTC (\ x' _ -> toCurry (f (fromCurry x'))) 

extFunc2 :: (Curry a, Curry c,Curry f,ConvertCH a b,ConvertCH c d,ConvertCH f e) => 
            (b->d->e) -> a -> c -> Result f
extFunc2 f x y = 
  gnfCTC (\x'->gnfCTC (\ y' _ -> toCurry (f (fromCurry x') (fromCurry y'))) y) x

extFunc3 :: (Curry c1, Curry c2, Curry c3, Curry cv,
             ConvertCH c1 h1,ConvertCH c2 h2,ConvertCH c3 h3,ConvertCH cv hv) => 
            (h1->h2->h3->hv) -> c1 -> c2 -> c3 -> Result cv
extFunc3 f x y z = 
  gnfCTC (\x' ->
  gnfCTC (\y' -> 
  gnfCTC (\z' _ -> toCurry (f (fromCurry x') (fromCurry y') (fromCurry z'))) z ) y) x

extFunc4 :: (Curry c1, Curry c2, Curry c3, Curry c4, Curry cv,
             ConvertCH c1 h1,ConvertCH c2 h2,ConvertCH c3 h3,ConvertCH c4 h4,ConvertCH cv hv) => 
            (h1->h2->h3->h4->hv) -> c1 -> c2 -> c3 -> c4 -> Result cv
extFunc4 f x1 x2 x3 x4 = 
  gnfCTC (\x1' ->
  gnfCTC (\x2' -> 
  gnfCTC (\x3' -> 
  gnfCTC (\x4' _ -> toCurry (f (fromCurry x1') (fromCurry x2') (fromCurry x3') (fromCurry x4'))) 
         x4) x3) x2) x1


hnf2 :: (Curry a, Curry b,Curry c) => (a->b->c) -> a -> b -> Result c
hnf2 f x y = hnfCTC (\ x' -> hnfCTC (\ y' _ -> f x' y') y) x

ioFunc0 :: (Curry b,ConvertCH b a) => IO a -> Result (C_IO b)
ioFunc0 iof _ = C_IO (\ _ -> iof Prelude.>>= \hv -> Prelude.return (IOVal (toCurry hv)))


ioFunc1 :: (Curry a,Curry d,ConvertCH a b,ConvertCH d c) => (b->IO c) -> a -> Result (C_IO d)
ioFunc1 iof x _ = C_IO (\ _ ->
           iof (fromCurry x) Prelude.>>= \hv ->
           Prelude.return (IOVal (toCurry hv)))

ioFunc2 :: (Curry a, Curry c,Curry f,ConvertCH a b,ConvertCH c d,ConvertCH f e) => 
            (b->d->IO e) -> a -> c -> Result (C_IO f)
ioFunc2 iof x y _ = C_IO (\ _ ->
           iof (fromCurry x) (fromCurry y) Prelude.>>= \hv ->
           Prelude.return (IOVal (toCurry hv)))

ioFunc3 iof x y z _ = C_IO (\ _ ->
           iof (fromCurry x) (fromCurry y) (fromCurry z) Prelude.>>= \hv ->
           Prelude.return (IOVal (toCurry hv)))

ghnfCTC2 :: (Curry a, Curry b,Curry c) => (a->b->c) -> a -> b -> Result c
ghnfCTC2 f x y = ghnfCTC (\x'-> ghnfCTC (\ y' _ -> f x' y') y) x



(=:<=) = error "function patterns not implemented"
