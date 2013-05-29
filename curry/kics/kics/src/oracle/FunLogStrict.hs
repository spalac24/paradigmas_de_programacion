module FunLogStrict where

import DebuggerMonad
import Store
import Term
import Control.Monad.State (liftIO)
import Char

--------------------------------------
-- the logic part called from outside
--------------------------------------

free :: Generator a => Debug a
free = nextDeRef 0 >>= generator

mkOr :: Convert a => a -> a -> Debug a
mkOr x1 x2 = ors Nothing [x1,x2]

mkGenOr :: Convert a => Int -> Int -> Int -> [a] -> Debug a
mkGenOr r s i xs = return (fromData (Or (mkRef r s i) (map toData xs)))

ors :: Convert a => Maybe Int -> [a] -> Debug a
ors b xs = do
  ref <- nextRef b 0 
  return (fromData (Or ref (map toData xs)))

childRef :: Int -> Debug Int
childRef 0 = return 0
childRef n = nextDeRef n


mapOr :: (Debug Data -> Debug Data) -> (Data -> Debug Data) -> OrRef -> [Data] -> Debug Data
mapOr ev cont ref bs = do
  st <- getStore
  manipulateStore
   (return (failure "mapOr"))
   (\ x _ -> cont x)
   (\ _ -> liftCase ev cont ref bs)
   (continueWith cont)
   ref bs st

continueWith :: (Convert a,Convert b) => (a -> Debug b) -> OrRef -> Data -> Store -> Debug b
continueWith cont ref x st = do
      putStore st 
      y <- cont (fromData x)
      return (fromData (Or ref [toData y]))

onData :: (Convert a,Convert b) => (a -> Debug b) -> Data -> Debug Data
onData f x = f (fromData x) >>= return . toData

liftCase :: (Debug Data -> Debug Data) -> (Data -> Debug Data) -> OrRef -> [Data] -> (Int -> Store) -> Debug Data
liftCase ev f r xs contSt = ev (do
  st <- getStore
  ys <- sequence 
    (zipWith (\ x i -> putStore (contSt i) >> f x) xs [0..])
  putStore st
  return (Or r ys))

genericCase :: (Convert a,Convert b) => (a -> Debug b) -> a -> Debug b
genericCase f x = case toData x of
  d@(Fail _) -> return (fromData d)
  Or r xs    -> ghnf f  (fromData (Or (narrowOrRef r) xs))
  Uneval     -> return (fromData (Fail "NonTermination"))
  _          -> return (fromData (Fail "no match"))

hnf,ghnf,nf,gnf :: (Convert a,Convert b) => (a -> Debug b) -> a -> Debug b
hnf  = evaluate True False 
ghnf = evaluate True True
nf f x  = nfDataT False x >>= hnf f 
gnf f x = nfDataT True x  >>= ghnf f 

evaluate :: (Convert a,Convert b) => Bool -> Bool -> (a -> Debug b) -> a -> Debug b
evaluate count mode f x = case toData x of
  d@(Fail _) -> return (fromData d)
  Or ref bs  -> do
    st <- getStore
    manipulateStore
      (return (fromData (failure "=:=")))
      (\ x' _ -> contEval (fromData x'))
      (\ ref' contSt -> if mode || not (isGenerator ref)
        then liftCase cod contEval' (narrowOrRef ref) bs contSt >>= 
             return . fromData
        else f (fromData (Or ref' bs)))
      (continueWith contEval)
      ref bs st
  _          -> f x
 where
   contEval  = evaluate count mode f
   contEval' = onData contEval
   cod x = if count then eval x else id x


------------------------------------------------------
-- normal form (is this faster than with sel/cons?
------------------------------------------------------

nfData :: Bool -> Data -> Debug Data
nfData b x@(Or r xs) = if b || not (isGenerator r)
                       then mapOr id (nfData b) r xs
                       else return x
nfData b (C1_1 x)  = nfData1 b x C1_1
nfData b (C1_2 x)  = nfData1 b x C1_2
nfData b (C1_3 x)  = nfData1 b x C1_3
nfData b (C1_ i x) = nfData1 b x (C1_ i)
nfData b (C2_1 x y) = nfData2 b x y C2_1
nfData b (C2_2 x y) = nfData2 b x y C2_2 
nfData b (C2_3 x y) = nfData2 b x y C2_3
nfData b (C2_ i x y) = nfData2 b x y (C2_ i) 
nfData b (C3_1 x y z) = nfData3 b x y z C3_1
nfData b (C3_2 x y z) = nfData3 b x y z C3_2
nfData b (C3_3 x y z) = nfData3 b x y z C3_3
nfData b (C3_ i x y z) = nfData3 b x y z (C3_ i)
nfData m (C i a b c d xs) = 
  nfData' (C i undefined undefined undefined undefined []) (a:b:c:d:xs)
  where
    nfData' (C i _ _ _ _ xs) [] = case reverse xs of
      (a':b':c':d':xs') -> return (C i a' b' c' d' xs')
    nfData' (C i a b c d xs) (y:ys) = 
      nfData m y >>= hnf' m (\x-> nfData' (C i a b c d (x:xs)) ys)
nfData _ x = return x

nfData1 :: Convert a => Bool -> Data -> (Data -> a) -> Debug a
nfData1 b x f = nfData b x >>= hnf' b (return . f)

nfData2 :: Convert a => Bool -> Data -> Data -> (Data -> Data -> a) -> Debug a
nfData2 b x y f = nfData b x >>= hnf' b (\ x' -> nfData b y >>= hnf' b (return . f x'))

nfData3 :: Convert a => Bool -> Data -> Data -> Data -> (Data -> Data -> Data -> a) -> Debug a
nfData3 b x y z f = nfData b x >>= 
  hnf' b (\ x' -> nfData b y >>= 
       hnf' b (\ y' -> nfData b z >>= 
            hnf' b (return . f x' y')))

nfDataT :: (Convert a) => Bool -> a -> Debug a
nfDataT b x = nfData b (toData x) >>= hnf' b (return . fromData)
 
hnf' :: (Convert a,Convert b) => Bool -> (a -> Debug b) -> a -> Debug b
hnf' = evaluate False 


------------------------------------------------------
-- generic traversals
------------------------------------------------------

trData :: (Char -> a) -> (Float -> a) -> (Term -> () -> a)
       -> (Term -> (Data -> Debug Data) -> a) -> a -> (String -> a)
       -> (OrRef -> [a] -> a) -> (Int -> [a] -> a) -> Data -> a
trData charf floatf primf funcf unevalf failf orf consf x =
  case x of
    PrimChar c   -> charf c
    PrimFloat f  -> floatf f
    Prim t u     -> primf t u
    PrimFunc t f -> funcf t f
    Uneval       -> unevalf 
    Fail s       -> failf s
    Or r ds      -> orf r (map rek ds)
    _            -> let (j,xs) = sel x 
                     in consf j (map rek xs)
  where
    rek = trData charf floatf primf funcf unevalf failf orf consf

cleanTerm :: Term -> Debug Term
cleanTerm term = do
  toclean <- isLogProg
  if toclean 
   then trTerm (return Underscore) (return . FailTerm) orf consf (bubble term)
   else return term
    where
    orf  r xs = do 
     st <- getStore
     manipulateStore
       (return (FailTerm "=:="))
       const
       (\ _ contSt -> sequence 
        (zipWith (\ act i -> putStore (contSt i) >> act) xs [0..]) >>= \ xs' ->
          putStore st >>
          case filter (trTerm True (\_->False) (\_ _->True) (\_ _->True)) xs' of
            []  -> return (FailTerm "failed")
            [x] -> return x
            xs''-> return (OrTerm r xs''))
       (\ ref' x st' -> x)
       r xs st
    
    consf i xs = sequence xs >> sequence xs >>= return . Term i


intToFour :: Int -> Data
intToFour  0 = C0_1
intToFour  1 = C0_2
intToFour  2 = C0_3
intToFour  3 = C0_ 4

charToSc ::  Char -> Data
charToSc c = C 0 (intToFour d64) (intToFour d16) (intToFour d4) (intToFour m4) []
  where
    o = ord c
    (d64,m64) = divMod o 64
    (d16,m16) = divMod m64 16
    (d4,m4)   = divMod m16 4

instance Convert Bool where
  fromData C0_1 = False
  fromData C0_2 = True
  toData False = C0_1
  toData True  = C0_2


eqData :: Bool -> Data -> Data -> Debug Data
eqData b x y = hnf' b (\x' -> hnf' b (geqData b x') y) x

geqData :: Bool -> Data -> Data -> Debug Data
geqData _ (PrimChar c1)  (PrimChar c2)  = return (toData (c1==c2))
geqData b (PrimChar c1)  c2@(C _ _ _ _ _ _) = geqData b (charToSc c1) c2
geqData b c1@(C _ _ _ _ _ _)  (PrimChar c2) = geqData b c1 (charToSc c2)
geqData _ (PrimFloat f1) (PrimFloat f2) = return (toData (f1==f2))
geqData _ (Prim t1 _) (Prim t2 _) = return (toData (t1==t2))
geqData _ (PrimFunc _ _) (PrimFunc _ _) = return (failure "comparing FUNCTION")
geqData _ x@(Fail _) _ = return x
geqData _ _ x@(Fail _) = return x
geqData b x@(Or _ _) (Or ry ys) = hnf' b unify x
  where
    unify (Or rx xs) 
      | drx==dry  = return (toData True)
      | otherwise = do
             --liftIO (print (ax,bx,drx))
             --liftIO (print (ay,by,dry))
             return (Or (equalFromTo ax bx drx ay by dry) [toData True])
         where (ax,bx,drx)=genInfo rx
               (ay,by,dry)=genInfo ry
geqData b (Or r xs) y = do 
  zs <- sequence (map (flip (eqData b) y) xs)
  return (Or r zs)
geqData b x (Or r ys) = do 
  zs <- sequence (map (eqData b x) ys)
  return (Or r zs)
geqData _ Uneval _ = return Uneval
geqData _ _ Uneval = return Uneval
geqData b c1 c2 = 
  let (i1,args1) = sel c1
      (i2,args2) = sel c2
   in if   i1==i2 
      then zipEqData b args1 args2
      else return (toData False)

zipEqData :: Bool -> [Data] -> [Data] -> Debug Data
zipEqData b [] [] = return (toData True)
zipEqData b [] _  = return (toData False)
zipEqData b _ []  = return (toData False)
zipEqData b (x:xs) (y:ys) = eqData b x y >>=
  hnf' True (\ z -> case z of
                      Uneval -> zipEqData b xs ys
                      _ -> if fromData z then zipEqData b xs ys else return z) 
{-
generatorInfo :: [Data] -> Maybe (Int,Int)
generatorInfo xs 
 | null refs = Nothing
 | otherwise = Just (foldr1 min refs,foldr1 max refs)
  where
    args Uneval = []
    args x      = snd (sel x)

    refs = concatMap (\ x -> map (\ (Or r _) -> deref r) (args x)) xs
-}

    
