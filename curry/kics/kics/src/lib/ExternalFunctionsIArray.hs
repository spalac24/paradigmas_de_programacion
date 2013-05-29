module ExternalFunctionsIArray where

import Curry
import CurryPrelude
import ExternalDataIArray

import qualified Array as A

------------------------------------------------------------
-- in order to use haskells array function, all assocs and 
-- all indexes within have to be ground values
------------------------------------------------------------

ensureIdxs :: (Curry a,Curry b) => ([(Integer,a)] -> b) -> List(T2 C_Int a) -> b
ensureIdxs f = ghnf0 (ensureList []) 
 where 
   ensureList xs List     = f (reverse xs)
   ensureList xs (y :< ys) = ghnf0 (ensureTup xs ys) y
 
   ensureTup xs ys (T2 idx e) = gnf0 (ensureIdx xs ys e) idx

   ensureIdx xs ys e i =  ghnf0 (ensureList ((fromCurry i,e):xs)) ys

------------------------------------------------------------

array :: (Curry t0) => (T2 C_Int C_Int) -> (List (T2 C_Int t0)) -> C_Array t0
array st tup xs = gnf fromCurryBounds st tup 
  where
    fromCurryBounds bounds = ensureIdxs (toArray (chBounds bounds)) xs
    toArray bounds xs = C_Array (A.array bounds xs)

(!) :: (Curry t0) => (C_Array t0) -> C_Int -> t0
(!) = ghnf02 lup 
  where
    lup (C_Array a) i = a A.! (fromCurry i)


bounds :: (Curry t0) => C_Array t0 -> T2 C_Int C_Int
bounds = ghnf0 (\ (C_Array arr) -> hcBounds (A.bounds arr))

assocs :: (Curry t0) => (C_Array t0) -> List (T2 C_Int t0)
assocs = ghnf0 (\ (C_Array arr) -> hcAssocs (A.assocs arr))

(//) :: (Curry t0) => (C_Array t0) -> (List (T2 C_Int t0)) -> C_Array t0
arr // xs = ghnf0 (\ (C_Array arr') -> ensureIdxs (change arr') xs) arr
  where
    change a ys = C_Array (a A.// ys)
{-
accum :: (Curry t0,Curry t1) => (Prim (t0 -> Prim (t1 -> t0))) -> (C_Array t0) -> (List (T2 C_Int t1)) -> C_Array t0
accum f a xs = let f' x y = apply (apply f x) y in ghnf0 (ensureXs f') a 
  where
    ensureXs f' a' = ensureIdxs (appAccum f' a') xs
    appAccum f' (C_Array ha) xs' = C_Array (A.accum f' ha xs')

-}