module ExternalFunctionsOptimizeST where

import Data.Tree
import CurryPrelude
import Curry
{-
optChStore err det br ref bs st = case changeStore ref st of
  Inconsistent             -> err
  Found i                  -> det i (bs!!i) st
  NoBinding _ contSt       -> br contSt
  NewInfo ref st           -> det 0 (head bs) st
  FoundAndNewInfo i ref st -> det i (bs!!i) st

-}
showSearchTree :: Curry a => a -> Result C_String
showSearchTree x st = toCurry (drawTree (mkSearchTree x emptyStore))

mkSearchTree :: Curry a => a -> Result (Tree String)
mkSearchTree x st = case consKind x of
  Val       -> Node "V" (foldCurry (\ xi ts st -> mkSearchTree xi st : ts) [] x st)
  Failed    -> Node "F" []
  Branching -> let ref = orRef x 
                   bs  = branches x in  Node ("Or "++show ref) $
     optChStore
       [Node "F" []]
       (\ i x st -> replicate i (Node "X" []) ++ 
                    mkSearchTree x st :
                    replicate (length bs - i - 1) (Node "X" []))
       (\ st -> map ($st) $ zipWith (descend mkSearchTree ref) [0..] bs)
       ref bs st


descend :: Curry a => (a -> Result b) -> OrRef -> Int -> a 
                   -> (Int -> Store) -> b
descend f r i x st = f x (st i)


countGarbage :: Curry a => a -> Result (T2 C_Int C_Int)
countGarbage x _ = error "countGarbage" --toCurry (countXs emptyStore x)

{-
countXs :: Curry a => Store -> a -> (Integer,Integer)
countXs st x = case consKind x of
  Val       -> foldCurry (\ xi -> plus (countXs st xi)) (0,0) x
  Failed    -> (0,0)
  Branching -> let ref = orRef x 
                   bs  = branches x in  
     case fromStore st ref of 
       Nothing -> foldr plus (1,0) (zipWith (descend countXs st ref) [0..] bs)
       Just i  -> plus (0,1) (countXs st (bs !! i))
  where
    plus (x1,x2) (y1,y2) = (x1+y1,x2+y2)

-}

optimizeST ::  Curry a => a -> Result a
optimizeST x st = error "optimizeST" --opt (maybe emptyStore id st) x

{-
opt ::  Curry a => Store -> a -> a
opt st x = case consKind x of
  Val       -> propagate (opt st) x
  Failed    -> x
  Branching -> let ref = orRef x 
                   bs  = branches x in  
     case fromStore st ref of 
       Nothing -> branching ref (zipWith (descend opt st ref) [0..] bs)
       Just i  -> opt st (bs !! i)






-}