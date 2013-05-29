--- Auxiliary functions for program transformations.
---
--- @author Sebastian Fischer
--- @version November 2006
module TransTools where

import Maybe
import FlatCurry
import FlatCurryGoodies


--- The module name of the prelude.
prelude = "Prelude"

--- Perform transformation on expressions as long as possible.
whilePossible :: (Expr -> Maybe Expr) -> Expr -> Expr
whilePossible step e = maybe e (whilePossible step) (step e)

--- Replace variables in expression w.r.t. given environment.
replace :: [(Int,Expr)] -> Expr -> Expr
replace ves = updVars (\n -> maybe (Var n) id (lookup n ves))

--- Maps a maybe function to a list of values. 
--- Returns <code>Nothing</code> only if all results are <code>Nothing</code>.
--- Returns modifications and/or old values otherwise.
mapMaybeKeep :: (a -> Maybe a) -> [a] -> Maybe [a]
mapMaybeKeep f xs
  | all isNothing (map snd xms) = Nothing
  | otherwise = Just (map merge xms)
 where
  xms = map (\x -> (x,f x)) xs
  merge (x,m) = maybe x id m

type TransExprRec a b = 
  {Var    :: VarIndex -> a,
   Lit    :: Literal  -> a,
   Comb   :: CombType -> QName -> [a] -> a,
   Let    :: [(Int,a)] -> a -> a,
   Free   :: [Int] -> a -> a,
   Or     :: a -> a -> a,
   Case   :: CaseType -> a -> [b] -> a,
   Branch :: Pattern -> a -> b }

defTrExpr :: TransExprRec Expr BranchExpr
defTrExpr = {Var=Var,
             Lit=Lit,
             Comb=Comb,
             Let = Let,
             Free=Free,
             Or=FlatCurry.Or,
             Case=Case,
             Branch=Branch}

trExpr :: TransExprRec a b -> Expr -> a
trExpr rec = 
  FlatCurryGoodies.trExpr (rec->Var)  (rec->Lit)  (rec->Comb) 
                          (rec->Let)  (rec->Free) (rec->Or)
                          (rec->Case) (rec->Branch)
