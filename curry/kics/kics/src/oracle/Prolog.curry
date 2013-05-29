------------------------------------------------------------------------------
--- A library containing a representation for Prolog programs together
--- with a simple pretty printer.
---
--- @author Michael Hanus
--- @version February 13, 2007
------------------------------------------------------------------------------

module Prolog(PlClause(..),PlGoal(..),PlTerm(..),showPlProg) where

import Char(isAlphaNum,isLower)
import List(union,intersperse)

----------------------------------------------------------------------------
-- Representation of Prolog programs:

data PlClause = PlClause String [PlTerm] [PlGoal]

data PlGoal = PlLit String [PlTerm]
            | PlCond [PlGoal] [PlGoal] [PlGoal]

data PlTerm = PlVar Int
            | PlAtom String
            | PlInt Int
            | PlFloat Float
            | PlStruct String [PlTerm]

--- Shows a Prolog program in standard Prolog syntax.
showPlProg :: [PlClause] -> String
showPlProg clauses = concatMap (showPlClause . optimizeClause) clauses

showPlClause (PlClause pred args []) =
  showPlGoal (PlLit pred args) ++ ".\n"
showPlClause (PlClause pred args body@(_:_)) =
  showPlGoal (PlLit pred args) ++ " :- " ++ showPlGoals body ++ ".\n"

showPlGoals gs = concat (intersperse ", " (map showPlGoal gs))

showPlGoal (PlLit pred args) =
  if pred=="="
  then showPlTerm (args!!0) ++ " = " ++ showPlTerm (args!!1)
  else showPlTerm (PlStruct pred args)
showPlGoal (PlCond cond tgoal fgoal) =
  "(" ++ showPlGoals cond ++ " -> " ++ showPlGoals tgoal ++ " ; " ++
  showPlGoals fgoal ++ ")"

showPlTerm (PlVar i) = if i<26 then [chr (65+i)] else "X"++show i
showPlTerm (PlAtom a) = showPlAtom a
showPlTerm (PlInt i) = show i
showPlTerm (PlFloat f) = show f
showPlTerm (PlStruct f []) = showPlAtom f
showPlTerm (PlStruct f args@(h:t)) =
  if f=="." && length args == 2
  then "[" ++ showPlTerm h ++ "|" ++ showPlTerm (head t) ++ "]"
  else showPlAtom f ++
       "(" ++ concat (intersperse "," (map showPlTerm args)) ++ ")"

showPlAtom a =
  if a=="[]" || (all (\c -> isAlphaNum c || c=='_') a && isLower (head a))
             || all (`elem` specialChars) a
  then a
  else '\'': (concatMap (\c->if c=='\'' then "\\\'" else [c]) a) ++"\'"

specialChars = "+-*/<=>`\\:.?@#$&^~"

----------------------------------------------------------------------------
-- Optimize a Prolog clause: "head :- b1,X=Y,b2" is replaced by
-- "head :- b1,[X/Y]b2" if X does not occur in head and b1

optimizeClause :: PlClause -> PlClause
optimizeClause (PlClause pred args body) =
  PlClause pred args (optimizeBody (unionMap varsOf args) body)

optimizeBody _ [] = []
optimizeBody vars (PlCond cond tgoal fgoal : lits) =
 let ocond = optimizeBody vars cond
     ocvars = union vars (unionMap varsOfLit cond)
     otgoal = optimizeBody ocvars tgoal
     ofgoal = optimizeBody ocvars fgoal
  in PlCond ocond otgoal ofgoal
      : optimizeBody (union ocvars (unionMap varsOfLit (otgoal++ofgoal))) lits
optimizeBody vars (PlLit pred args : lits)
 | pred=="=" && isPlVar (head args) && (varOf (head args) `notElem` vars)
  = optimizeBody (union vars (varsOf (args!!1)))
                 (map (replaceInLit (varOf (head args)) (args!!1)) lits)
 | pred=="=" && isPlVar (args!!1) && (varOf (args!!1) `notElem` vars)
  = optimizeBody (union vars (varsOf (args!!0)))
                 (map (replaceInLit (varOf (args!!1)) (args!!0)) lits)
 | otherwise
  = PlLit pred args : optimizeBody (union vars (unionMap varsOf args)) lits

replaceInLit x y (PlLit pred args) = PlLit pred (map (replaceInTerm x y) args)
replaceInLit x y (PlCond cond tgoal fgoal) =
  PlCond (map (replaceInLit x y) cond)
         (map (replaceInLit x y) tgoal)
         (map (replaceInLit x y) fgoal)

replaceInTerm x y (PlVar   v) = if x==v then y else PlVar v
replaceInTerm _ _ (PlAtom  a) = PlAtom  a
replaceInTerm _ _ (PlInt   i) = PlInt   i
replaceInTerm _ _ (PlFloat f) = PlFloat f
replaceInTerm x y (PlStruct f args) = PlStruct f (map (replaceInTerm x y) args)

varsOfLit (PlLit _ args) = unionMap varsOf args
varsOfLit (PlCond g1 g2 g3) = unionMap varsOfLit (g1++g2++g3)

varsOf (PlVar   v) = [v]
varsOf (PlAtom  _) = []
varsOf (PlInt   _) = []
varsOf (PlFloat _) = []
varsOf (PlStruct _ args) = unionMap varsOf args

isPlVar t = case t of
              PlVar _ -> True
              _       -> False

varOf (PlVar v) = v

unionMap f = foldr union [] . map f
