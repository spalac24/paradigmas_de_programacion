------------------------------------------------------------------------------
--- The front-end of the debugging tool.
--- Its reads a Curry program (containing the main function)
--- together with all imported modules and returns a FlatCurry program
--- containing all accessible functions.
--- In addition, all non-recursive lets are replaced by auxiliary functions.
---
--- @author Michael Hanus
--- @version February 13, 2007
------------------------------------------------------------------------------

module ReadFlat(readCompactFlatCurry) where

import FlatCurry
import CompactFlatCurry
import List
import Prelude hiding (Or)

m4 = readCompactFlatCurry "ex4" "main" >>= writeFCY "ex4t.fcy"
mb = readCompactFlatCurry "bench" "main" >>= writeFCY "bencht.fcy"

--- Reads a main module and imported modules and returns a compact program
--- w.r.t. a main function where all non-recursive lets are replaced
--- by auxiliary functions.
readCompactFlatCurry :: String -> String -> IO Prog
readCompactFlatCurry mainmodname mainfun = do
  putStrLn "Collecting Curry functions to be translated..."
  prog <- computeCompactFlatCurry [Main mainfun, Required defaultRequired]
                                  mainmodname
  return (elimLetInProg (mainmodname,"LET_") prog)


--- Lift nested or/case expressions by replacing them by auxiliary functions.
--- The names of the new auxiliary functions are prefixed by a string
--- and a number (starting with 0).
--- @param prefix - the prefix for the new auxiliary functions (e.g.,
---                 ("module","LET_")
--- @param funs - the list of functions to be lifted
--- @return the list of lifted functions and auxiliary functions

elimLetInProg :: QName -> Prog -> Prog
elimLetInProg prefix (Prog mname imps tdecls fdecls ops) =
   Prog mname imps tdecls (elimLetInFuncs prefix fdecls) ops

elimLetInFuncs :: QName -> [FuncDecl] -> [FuncDecl]
elimLetInFuncs prefix funcs = fst (elimLetInFuncsI prefix 0 funcs)

elimLetInFuncsI _ idx [] = ([],idx)
elimLetInFuncsI prefix idx (Func f n v t (Rule lvars rhs) : fs) =
  let (newrhs,newffuns,newidx) = elimLetInExp prefix idx rhs
      (newfsfuns,newidx1) = elimLetInFuncsI prefix newidx (fs++newffuns)
   in (Func f n v t (Rule lvars newrhs):newfsfuns, newidx1)
elimLetInFuncsI prefix idx (Func f n v t (External fe) : fs) =
  let (newfsfuns,newidx) = elimLetInFuncsI prefix idx fs
   in (Func f n v t (External fe):newfsfuns,newidx)

elimLetInExp :: QName -> Int -> Expr -> (Expr,[FuncDecl],Int)
elimLetInExp _ idx (Var v) = (Var v,[],idx)
elimLetInExp _ idx (Lit l) = (Lit l,[],idx)
elimLetInExp prefix idx (Comb ct cf exps) =
       let (newexps,newfuns,newidx) = elimLetInExps prefix idx exps
        in (Comb ct cf newexps,newfuns,newidx)
elimLetInExp prefix idx (Or e1 e2) =
  let (newe1,newfuns1,idx1) = elimLetInExp prefix idx e1
      (newe2,newfuns2,idx2) = elimLetInExp prefix idx1 e2
   in (Or newe1 newe2,newfuns1++newfuns2,idx2)
elimLetInExp prefix idx (Case ct e brs) =
  let (newe,newfuns1,idx1) = elimLetInExp prefix idx e
      (newbrs,newfuns2,idx2) = elimLetInBranches prefix idx1 brs
   in (Case ct newe newbrs,newfuns1++newfuns2,idx2)
elimLetInExp prefix idx (Free vs exp) =
       let (newexp,newfuns,newidx) = elimLetInExp prefix idx exp
        in (Free vs newexp,newfuns,newidx)
elimLetInExp prefix idx (Let bs exp) =
  let (newbs,newfuns1,idx1) = elimLetInBindings prefix idx bs
      (newexp,newfuns2,newidx) = elimLetInExp prefix idx1 exp
   in eliminateLetExp prefix (newfuns1++newfuns2) newidx newbs newexp

elimLetInExps _ idx [] = ([],[],idx)
elimLetInExps prefix idx (e:es) =
  let (newe,newefun,idx1) = elimLetInExp prefix idx e
      (newes,newesfun,idx2) = elimLetInExps prefix idx1 es
   in (newe:newes,newefun++newesfun,idx2)

elimLetInBindings _ idx [] = ([],[],idx)
elimLetInBindings prefix idx ((v,e):bs) =
  let (newe,newefun,idx1) = elimLetInExp prefix idx e
      (newes,newesfun,idx2) = elimLetInBindings prefix idx1 bs
   in ((v,newe):newes,newefun++newesfun,idx2)

elimLetInBranches _ idx [] = ([],[],idx)
elimLetInBranches prefix idx (Branch p e:bs) =
  let (newe,newefun,idx1) = elimLetInExp prefix idx e
      (newes,newesfun,idx2) = elimLetInBranches prefix idx1 bs
   in (Branch p newe:newes,newefun++newesfun,idx2)


-- try to eliminate a non-recursive Let expression by introducing
-- auxiliary functions:
eliminateLetExp :: QName -> [FuncDecl] -> Int -> [(Int,Expr)] -> Expr
                -> (Expr,[FuncDecl],Int)
eliminateLetExp prefix newfuns idx bindings exp =
  if recursiveBindings bindings
  then let (nonrecbs,letrecexp) = transformLet2NestedLet bindings exp in
       if null nonrecbs
       then (letrecexp, newfuns, idx) -- no Let elimination possible
       else eliminateLetExp prefix newfuns idx nonrecbs letrecexp
  else let fvars = filter (\v->not(elem v (map fst bindings)))
                          (freeVarsInExp exp)
           auxfname = (fst prefix, snd prefix ++show idx)
       in
       (Comb FuncCall auxfname (map Var fvars ++ map snd bindings),
       [Func auxfname (length fvars + length bindings) Private
             (genPolyType (length fvars + length bindings)) -- TODO: insert real type
             (Rule (fvars++map fst bindings) exp)] ++ newfuns,
       idx+1)

-- try to replace a potentially recursive Let expression by nested
-- non-recursive Let expressions:
transformLet2NestedLet bs exp =
  let (nonrecbs,recbs) = splitBindings (map fst bs) bs
   in if null recbs then (nonrecbs,exp)
                    else (nonrecbs,Let recbs exp)
 where
  splitBindings _ [] = ([],[])
  splitBindings boundvars (binding:bindings) =
   let (nbs,rbs) = splitBindings boundvars bindings
    in if any (`elem` boundvars) (freeVarsInExp (snd binding))
       then (nbs,binding:rbs)
       else (binding:nbs,rbs)

-- Is a set of bindings recursive?
recursiveBindings :: [(Int,Expr)] -> Bool
recursiveBindings bs =
  any (`elem` (map fst bs)) ((concatMap freeVarsInExp (map snd bs)))


--- Gets all unbound variables of an expression.
freeVarsInExp :: Expr -> [Int]
freeVarsInExp (Var v) = [v]
freeVarsInExp (Lit _) = []
freeVarsInExp (Comb _ _ exps) = unionMap freeVarsInExp exps
freeVarsInExp (Or e1 e2) = union (freeVarsInExp e1) (freeVarsInExp e2)
freeVarsInExp (Let bs e) =
  filter (`notElem` (map fst bs)) (unionMap freeVarsInExp (e : map snd bs))
freeVarsInExp (Free vs exp) = filter (`notElem` vs) (freeVarsInExp exp)
freeVarsInExp (Case _ e bs) =
  union (freeVarsInExp e) (unionMap freeVarsInBranch bs)

freeVarsInBranch (Branch (Pattern _ vs) e) =
                                        filter (`notElem` vs) (freeVarsInExp e)
freeVarsInBranch (Branch (LPattern _) e) = freeVarsInExp e

unionMap f = foldr union [] . map f


-- generate a most general polymorphic type of the form "a1->a2->...->an->a0"
-- where all ai a pairwise different type variables:
genPolyType n =
  if n==0 then TVar 0
          else FuncType (TVar (n+1)) (genPolyType (n-1))
