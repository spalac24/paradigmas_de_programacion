{-
import FiniteMap

-- impure, but principally the same as "free"
newFreeRep :: () -> FreeRep
newFreeRep external

cmpRep :: FreeRep -> FreeRep -> Bool
cmpRep external 

newFreeReps :: [FreeRep] 
newFreeReps = newFreeRep () : newFreeReps

fromTerm :: Term -> a
fromTerm t = identicalFromTerm (subst t)
  where
    env = buildEnv (emptyFM cmpRep) t

    subst (Free r) = Free (maybe (error "env wrong") id (lookupFM env r))
    subst (NumConstr i ts)  = NumConstr i (map subst ts)
    subst (NameConstr s ts) = NameConstr s (map subst ts)
    subst (Constr i s ts)   = Constr i s (map subst ts)

    buildEnv :: FM FreeRep FreeRep -> Term -> FM FreeRep FreeRep
    buildEnv fm (Free r) = addToFM fm r (newFreeRep ())
    buildEnv fm (NumConstr _ ts)  = foldl buildEnv fm ts
    --buildEnv fm (NameConstr _ ts) = foldl fm buildEnv ts
    --buildEnv fm (Constr _ _ ts)   = foldl fm buildEnv ts

-- dangerous, can lead to internal errors!!!
-- in most cases, fromTerm will be sufficient. 
identicalFromTerm :: Term -> a
identicalFromTerm external

-- the exact structure of a term, i.e. with representation
-- of free variables, depends on the actual state of computation
-- and is therefore IO.
toTerm :: a -> IO Term
toTerm external
-}

fold :: (a -> a -> a) -> a -> b -> a
fold external




