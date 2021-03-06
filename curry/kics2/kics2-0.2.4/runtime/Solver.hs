-- ---------------------------------------------------------------------------
-- Constraint Solving
-- ---------------------------------------------------------------------------
module Solver (Solution, solve,mkSolution) where

import Types

type Solution m a = m (Maybe (m (), a))

mkDecision :: Store m => ID -> Decision -> a -> Solution m a
mkDecision i d a = setUnsetDecision i d >>= \reset -> return $ Just (reset, a)

mkSolution :: Monad m => a -> Solution m a
mkSolution a = return $ Just (return (), a)

noSolution :: Monad m => Solution m a
noSolution = return Nothing

(>>-) :: Monad m => Solution m a -> (a -> Solution m b) -> Solution m b
ma >>- mbf = do
  mbSolutionA <- ma
  case mbSolutionA of
    Nothing          -> noSolution
    Just (resetA, a) -> do
      mbSolutionB <- mbf a
      case mbSolutionB of
        Nothing          -> resetA >> noSolution
        Just (resetB, b) -> return $ Just (resetB >> resetA, b)

solve :: (Store m, NonDet a) => Constraints -> a -> Solution m a
solve cnstrs val = solve' (getConstrList cnstrs) val
  where
  solve' []     a = mkSolution a
  solve' (c:cs) a = solveOne c a >>- solve' cs

solveOne :: (Store m, NonDet a) => Constraint -> a -> Solution m a
solveOne (Unsolvable _) _ = noSolution
solveOne (ConstraintChoice cd i lcs rcs) e = lookupDecision i >>= follow
  where
  follow ChooseLeft  = mkSolution $ guardCons  defCover (StructConstr lcs) e
  follow ChooseRight = mkSolution $ guardCons  defCover (StructConstr rcs) e
  follow NoDecision  = mkSolution $ choiceCons cd i
                                    (guardCons defCover (StructConstr lcs) e)
                                    (guardCons defCover (StructConstr rcs) e)
  follow c           = error $ "Solver.solve.choose: CC:" ++ show c

solveOne cc@(ConstraintChoices cd i css) e = lookupDecision i >>= follow
  where
  follow (ChooseN c _) = mkSolution $ guardCons defCover (StructConstr (css !! c)) e
  follow NoDecision    = mkSolution $ choicesCons cd i
                                    $ map (\cs -> guardCons defCover (StructConstr cs) e) css
  follow (LazyBind cs) = mkSolution $ guardCons defCover (StructConstr (cs ++ [cc])) e
  follow c             = error $ "Solver.solve.choose: CCs:" ++ show c

solveOne (i :=: cc) e = lookupDecision i >>= follow cc
  where
  -- 1st param: the (new) Choice which should be stored for i
  -- 2nd param: the (old) Choice for i in the store
  follow (LazyBind  _) NoDecision    = mkDecision i cc e
  follow _             (LazyBind cs) = mkDecision i cc
                                     $ guardCons defCover (StructConstr cs) e
  follow (LazyBind cs) _             = mkSolution
                                     $ guardCons defCover (StructConstr cs) e
  follow (BindTo j)    ci            = lookupDecision j >>= \cj -> check i j ci cj e
  follow c             NoDecision    = mkDecision i c e
  follow c             ci            | c == ci   = mkSolution e
                                     | otherwise = noSolution

-- Check whether i can be bound to j and do so if possible
check :: (Store m, NonDet a) => ID -> ID -> Decision -> Decision -> a
      -> Solution m a
check i j _               (LazyBind cs)   e = mkDecision j (BindTo i)
                                            $ guardCons defCover (StructConstr cs) e
check i j NoDecision      _               e = mkDecision i (BindTo j) e
check i j _               NoDecision      e = mkDecision j (BindTo i) e
check i j (ChooseN iN ip) (ChooseN jN jp) e
  = if iN == jN && ip == jp
    then mkSolution $ guardCons defCover
        (StructConstr (zipWith (\ci cj -> ci :=: BindTo cj)
                               (nextNIDs i ip) (nextNIDs j ip)))
        e
    else noSolution
check _ _ ci              cj              e | ci == cj  = mkSolution e
                                            | otherwise = noSolution
