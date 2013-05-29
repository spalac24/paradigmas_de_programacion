{-# LANGUAGE EmptyDataDecls, ForeignFunctionInterface #-}
{-# INCLUDE <coracle.h> #-}

module ExternalFunctionsCEventOracle where

import Foreign
import Foreign.C.String	

import ExternalDataCEventOracle
import Curry
import qualified CurryPrelude as CP

foreign import ccall unsafe "static coracle.h init"      cinitialize :: Ref
foreign import ccall unsafe "static coracle.h nextref"   cfresh      :: Int -> Ref

foreign import ccall unsafe "static coracle.h collapse"  ccollapse   :: Ref -> ()
foreign import ccall unsafe "static coracle.h close_ref" ccloseRef   :: Ref -> ()
foreign import ccall unsafe "static coracle.h inconly"   creplace    :: Ref -> ()
foreign import ccall unsafe "static coracle.h expand"    cexpand     :: Ref -> Int -> ()

foreign import ccall unsafe "coracle.h finalize" cfinalize :: CString -> ()

initRef :: Result (CP.C_IO C_Ref)
initRef = let ref = cinitialize in seq ref (CP.return (C_Ref ref))

finalize :: CP.C_String -> Result (CP.C_IO CP.T0)
finalize s _ = CP.C_IO (\ _ -> do
  s' <- newCString (CP.fromCurry s)
  seq (cfinalize s') (return (CP.IOVal (CP.T0))))

--- Side effect that computes a fresh reference.
fresh :: a -> Result C_Ref
fresh _ _ =  C_Ref (cfresh 0)

--- increase couter of ref by one
replace :: C_Ref -> a -> b -> a
replace  (C_Ref ref) x _ = seq (creplace ref) x

--- Remove a ref and combine and counter +1
collapse :: C_Ref -> a -> b -> a
collapse  (C_Ref ref) x _ = seq (ccollapse ref) x

--- Remove a ref and combine and counter +1
closeRef :: C_Ref -> a -> b -> a
closeRef  (C_Ref ref) x _ = seq (ccloseRef ref) x


--- Projection on last argument that releases an event as a side effect.
-- increment step counter of first ref and add remaining refs
-- list has to be at least of size 1
expand :: C_Ref -> CP.List C_Ref -> a -> Result a
expand  (C_Ref ref) refs x _
  = seq (cexpand ref (len refs)) (seq (evalRefs refs) x)
 where
  len CP.List      = 0
  len (_ CP.:< rs) = 1+len rs

  evalRefs CP.List = ()
  evalRefs (r CP.:< rs) = seq r (evalRefs rs)

-- compare to BaseCurry.mapOr
c_onBranches :: (BaseCurry a,BaseCurry b) => 
  C_Ref -> (C_Ref -> a -> Result b) -> OrRef -> Branches a -> Result b
c_onBranches r cont orref bs = 
  evalRef' True (\ r x st -> replace r (cont r x st) st) (branching orref bs) r 
  
liftCase :: (BaseCurry a,BaseCurry b) => 
  C_Ref -> (C_Ref -> a -> Result b) -> OrRef -> Branches a -> (Int -> State) -> b
liftCase ref f orref bs = 
  expand' ref (length bs - 1) (\ refs -> lift ($) orref (zipWith f refs bs))

expand' :: C_Ref -> Int -> ([C_Ref] -> a) -> a
expand' (C_Ref ref) l cont = 
   cexpand ref l `seq`
   evalRefs refs `seq`
   cont (map C_Ref (ref:refs))
 where
   refs = map cfresh (replicate l 0)
   
   evalRefs [] = ()
   evalRefs (r : rs) = seq r (evalRefs rs)
   

--- generating a fresh variable
unknown :: CP.Curry a => C_Ref -> Result a
unknown r st = freeF (\x -> gen r x st)

gen :: CP.Curry a => C_Ref -> a -> Result a
gen r x st = case consKind x of
  Val       -> push r x st
  Branching -> liftCase r gen (orRef x) (branches x) (const st)

push :: CP.Curry a => C_Ref -> a -> Result a
push r x st = case CP.foldCurry (\ _ n _ -> n+1) 0 x st of
  0 -> collapse r x st
  1 -> replace r (CP.propagate (\_ -> gen r) x st) st
  n -> expand' r (n-1) (\ refs -> CP.propagate (distRefs refs) x st)

  where
    distRefs rs i x = gen (rs!!i) x
  
 
type RefFun a b = C_Ref -> Result (CP.Prim (a -> Result b))

($!), ($#),($!!),($##), apply :: (CP.Curry a,CP.Curry b) => 
  CP.Prim (RefFun a b) -> a -> C_Ref -> Result b
(cont $! x)  r = prepApply hnfRef x cont r
(cont $# x)  r = prepApply (evalRef True) x cont r
(cont $!! x) r st = prepApply hnfRef (nfCTC const x st) cont r st
(cont $## x) r st = prepApply (evalRef True) (gnfCTC const x st) cont r st


apply cont x r = prepApply (\ cont' x' r' st' -> replace r' (CP.apply (cont' r' st') x' st') st') x cont r

prepApply :: (BaseCurry a,BaseCurry b) => 
  (RefFun b a -> b -> C_Ref -> Result a) -> b -> CP.Prim (RefFun b a) -> C_Ref -> Result a
prepApply  prep x (CP.PrimValue f) cref st = prep f x cref st
prepApply  prep x (CP.PrimOr r bs) cref st = 
  c_onBranches cref (\ r' f' -> prepApply prep x f' r') r bs st
prepApply  _    _ f                r st  = collapse r (patternFail "Prelude.prepApply" f) st

hnfRef :: (BaseCurry a,BaseCurry b) => RefFun b a -> b -> C_Ref -> Result a
hnfRef = evalRef False 

evalRef :: (BaseCurry a,BaseCurry b) => HNFMode -> RefFun a b -> a -> C_Ref -> Result b
evalRef mode cont = evalRef' mode (\ r x st ->  replace r (CP.apply (cont r st) x st) st)

evalRef' :: (BaseCurry a,BaseCurry b) => 
            HNFMode -> (C_Ref -> a -> Result b) -> a -> C_Ref -> Result b
evalRef' mode cont x r state = 
 case consKind x of
   Failed    -> closeRef r (addException err x) state
   Branching -> let orref = orRef x
                    bs = branches x in
    manipulateStore
      (closeRef r (failed (curryError "=:=")) state)
      contEval
      (\ ref' contSt -> if mode || not (isGenerator orref)
                      then liftCase r contEvalRef (narrowOrRef orref) bs contSt
                      else cont r (branching ref' bs) state)
      (\ orref' x' st' -> branching orref' [contEval x' st'])
      orref bs state
   Val       -> cont r x state

  where
    err = curryError ("Prelude."++if mode then "$#" else "$!")
    contEvalRef r' x' st' = evalRef' mode cont x' r' st'
    contEval = contEvalRef r
