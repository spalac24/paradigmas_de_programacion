module ExternalFunctionsGeneric (module ExternalFunctionsGeneric) where

import Curry
import CurryPrelude

fold :: (Curry t0,Curry t1) => (Prim (t0 -> Result (Prim (t0 -> Result t0)))) -> t0 -> t1 -> Result t0
fold f = fold' (\ x y st' -> apply (apply f x st') y st') 

fold' :: (Curry a,Curry b) => (a -> a -> Result a) -> a -> b -> Result a
fold' f g = ghnfCTC (foldCurry (\ x y st' -> f (fold' f g x st') y st') g) 






