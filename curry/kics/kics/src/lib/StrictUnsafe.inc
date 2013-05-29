
import qualified Prelude (error)
import Unsafe.Coerce

prim_unsafePerformIO :: (ShowTerm x0) => (IO x0) -> (Debug x0) 
prim_unsafePerformIO = Prelude.error "prim_unsafePerformIO"

spawnConstraint :: (ShowTerm x0) => Success -> x0 -> (Debug x0) 
spawnConstraint = Prelude.error "spawnConstraint"

prim_isVar :: (ShowTerm x0) => x0 -> (Debug Bool) 
prim_isVar = Prelude.error "prim_isVar"

prim_identicalVar :: (ShowTerm x0) => x0 -> x0 -> (Debug Bool) 
prim_identicalVar = Prelude.error "prim_identicalVar"

prim_showAnyTerm :: (ShowTerm x0) => x0 -> (Debug (List Char)) 
prim_showAnyTerm = Prelude.error "prim_showAnyTerm"

prim_showAnyQTerm :: (ShowTerm x0) => x0 -> (Debug (List Char)) 
prim_showAnyQTerm = Prelude.error "prim_showAnyQTerm"

prim_readsAnyUnqualifiedTerm
 :: (ShowTerm x0) => List (List Char) -> List Char -> Debug (List (T2 x0 (List Char))) 
prim_readsAnyUnqualifiedTerm = Prelude.error "prim_readsAnyUnqualifiedTerm"

prim_readsAnyQTerm :: (ShowTerm x0) => List Char -> Debug (List (T2 x0 (List Char))) 
prim_readsAnyQTerm = Prelude.error "prim_readsAnyQTerm"

showAnyExpression :: (ShowTerm x0) => x0 -> (Debug (List Char)) 
showAnyExpression = Prelude.error "showAnyExpression"

showAnyQExpression :: (ShowTerm x0) => x0 -> (Debug (List Char)) 
showAnyQExpression = Prelude.error "showAnyQExpression"

try :: (ShowTerm x0) => x0 -> (Debug (Either x0 (OrRef,[x0]))) 
try = Prelude.error "try"

orsWithOrRef :: (ShowTerm x0) => OrRef -> List x0 -> (Debug x0) 
orsWithOrRef (OrRef (Prim _ r)) (List xs) = 
  eval (P.return (fromData (Or (unsafeCoerce r) (fromData xs))))

nrOfChoices ::  (Debug (IO Int)) 
nrOfChoices = Prelude.error "nrOfChoices"

