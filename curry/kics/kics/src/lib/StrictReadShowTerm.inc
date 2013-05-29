
import qualified Prelude (error)

prim_showTerm :: (ShowTerm x0) => x0 -> (Debug (List Char)) 
prim_showTerm = Prelude.error "prim_showTerm"

prim_showQTerm :: (ShowTerm x0) => x0 -> Debug (List Char)
prim_showQTerm = Prelude.error "prim_showQTerm"

prim_readsUnqualifiedTerm
 :: (ShowTerm x0) => List (List Char) -> List Char -> Debug (List (T2 x0 (List Char))) 
prim_readsUnqualifiedTerm = Prelude.error "prim_readsUnqualifiedTerm"

prim_readsQTerm :: (ShowTerm x0) => List Char -> Debug (List (T2 x0 (List Char))) 
prim_readsQTerm = Prelude.error "prim_readsQTerm"

