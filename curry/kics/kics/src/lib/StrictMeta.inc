
import qualified Prelude (error)
import qualified Store as S
import Unsafe.Coerce

prim_isFree :: (ShowTerm x0) => x0 -> (Debug (IO (Either x0 x0))) 
prim_isFree = Prelude.error "prim_isFree"

headNormalFormIO :: (ShowTerm x0,ShowTerm x1) => Func x0 (IO x1) -> x0 -> Debug (IO x1)
headNormalFormIO f x = apply f x

searchTree :: (ShowTerm x0) => x0 -> (Debug (SearchTree x0)) 
searchTree = Prelude.error "searchTree"

gnfIO :: (ShowTerm x0) => x0 -> (Debug (IO x0)) 
gnfIO = Prelude.error "gnfIO"

ghnfIO :: (ShowTerm x0) => x0 -> (Debug (IO x0)) 
ghnfIO = Prelude.error "ghnfIO"

nfIO :: (ShowTerm x0) => x0 -> (Debug (IO x0)) 
nfIO = Prelude.error "nfIO"

hnfIO :: (ShowTerm x0) => x0 -> (Debug (IO x0)) 
hnfIO = Prelude.error "hnfIO"

getRichSearchTree :: (ShowTerm x0) => x0 -> (Debug (IO (RichSearchTree x0))) 
getRichSearchTree = Prelude.error "getRichSearchTree"

instance Convert S.OrRef where
  fromData (Prim _ x) = unsafeCoerce x
  fromData x = P.error (P.show x)
  toData   r          = Prim (consTerm (P.show (S.deref r)) []) (unsafeCoerce r)

richSearchTree :: (ShowTerm x0) => x0 -> (Debug (RichSearchTree x0)) 
richSearchTree x = P.error "richSearchTree"

parallelSearch :: (ShowTerm x0) => x0 -> (Debug (IO [x0])) 
parallelSearch = Prelude.error "parallelSearch"

cover :: (ShowTerm x0) => x0 -> (Debug x0) 
cover = Prelude.error "cover"

st :: (ShowTerm x0) => x0 -> (Debug (SearchTree x0)) 
st = Prelude.error "st"

richST :: (ShowTerm x0) => x0 -> (Debug (RichSearchTree x0)) 
richST x = eval (P.return (RichSearchTree (rst (toData x))))
  where
    rst (Fail s)  = C1_1 (C1_1 (toData s))
    rst (Or r xs) = C2_1 (toData r) (toData (P.map rst xs))
    rst  v        = C1_2 v


ors :: (ShowTerm x0) => [x0] -> (Debug x0) 
ors = Prelude.error "ors"

prim_throw :: (ShowTerm x0) => Exception -> (Debug x0) 
prim_throw = Prelude.error "prim_throw"

showConsOrRef :: OrRef -> Term
showConsOrRef (OrRef o) = showTerm o

instance Generator OrRef where
  generator = P.error "free variable of type OrRef"