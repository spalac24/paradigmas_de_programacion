module ExternalDataIArray where

import Curry 
import CurryPrelude

import Data.Array
import Data.Array.IArray (amap)

-------------------------------------------------------
-- convert between haskell and curry
-------------------------------------------------------

chAssoc :: T2 C_Int a -> (Integer,a)
chAssoc (T2 i e) = (fromCurry i,e)

hcAssoc :: (Integer,a) -> T2 C_Int a
hcAssoc (i,e) = T2 (toCurry i) e

chAssocs xs = map chAssoc (toHaskellList xs)
hcAssocs ys = fromHaskellList (map hcAssoc ys)

chBounds :: T2 C_Int C_Int -> (Integer,Integer)
chBounds (T2 x y) = (fromCurry x,fromCurry y)

hcBounds  :: (Integer,Integer) -> T2 C_Int C_Int
hcBounds (x,y) = T2 (toCurry x) (toCurry y)

-------------------------------------------------------
-- data structure for arrays
-------------------------------------------------------

data C_Array t0 = C_Array (Array Integer t0)
  | C_ArrayFreeVar (FreeVarRef (C_Array t0))
  | C_ArrayFail C_Exceptions
  | C_ArrayOr OrRef (Branches (C_Array t0))
  | C_ArraySusp SuspRef (SuspCont (C_Array t0))


instance (BaseCurry t0) => BaseCurry (C_Array t0) where
  nf f state0 (C_Array a) = 
   let xs = hcAssocs (assocs a) 
       bs = bounds a
       rebuild ys = C_Array (array bs (chAssocs ys))
    in Curry.nfCTC (\state1 xs' -> f state1 (rebuild xs')) state0 xs
  nf f state x = f(state)(x)

  gnf f state0 (C_Array a) = 
   let xs = hcAssocs (assocs a) 
       bs = bounds a
       rebuild ys = C_Array (array bs (chAssocs ys))
    in Curry.gnfCTC (\state1 xs' -> f state1 (rebuild xs')) state0 xs
  gnf f state x = f(state)(x)

  free _ = error "free variable of type array"
  pattern _ = error "free variable of type array"

  failed  = C_ArrayFail

  freeVar  = C_ArrayFreeVar

  branching  = C_ArrayOr

  suspend  = C_ArraySusp

  consKind (C_ArrayFreeVar _) = Curry.Free
  consKind (C_ArrayOr _ _) = Curry.Branching
  consKind (C_ArrayFail _) = Curry.Failed
  consKind (C_ArraySusp _ _) = Curry.Suspended
  consKind _ = Curry.Val

  exceptions (C_ArrayFail x) = x

  freeVarRef (C_ArrayFreeVar x) = x

  orRef (C_ArrayOr x _) = x

  branches (C_ArrayOr _ x) = x

  suspRef (C_ArraySusp x _) = x

  suspCont (C_ArraySusp _ x) = x


instance (Curry t0) => Curry (C_Array t0) where
  strEq st (C_Array x1) (C_Array y1) = 
    let xs = hcAssocs (assocs x1) 
        ys = hcAssocs (assocs y1)
        bsx = hcBounds (bounds x1)
        bsy = hcBounds (bounds y1)
     in concAnd st (genStrEq st bsx bsy) (genStrEq st xs ys)
  strEq _ x0 _ = strEqFail(typeName(x0))

  eq st (C_Array x1) (C_Array y1) =  
    let xs = hcAssocs (assocs x1) 
        ys = hcAssocs (assocs y1)
        bsx = hcBounds (bounds x1) 
        bsy = hcBounds (bounds y1)
     in op_38_38 st (genEq st bsx bsy) (genEq st xs ys)
  eq _ _ _ = CurryPrelude.C_False

  typeName _ = "Array"

  propagate f (C_Array x) = C_Array (amap f x)

  foldCurry f c (C_Array x) = foldCurry f c (hcAssocs (assocs x))

instance (Show t0) => Show (C_Array t0) where
  showsPrec d (C_Array x1) = showParen (d>10) showStr
   where
    showStr  = showString "Array" . showsPrec 11 x1

  showsPrec _ (C_ArrayFreeVar i) = showString ('_':show i)


instance (Read t0) => Read (C_Array t0) where
  readsPrec d r = readParen (d>10) 
    (\ r -> [ (C_Array x1,r1) | (_,r0)  <- readQualified "IArray" "Array" r, 
                                (x1,r1) <- readsPrec 11 r0]) r

