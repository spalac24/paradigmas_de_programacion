module ExternalDataMeta where

import Curry
import CurryPrelude
--import Store
{-
newtype C_Context = Context State deriving (Show)

instance BaseCurry C_Context where

instance Curry C_Context where

instance Read C_Context where
  readsPrec _ _ = error "reading contest"
-}
data C_OrRef = C_OrRef OrRef
  | C_OrRefFail Curry.C_Exceptions
  | C_OrRefOr Curry.OrRef (Curry.Branches C_OrRef)

instance BaseCurry C_OrRef where
  nf f x state = f(x)(state)

  gnf f x state = f(x)(state)

  generator _ = error "free Variable of type OrRef"

  failed  = C_OrRefFail

  branching  = C_OrRefOr

  consKind (C_OrRefOr _ _) = Curry.Branching
  consKind (C_OrRefFail _) = Curry.Failed
  consKind _ = Curry.Val

  exceptions (C_OrRefFail x) = x

  orRef (C_OrRefOr x _) = x

  branches (C_OrRefOr _ x) = x

instance Curry C_OrRef where
  strEq (C_OrRef x1) (C_OrRef y1) _ 
     = if x1 Prelude.== y1 then strEqSuccess else strEqFail "OrRef"
  strEq x0 _ _ = CurryPrelude.strEqFail(CurryPrelude.typeName(x0))

  eq (C_OrRef x1) (C_OrRef y1) _ = 
    if x1 Prelude.== y1 then C_True else C_False
  eq _ _ _ = C_False

  typeName _ = "OrRef"

  propagate _ o _ = o
  
  foldCurry _ c _ _ = c

  showQ d (C_OrRef x1) = showParen (d>10) (showString "Unsafe.OrRef" . showsPrec d x1)





instance Show C_OrRef where
  showsPrec d (C_OrRef x1) = showParen (d>10) (showString "OrRef" . showsPrec d x1)


instance Read C_OrRef where
  readsPrec d r = [ (C_OrRef ref,s) | (ref,s) <- readsPrec d r]




