{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Curry_HigherOrder (C_Order (..), d_C_showOrder, d_C_hiOrdType, nd_C_hiOrdType, d_C_hiOrdCons, nd_C_hiOrdCons, d_C_hiOrdFunc, nd_C_hiOrdFunc) where

import Basics
import qualified Curry_Analysis
import qualified Curry_FlatCurry
import qualified Curry_FlatCurryGoodies
import qualified Curry_GenericProgInfo
import qualified Curry_Maybe
import qualified Curry_Prelude
data C_Order
     = C_HO
     | C_FO
     | Choice_C_Order Cover ID C_Order C_Order
     | Choices_C_Order Cover ID ([C_Order])
     | Fail_C_Order Cover FailInfo
     | Guard_C_Order Cover Constraints C_Order

instance Show C_Order where
  showsPrec d (Choice_C_Order cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_Order cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_Order cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_Order cd info) = showChar '!'
  showsPrec _ C_HO = showString "HO"
  showsPrec _ C_FO = showString "FO"


instance Read C_Order where
  readsPrec _ s = (readParen False (\r -> [ (C_HO,r0) | (_,r0) <- readQualified "HigherOrder" "HO" r]) s) ++ (readParen False (\r -> [ (C_FO,r0) | (_,r0) <- readQualified "HigherOrder" "FO" r]) s)


instance NonDet C_Order where
  choiceCons = Choice_C_Order
  choicesCons = Choices_C_Order
  failCons = Fail_C_Order
  guardCons = Guard_C_Order
  try (Choice_C_Order cd i x y) = tryChoice cd i x y
  try (Choices_C_Order cd i xs) = tryChoices cd i xs
  try (Fail_C_Order cd info) = Fail cd info
  try (Guard_C_Order cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_Order cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_Order cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_Order cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_Order cd i _) = error ("HigherOrder.Order.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_Order cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_Order cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_Order where
  generate s = Choices_C_Order defCover (freeID [0,0] s) [C_HO,C_FO]


instance NormalForm C_Order where
  ($!!) cont C_HO cs = cont C_HO cs
  ($!!) cont C_FO cs = cont C_FO cs
  ($!!) cont (Choice_C_Order cd i x y) cs = nfChoice cont cd i x y cs
  ($!!) cont (Choices_C_Order cd i xs) cs = nfChoices cont cd i xs cs
  ($!!) cont (Guard_C_Order cd c e) cs = guardCons cd c ((cont $!! e) (addCs c cs))
  ($!!) _ (Fail_C_Order cd info) _ = failCons cd info
  ($##) cont C_HO cs = cont C_HO cs
  ($##) cont C_FO cs = cont C_FO cs
  ($##) cont (Choice_C_Order cd i x y) cs = gnfChoice cont cd i x y cs
  ($##) cont (Choices_C_Order cd i xs) cs = gnfChoices cont cd i xs cs
  ($##) cont (Guard_C_Order cd c e) cs = guardCons cd c ((cont $## e) (addCs c cs))
  ($##) _ (Fail_C_Order cd info) _ = failCons cd info
  searchNF _ cont C_HO = cont C_HO
  searchNF _ cont C_FO = cont C_FO
  searchNF _ _ x = error ("HigherOrder.Order.searchNF: no constructor: " ++ (show x))


instance Unifiable C_Order where
  (=.=) C_HO C_HO cs = C_Success
  (=.=) C_FO C_FO cs = C_Success
  (=.=) _ _ _ = Fail_C_Success defCover defFailInfo
  (=.<=) C_HO C_HO cs = C_Success
  (=.<=) C_FO C_FO cs = C_Success
  (=.<=) _ _ _ = Fail_C_Success defCover defFailInfo
  bind i C_HO = ((i :=: (ChooseN 0 0)):(concat []))
  bind i C_FO = ((i :=: (ChooseN 1 0)):(concat []))
  bind i (Choice_C_Order cd j x y) = [(ConstraintChoice cd j (bind i x) (bind i y))]
  bind i (Choices_C_Order cd j@(FreeID _ _) xs) = bindOrNarrow i cd j xs
  bind i (Choices_C_Order cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind i) xs))]
  bind _ (Choices_C_Order cd i _) = error ("HigherOrder.Order.bind: Choices with ChoiceID: " ++ (show i))
  bind _ (Fail_C_Order cd info) = [(Unsolvable info)]
  bind i (Guard_C_Order cd c e) = (getConstrList c) ++ (bind i e)
  lazyBind i C_HO = [(i :=: (ChooseN 0 0))]
  lazyBind i C_FO = [(i :=: (ChooseN 1 0))]
  lazyBind i (Choice_C_Order cd j x y) = [(ConstraintChoice cd j (lazyBind i x) (lazyBind i y))]
  lazyBind i (Choices_C_Order cd j@(FreeID _ _) xs) = lazyBindOrNarrow i cd j xs
  lazyBind i (Choices_C_Order cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind i) xs))]
  lazyBind _ (Choices_C_Order cd i _) = error ("HigherOrder.Order.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ (Fail_C_Order cd info) = [(Unsolvable info)]
  lazyBind i (Guard_C_Order cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind i e)))]


instance Curry_Prelude.Curry C_Order where
  (=?=) (Choice_C_Order cd i x y) z cs = narrow cd i ((x Curry_Prelude.=?= z) cs) ((y Curry_Prelude.=?= z) cs)
  (=?=) (Choices_C_Order cd i xs) y cs = narrows cs cd i (\x -> (x Curry_Prelude.=?= y) cs) xs
  (=?=) (Guard_C_Order cd c e) y cs = guardCons cd c ((e Curry_Prelude.=?= y) (addCs c cs))
  (=?=) (Fail_C_Order cd info) _ _ = failCons cd info
  (=?=) z (Choice_C_Order cd i x y) cs = narrow cd i ((z Curry_Prelude.=?= x) cs) ((z Curry_Prelude.=?= y) cs)
  (=?=) y (Choices_C_Order cd i xs) cs = narrows cs cd i (\x -> (y Curry_Prelude.=?= x) cs) xs
  (=?=) y (Guard_C_Order cd c e) cs = guardCons cd c ((y Curry_Prelude.=?= e) (addCs c cs))
  (=?=) _ (Fail_C_Order cd info) _ = failCons cd info
  (=?=) C_HO C_HO cs = Curry_Prelude.C_True
  (=?=) C_FO C_FO cs = Curry_Prelude.C_True
  (=?=) _ _ _ = Curry_Prelude.C_False
  (<?=) (Choice_C_Order cd i x y) z cs = narrow cd i ((x Curry_Prelude.<?= z) cs) ((y Curry_Prelude.<?= z) cs)
  (<?=) (Choices_C_Order cd i xs) y cs = narrows cs cd i (\x -> (x Curry_Prelude.<?= y) cs) xs
  (<?=) (Guard_C_Order cd c e) y cs = guardCons cd c ((e Curry_Prelude.<?= y) (addCs c cs))
  (<?=) (Fail_C_Order cd info) _ _ = failCons cd info
  (<?=) z (Choice_C_Order cd i x y) cs = narrow cd i ((z Curry_Prelude.<?= x) cs) ((z Curry_Prelude.<?= y) cs)
  (<?=) y (Choices_C_Order cd i xs) cs = narrows cs cd i (\x -> (y Curry_Prelude.<?= x) cs) xs
  (<?=) y (Guard_C_Order cd c e) cs = guardCons cd c ((y Curry_Prelude.<?= e) (addCs c cs))
  (<?=) _ (Fail_C_Order cd info) _ = failCons cd info
  (<?=) C_HO C_HO cs = Curry_Prelude.C_True
  (<?=) C_HO C_FO _ = Curry_Prelude.C_True
  (<?=) C_FO C_FO cs = Curry_Prelude.C_True
  (<?=) _ _ _ = Curry_Prelude.C_False


instance Coverable C_Order where
  cover C_HO = C_HO
  cover C_FO = C_FO
  cover (Choice_C_Order cd i x y) = Choice_C_Order (incCover cd) i (cover x) (cover y)
  cover (Choices_C_Order cd i xs) = Choices_C_Order (incCover cd) i (map cover xs)
  cover (Fail_C_Order cd info) = Fail_C_Order (incCover cd) info
  cover (Guard_C_Order cd c e) = Guard_C_Order (incCover cd) c (cover e)


d_C_showOrder :: C_Order -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showOrder x1 x3500 = case x1 of
     C_HO -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) Curry_Prelude.OP_List)))))))))))
     C_FO -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) Curry_Prelude.OP_List))))))))))
     (Choice_C_Order x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_showOrder x1002 x3500) (d_C_showOrder x1003 x3500)
     (Choices_C_Order x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_showOrder z x3500) x1002
     (Guard_C_Order x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_showOrder x1002) $! (addCs x1001 x3500))
     (Fail_C_Order x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_hoOr :: C_Order -> C_Order -> ConstStore -> C_Order
d_C_hoOr x1 x2 x3500 = case x1 of
     C_HO -> C_HO
     C_FO -> x2
     (Choice_C_Order x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_hoOr x1002 x2 x3500) (d_C_hoOr x1003 x2 x3500)
     (Choices_C_Order x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_hoOr z x2 x3500) x1002
     (Guard_C_Order x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_hoOr x1002 x2) $! (addCs x1001 x3500))
     (Fail_C_Order x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_hiOrdType :: ConstStore -> Curry_Analysis.C_Analysis C_Order
d_C_hiOrdType x3500 = Curry_Analysis.d_C_dependencyTypeAnalysis (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'H'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'O'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'T'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))))))))) C_FO (acceptCs id d_C_orderOfType) x3500

nd_C_hiOrdType :: IDSupply -> ConstStore -> Curry_Analysis.C_Analysis C_Order
nd_C_hiOrdType x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Analysis.nd_C_dependencyTypeAnalysis (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'H'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'O'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'T'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))))))))) C_FO (wrapDX (wrapDX id) (acceptCs id d_C_orderOfType)) x2000 x3500))

d_C_orderOfType :: Curry_FlatCurry.C_TypeDecl -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) C_Order) -> ConstStore -> C_Order
d_C_orderOfType x1 x2 x3500 = case x1 of
     (Curry_FlatCurry.C_Type x3 x4 x5 x6) -> d_C_hoOr (Curry_Prelude.d_C_foldr (acceptCs id d_C_hoOr) C_FO (Curry_Prelude.d_C_map d_OP_orderOfType_dot_orderOfConsDecl_dot_16 x6 x3500) x3500) (Curry_Prelude.d_C_foldr (acceptCs id d_C_hoOr) C_FO (Curry_Prelude.d_C_map Curry_Prelude.d_C_snd x2 x3500) x3500) x3500
     (Curry_FlatCurry.C_TypeSyn x7 x8 x9 x10) -> d_C_hoOr (d_C_orderOfTypeExpr x10 x3500) (Curry_Prelude.d_C_foldr (acceptCs id d_C_hoOr) C_FO (Curry_Prelude.d_C_map Curry_Prelude.d_C_snd x2 x3500) x3500) x3500
     (Curry_FlatCurry.Choice_C_TypeDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_orderOfType x1002 x2 x3500) (d_C_orderOfType x1003 x2 x3500)
     (Curry_FlatCurry.Choices_C_TypeDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_orderOfType z x2 x3500) x1002
     (Curry_FlatCurry.Guard_C_TypeDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_orderOfType x1002 x2) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_TypeDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_orderOfType_dot_orderOfConsDecl_dot_16 :: Curry_FlatCurry.C_ConsDecl -> ConstStore -> C_Order
d_OP_orderOfType_dot_orderOfConsDecl_dot_16 x1 x3500 = case x1 of
     (Curry_FlatCurry.C_Cons x2 x3 x4 x5) -> Curry_Prelude.d_C_foldr (acceptCs id d_C_hoOr) C_FO (Curry_Prelude.d_C_map d_C_orderOfTypeExpr x5 x3500) x3500
     (Curry_FlatCurry.Choice_C_ConsDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_orderOfType_dot_orderOfConsDecl_dot_16 x1002 x3500) (d_OP_orderOfType_dot_orderOfConsDecl_dot_16 x1003 x3500)
     (Curry_FlatCurry.Choices_C_ConsDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_orderOfType_dot_orderOfConsDecl_dot_16 z x3500) x1002
     (Curry_FlatCurry.Guard_C_ConsDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_orderOfType_dot_orderOfConsDecl_dot_16 x1002) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_ConsDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_orderOfTypeExpr :: Curry_FlatCurry.C_TypeExpr -> ConstStore -> C_Order
d_C_orderOfTypeExpr x1 x3500 = case x1 of
     (Curry_FlatCurry.C_TVar x2) -> C_FO
     (Curry_FlatCurry.C_FuncType x3 x4) -> C_HO
     (Curry_FlatCurry.C_TCons x5 x6) -> Curry_Prelude.d_C_foldr (acceptCs id d_C_hoOr) C_FO (Curry_Prelude.d_C_map d_C_orderOfTypeExpr x6 x3500) x3500
     (Curry_FlatCurry.Choice_C_TypeExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_orderOfTypeExpr x1002 x3500) (d_C_orderOfTypeExpr x1003 x3500)
     (Curry_FlatCurry.Choices_C_TypeExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_orderOfTypeExpr z x3500) x1002
     (Curry_FlatCurry.Guard_C_TypeExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_orderOfTypeExpr x1002) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_TypeExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_hiOrdCons :: ConstStore -> Curry_Analysis.C_Analysis C_Order
d_C_hiOrdCons x3500 = Curry_Analysis.d_C_simpleConstructorAnalysis (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'H'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'O'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) Curry_Prelude.OP_List))))))))))))) (acceptCs id d_OP_hiOrdCons_dot_orderOfConsDecl_dot_38) x3500

nd_C_hiOrdCons :: IDSupply -> ConstStore -> Curry_Analysis.C_Analysis C_Order
nd_C_hiOrdCons x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Analysis.nd_C_simpleConstructorAnalysis (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'H'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'O'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) Curry_Prelude.OP_List))))))))))))) (wrapDX (wrapDX id) (acceptCs id d_OP_hiOrdCons_dot_orderOfConsDecl_dot_38)) x2000 x3500))

d_OP_hiOrdCons_dot_orderOfConsDecl_dot_38 :: Curry_Prelude.Curry t0 => Curry_FlatCurry.C_ConsDecl -> t0 -> ConstStore -> C_Order
d_OP_hiOrdCons_dot_orderOfConsDecl_dot_38 x1 x2 x3500 = case x1 of
     (Curry_FlatCurry.C_Cons x3 x4 x5 x6) -> Curry_Prelude.d_C_foldr (acceptCs id d_C_hoOr) C_FO (Curry_Prelude.d_C_map d_C_orderOfTypeExpr x6 x3500) x3500
     (Curry_FlatCurry.Choice_C_ConsDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_hiOrdCons_dot_orderOfConsDecl_dot_38 x1002 x2 x3500) (d_OP_hiOrdCons_dot_orderOfConsDecl_dot_38 x1003 x2 x3500)
     (Curry_FlatCurry.Choices_C_ConsDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_hiOrdCons_dot_orderOfConsDecl_dot_38 z x2 x3500) x1002
     (Curry_FlatCurry.Guard_C_ConsDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_hiOrdCons_dot_orderOfConsDecl_dot_38 x1002 x2) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_ConsDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_hiOrdFunc :: ConstStore -> Curry_Analysis.C_Analysis C_Order
d_C_hiOrdFunc x3500 = Curry_Analysis.d_C_combinedSimpleFuncAnalysis (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'H'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'O'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'F'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) Curry_Prelude.OP_List))))))))))) (d_C_hiOrdType x3500) (acceptCs id d_C_orderOfFunc) x3500

nd_C_hiOrdFunc :: IDSupply -> ConstStore -> Curry_Analysis.C_Analysis C_Order
nd_C_hiOrdFunc x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_Analysis.nd_C_combinedSimpleFuncAnalysis (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'H'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'O'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'F'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) Curry_Prelude.OP_List))))))))))) (nd_C_hiOrdType x2000 x3500) (wrapDX (wrapNX id) (acceptCs id nd_C_orderOfFunc)) x2001 x3500)))))

d_C_orderOfFunc :: Curry_GenericProgInfo.C_ProgInfo C_Order -> Curry_FlatCurry.C_FuncDecl -> ConstStore -> C_Order
d_C_orderOfFunc x1 x2 x3500 = d_C_orderOfFuncTypeArity x1 (Curry_Prelude.d_C_apply (Curry_FlatCurryGoodies.d_C_funcType x3500) x2 x3500) (Curry_Prelude.d_C_apply (Curry_FlatCurryGoodies.d_C_funcArity x3500) x2 x3500) x3500

nd_C_orderOfFunc :: Curry_GenericProgInfo.C_ProgInfo C_Order -> Curry_FlatCurry.C_FuncDecl -> IDSupply -> ConstStore -> C_Order
nd_C_orderOfFunc x1 x2 x3000 x3500 = let
     x2007 = x3000
      in (seq x2007 (let
          x2006 = leftSupply x2007
          x2008 = rightSupply x2007
           in (seq x2006 (seq x2008 (let
               x2002 = leftSupply x2008
               x2005 = rightSupply x2008
                in (seq x2002 (seq x2005 (nd_C_orderOfFuncTypeArity x1 (let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_FlatCurryGoodies.nd_C_funcType x2000 x3500) x2 x2001 x3500)))) (let
                    x2004 = leftSupply x2005
                    x2003 = rightSupply x2005
                     in (seq x2004 (seq x2003 (Curry_Prelude.nd_C_apply (Curry_FlatCurryGoodies.nd_C_funcArity x2003 x3500) x2 x2004 x3500)))) x2006 x3500))))))))

d_C_orderOfFuncTypeArity :: Curry_GenericProgInfo.C_ProgInfo C_Order -> Curry_FlatCurry.C_TypeExpr -> Curry_Prelude.C_Int -> ConstStore -> C_Order
d_C_orderOfFuncTypeArity x1 x2 x3 x3500 = d_OP__case_3 x1 x2 x3 (Curry_Prelude.d_OP_eq_eq x3 (Curry_Prelude.C_Int 0#) x3500) x3500

nd_C_orderOfFuncTypeArity :: Curry_GenericProgInfo.C_ProgInfo C_Order -> Curry_FlatCurry.C_TypeExpr -> Curry_Prelude.C_Int -> IDSupply -> ConstStore -> C_Order
nd_C_orderOfFuncTypeArity x1 x2 x3 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (nd_OP__case_3 x1 x2 x3 (Curry_Prelude.d_OP_eq_eq x3 (Curry_Prelude.C_Int 0#) x3500) x2000 x3500))

d_OP_orderOfFuncTypeArity_dot___hash_selFP2_hash_x :: Curry_FlatCurry.C_TypeExpr -> ConstStore -> Curry_FlatCurry.C_TypeExpr
d_OP_orderOfFuncTypeArity_dot___hash_selFP2_hash_x x1 x3500 = case x1 of
     (Curry_FlatCurry.C_FuncType x2 x3) -> x2
     (Curry_FlatCurry.Choice_C_TypeExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_orderOfFuncTypeArity_dot___hash_selFP2_hash_x x1002 x3500) (d_OP_orderOfFuncTypeArity_dot___hash_selFP2_hash_x x1003 x3500)
     (Curry_FlatCurry.Choices_C_TypeExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_orderOfFuncTypeArity_dot___hash_selFP2_hash_x z x3500) x1002
     (Curry_FlatCurry.Guard_C_TypeExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_orderOfFuncTypeArity_dot___hash_selFP2_hash_x x1002) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_TypeExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_orderOfFuncTypeArity_dot___hash_selFP3_hash_y :: Curry_FlatCurry.C_TypeExpr -> ConstStore -> Curry_FlatCurry.C_TypeExpr
d_OP_orderOfFuncTypeArity_dot___hash_selFP3_hash_y x1 x3500 = case x1 of
     (Curry_FlatCurry.C_FuncType x2 x3) -> x3
     (Curry_FlatCurry.Choice_C_TypeExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_orderOfFuncTypeArity_dot___hash_selFP3_hash_y x1002 x3500) (d_OP_orderOfFuncTypeArity_dot___hash_selFP3_hash_y x1003 x3500)
     (Curry_FlatCurry.Choices_C_TypeExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_orderOfFuncTypeArity_dot___hash_selFP3_hash_y z x3500) x1002
     (Curry_FlatCurry.Guard_C_TypeExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_orderOfFuncTypeArity_dot___hash_selFP3_hash_y x1002) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_TypeExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_3 x1 x2 x3 x14 x3500 = case x14 of
     Curry_Prelude.C_True -> d_OP__case_2 x1 x2 x3500
     Curry_Prelude.C_False -> let
          x12 = d_OP_orderOfFuncTypeArity_dot___hash_selFP2_hash_x x2 x3500
          x13 = d_OP_orderOfFuncTypeArity_dot___hash_selFP3_hash_y x2 x3500
           in (d_C_hoOr (d_C_orderOfFuncTypeArity x1 x12 (Curry_Prelude.C_Int 0#) x3500) (d_C_orderOfFuncTypeArity x1 x13 (Curry_Prelude.d_OP_minus x3 (Curry_Prelude.C_Int 1#) x3500) x3500) x3500)
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_3 x1 x2 x3 x1002 x3500) (d_OP__case_3 x1 x2 x3 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_3 x1 x2 x3 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_3 x1 x2 x3 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_3 x1 x2 x3 x14 x3000 x3500 = case x14 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_2 x1 x2 x2000 x3500))
     Curry_Prelude.C_False -> let
          x2002 = x3000
           in (seq x2002 (let
               x12 = d_OP_orderOfFuncTypeArity_dot___hash_selFP2_hash_x x2 x3500
               x13 = d_OP_orderOfFuncTypeArity_dot___hash_selFP3_hash_y x2 x3500
                in (let
                    x2000 = leftSupply x2002
                    x2001 = rightSupply x2002
                     in (seq x2000 (seq x2001 (d_C_hoOr (nd_C_orderOfFuncTypeArity x1 x12 (Curry_Prelude.C_Int 0#) x2000 x3500) (nd_C_orderOfFuncTypeArity x1 x13 (Curry_Prelude.d_OP_minus x3 (Curry_Prelude.C_Int 1#) x3500) x2001 x3500) x3500))))))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_3 x1 x2 x3 x1002 x3000 x3500) (nd_OP__case_3 x1 x2 x3 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_3 x1 x2 x3 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_3 x1 x2 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_2 x1 x2 x3500 = case x2 of
     (Curry_FlatCurry.C_FuncType x4 x5) -> C_HO
     (Curry_FlatCurry.C_TVar x6) -> let
          x7 = x6
           in (d_OP__case_1 x7 (Curry_Prelude.d_OP_eq_eq x7 (Curry_Prelude.C_Int -42#) x3500) x3500)
     (Curry_FlatCurry.C_TCons x8 x9) -> d_OP__case_0 x1 x8 x9 x3500
     (Curry_FlatCurry.Choice_C_TypeExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_2 x1 x1002 x3500) (d_OP__case_2 x1 x1003 x3500)
     (Curry_FlatCurry.Choices_C_TypeExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_2 x1 z x3500) x1002
     (Curry_FlatCurry.Guard_C_TypeExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_2 x1 x1002) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_TypeExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_2 x1 x2 x3000 x3500 = case x2 of
     (Curry_FlatCurry.C_FuncType x4 x5) -> C_HO
     (Curry_FlatCurry.C_TVar x6) -> let
          x2000 = x3000
           in (seq x2000 (let
               x7 = x6
                in (nd_OP__case_1 x7 (Curry_Prelude.d_OP_eq_eq x7 (Curry_Prelude.C_Int -42#) x3500) x2000 x3500)))
     (Curry_FlatCurry.C_TCons x8 x9) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_0 x1 x8 x9 x2000 x3500))
     (Curry_FlatCurry.Choice_C_TypeExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_2 x1 x1002 x3000 x3500) (nd_OP__case_2 x1 x1003 x3000 x3500)
     (Curry_FlatCurry.Choices_C_TypeExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_2 x1 z x3000 x3500) x1002
     (Curry_FlatCurry.Guard_C_TypeExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_2 x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_TypeExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_0 x1 x8 x9 x3500 = case x9 of
     (Curry_Prelude.OP_Cons x10 x11) -> d_C_hoOr (d_C_orderOfFuncTypeArity x1 x10 (Curry_Prelude.C_Int 0#) x3500) (d_C_orderOfFuncTypeArity x1 (Curry_FlatCurry.C_TCons x8 x11) (Curry_Prelude.C_Int 0#) x3500) x3500
     Curry_Prelude.OP_List -> Curry_Maybe.d_C_fromMaybe C_FO (Curry_GenericProgInfo.d_C_lookupProgInfo x8 x1 x3500) x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_0 x1 x8 x1002 x3500) (d_OP__case_0 x1 x8 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_0 x1 x8 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_0 x1 x8 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_0 x1 x8 x9 x3000 x3500 = case x9 of
     (Curry_Prelude.OP_Cons x10 x11) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2000 = leftSupply x2002
               x2001 = rightSupply x2002
                in (seq x2000 (seq x2001 (d_C_hoOr (nd_C_orderOfFuncTypeArity x1 x10 (Curry_Prelude.C_Int 0#) x2000 x3500) (nd_C_orderOfFuncTypeArity x1 (Curry_FlatCurry.C_TCons x8 x11) (Curry_Prelude.C_Int 0#) x2001 x3500) x3500)))))
     Curry_Prelude.OP_List -> let
          x2000 = x3000
           in (seq x2000 (Curry_Maybe.d_C_fromMaybe C_FO (Curry_GenericProgInfo.nd_C_lookupProgInfo x8 x1 x2000 x3500) x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_0 x1 x8 x1002 x3000 x3500) (nd_OP__case_0 x1 x8 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_0 x1 x8 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_0 x1 x8 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_1 x7 x8 x3500 = case x8 of
     Curry_Prelude.C_True -> C_HO
     Curry_Prelude.C_False -> C_FO
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_1 x7 x1002 x3500) (d_OP__case_1 x7 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_1 x7 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_1 x7 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_1 x7 x8 x3000 x3500 = case x8 of
     Curry_Prelude.C_True -> C_HO
     Curry_Prelude.C_False -> C_FO
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_1 x7 x1002 x3000 x3500) (nd_OP__case_1 x7 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_1 x7 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_1 x7 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo
