{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Curry_Analysis (C_Declaration (..), C_Visibilities (..), C_Map, C_AnalysisFunction, C_NDResult, C_HOResult, d_C_getDeclName, d_C_getFunctionCalls, d_C_getCalls, d_C_fullIteration, nd_C_fullIteration, d_C_initNDResult, nd_C_initNDResult, d_C_analyseND, nd_C_analyseND, d_C_ndFunc, nd_C_ndFunc, d_C_isNDExpr, nd_C_isNDExpr, d_C_qmark, d_C_initHOResult, nd_C_initHOResult, d_C_analyseHOFunc, nd_C_analyseHOFunc, d_C_analyseHOCons, nd_C_analyseHOCons, d_C_consOrder, d_C_hoOr, d_C_ordFunc, nd_C_ordFunc, d_C_goThroughConsList, nd_C_goThroughConsList, d_C_ordHelp1, nd_C_ordHelp1, d_C_ordHelp2, nd_C_ordHelp2, d_C_getPrivateFunc, d_C_getPublicFunc, d_C_getPrivateType, d_C_getPublicType, d_C_getPrivateCons, d_C_getPublicCons, d_C_analyzeVisibility, d_C_splitVisibleFuncs, d_C_splitVisibleTypes, d_C_splitVisibleCons, d_OP_bar_plus_plus_bar) where

import Basics
import qualified Curry_Base
import qualified Curry_Dependency2
import qualified Curry_FiniteMap
import qualified Curry_FlatCurry
import qualified Curry_FlatCurryGoodies
import qualified Curry_List
import qualified Curry_Maybe
import qualified Curry_Names
import qualified Curry_Prelude
type C_Map t0 = Curry_FiniteMap.C_FM (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) t0

type C_AnalysisFunction t0 t1 = Curry_FiniteMap.C_FM (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) t1 -> ConstStore -> Curry_Prelude.OP_Tuple2 t0 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) t1

type C_NDResult = Curry_FiniteMap.C_FM (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Base.C_NDClass

type C_HOResult = Curry_FiniteMap.C_FM (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Base.C_HOClass

data C_Declaration
     = C_F Curry_FlatCurry.C_FuncDecl
     | C_T Curry_FlatCurry.C_TypeDecl
     | Choice_C_Declaration Cover ID C_Declaration C_Declaration
     | Choices_C_Declaration Cover ID ([C_Declaration])
     | Fail_C_Declaration Cover FailInfo
     | Guard_C_Declaration Cover Constraints C_Declaration

instance Show C_Declaration where
  showsPrec d (Choice_C_Declaration cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_Declaration cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_Declaration cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_Declaration cd info) = showChar '!'
  showsPrec _ (C_F x1) = (showString "(F") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (C_T x1) = (showString "(T") . ((showChar ' ') . ((shows x1) . (showChar ')')))


instance Read C_Declaration where
  readsPrec d s = (readParen (d > 10) (\r -> [ (C_F x1,r1) | (_,r0) <- readQualified "Analysis" "F" r, (x1,r1) <- readsPrec 11 r0]) s) ++ (readParen (d > 10) (\r -> [ (C_T x1,r1) | (_,r0) <- readQualified "Analysis" "T" r, (x1,r1) <- readsPrec 11 r0]) s)


instance NonDet C_Declaration where
  choiceCons = Choice_C_Declaration
  choicesCons = Choices_C_Declaration
  failCons = Fail_C_Declaration
  guardCons = Guard_C_Declaration
  try (Choice_C_Declaration cd i x y) = tryChoice cd i x y
  try (Choices_C_Declaration cd i xs) = tryChoices cd i xs
  try (Fail_C_Declaration cd info) = Fail cd info
  try (Guard_C_Declaration cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_Declaration cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_Declaration cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_Declaration cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_Declaration cd i _) = error ("Analysis.Declaration.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_Declaration cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_Declaration cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_Declaration where
  generate s = Choices_C_Declaration defCover (freeID [1,1] s) [(C_F (generate (leftSupply s))),(C_T (generate (leftSupply s)))]


instance NormalForm C_Declaration where
  ($!!) cont (C_F x1) cs = ((\y1 cs -> cont (C_F y1) cs) $!! x1) cs
  ($!!) cont (C_T x1) cs = ((\y1 cs -> cont (C_T y1) cs) $!! x1) cs
  ($!!) cont (Choice_C_Declaration cd i x y) cs = nfChoice cont cd i x y cs
  ($!!) cont (Choices_C_Declaration cd i xs) cs = nfChoices cont cd i xs cs
  ($!!) cont (Guard_C_Declaration cd c e) cs = guardCons cd c ((cont $!! e) (addCs c cs))
  ($!!) _ (Fail_C_Declaration cd info) _ = failCons cd info
  ($##) cont (C_F x1) cs = ((\y1 cs -> cont (C_F y1) cs) $## x1) cs
  ($##) cont (C_T x1) cs = ((\y1 cs -> cont (C_T y1) cs) $## x1) cs
  ($##) cont (Choice_C_Declaration cd i x y) cs = gnfChoice cont cd i x y cs
  ($##) cont (Choices_C_Declaration cd i xs) cs = gnfChoices cont cd i xs cs
  ($##) cont (Guard_C_Declaration cd c e) cs = guardCons cd c ((cont $## e) (addCs c cs))
  ($##) _ (Fail_C_Declaration cd info) _ = failCons cd info
  searchNF search cont (C_F x1) = search (\y1 -> cont (C_F y1)) x1
  searchNF search cont (C_T x1) = search (\y1 -> cont (C_T y1)) x1
  searchNF _ _ x = error ("Analysis.Declaration.searchNF: no constructor: " ++ (show x))


instance Unifiable C_Declaration where
  (=.=) (C_F x1) (C_F y1) cs = (x1 =:= y1) cs
  (=.=) (C_T x1) (C_T y1) cs = (x1 =:= y1) cs
  (=.=) _ _ _ = Fail_C_Success defCover defFailInfo
  (=.<=) (C_F x1) (C_F y1) cs = (x1 =:<= y1) cs
  (=.<=) (C_T x1) (C_T y1) cs = (x1 =:<= y1) cs
  (=.<=) _ _ _ = Fail_C_Success defCover defFailInfo
  bind i (C_F x2) = ((i :=: (ChooseN 0 1)):(concat [(bind (leftID i) x2)]))
  bind i (C_T x2) = ((i :=: (ChooseN 1 1)):(concat [(bind (leftID i) x2)]))
  bind i (Choice_C_Declaration cd j x y) = [(ConstraintChoice cd j (bind i x) (bind i y))]
  bind i (Choices_C_Declaration cd j@(FreeID _ _) xs) = bindOrNarrow i cd j xs
  bind i (Choices_C_Declaration cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind i) xs))]
  bind _ (Choices_C_Declaration cd i _) = error ("Analysis.Declaration.bind: Choices with ChoiceID: " ++ (show i))
  bind _ (Fail_C_Declaration cd info) = [(Unsolvable info)]
  bind i (Guard_C_Declaration cd c e) = (getConstrList c) ++ (bind i e)
  lazyBind i (C_F x2) = [(i :=: (ChooseN 0 1)),((leftID i) :=: (LazyBind (lazyBind (leftID i) x2)))]
  lazyBind i (C_T x2) = [(i :=: (ChooseN 1 1)),((leftID i) :=: (LazyBind (lazyBind (leftID i) x2)))]
  lazyBind i (Choice_C_Declaration cd j x y) = [(ConstraintChoice cd j (lazyBind i x) (lazyBind i y))]
  lazyBind i (Choices_C_Declaration cd j@(FreeID _ _) xs) = lazyBindOrNarrow i cd j xs
  lazyBind i (Choices_C_Declaration cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind i) xs))]
  lazyBind _ (Choices_C_Declaration cd i _) = error ("Analysis.Declaration.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ (Fail_C_Declaration cd info) = [(Unsolvable info)]
  lazyBind i (Guard_C_Declaration cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind i e)))]


instance Curry_Prelude.Curry C_Declaration where
  (=?=) (Choice_C_Declaration cd i x y) z cs = narrow cd i ((x Curry_Prelude.=?= z) cs) ((y Curry_Prelude.=?= z) cs)
  (=?=) (Choices_C_Declaration cd i xs) y cs = narrows cs cd i (\x -> (x Curry_Prelude.=?= y) cs) xs
  (=?=) (Guard_C_Declaration cd c e) y cs = guardCons cd c ((e Curry_Prelude.=?= y) (addCs c cs))
  (=?=) (Fail_C_Declaration cd info) _ _ = failCons cd info
  (=?=) z (Choice_C_Declaration cd i x y) cs = narrow cd i ((z Curry_Prelude.=?= x) cs) ((z Curry_Prelude.=?= y) cs)
  (=?=) y (Choices_C_Declaration cd i xs) cs = narrows cs cd i (\x -> (y Curry_Prelude.=?= x) cs) xs
  (=?=) y (Guard_C_Declaration cd c e) cs = guardCons cd c ((y Curry_Prelude.=?= e) (addCs c cs))
  (=?=) _ (Fail_C_Declaration cd info) _ = failCons cd info
  (=?=) (C_F x1) (C_F y1) cs = (x1 Curry_Prelude.=?= y1) cs
  (=?=) (C_T x1) (C_T y1) cs = (x1 Curry_Prelude.=?= y1) cs
  (=?=) _ _ _ = Curry_Prelude.C_False
  (<?=) (Choice_C_Declaration cd i x y) z cs = narrow cd i ((x Curry_Prelude.<?= z) cs) ((y Curry_Prelude.<?= z) cs)
  (<?=) (Choices_C_Declaration cd i xs) y cs = narrows cs cd i (\x -> (x Curry_Prelude.<?= y) cs) xs
  (<?=) (Guard_C_Declaration cd c e) y cs = guardCons cd c ((e Curry_Prelude.<?= y) (addCs c cs))
  (<?=) (Fail_C_Declaration cd info) _ _ = failCons cd info
  (<?=) z (Choice_C_Declaration cd i x y) cs = narrow cd i ((z Curry_Prelude.<?= x) cs) ((z Curry_Prelude.<?= y) cs)
  (<?=) y (Choices_C_Declaration cd i xs) cs = narrows cs cd i (\x -> (y Curry_Prelude.<?= x) cs) xs
  (<?=) y (Guard_C_Declaration cd c e) cs = guardCons cd c ((y Curry_Prelude.<?= e) (addCs c cs))
  (<?=) _ (Fail_C_Declaration cd info) _ = failCons cd info
  (<?=) (C_F x1) (C_F y1) cs = (x1 Curry_Prelude.<?= y1) cs
  (<?=) (C_F _) (C_T _) _ = Curry_Prelude.C_True
  (<?=) (C_T x1) (C_T y1) cs = (x1 Curry_Prelude.<?= y1) cs
  (<?=) _ _ _ = Curry_Prelude.C_False


instance Coverable C_Declaration where
  cover (C_F x1) = C_F (cover x1)
  cover (C_T x1) = C_T (cover x1)
  cover (Choice_C_Declaration cd i x y) = Choice_C_Declaration (incCover cd) i (cover x) (cover y)
  cover (Choices_C_Declaration cd i xs) = Choices_C_Declaration (incCover cd) i (map cover xs)
  cover (Fail_C_Declaration cd info) = Fail_C_Declaration (incCover cd) info
  cover (Guard_C_Declaration cd c e) = Guard_C_Declaration (incCover cd) c (cover e)


data C_Visibilities
     = C_Vis (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))))
     | Choice_C_Visibilities Cover ID C_Visibilities C_Visibilities
     | Choices_C_Visibilities Cover ID ([C_Visibilities])
     | Fail_C_Visibilities Cover FailInfo
     | Guard_C_Visibilities Cover Constraints C_Visibilities

instance Show C_Visibilities where
  showsPrec d (Choice_C_Visibilities cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_Visibilities cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_Visibilities cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_Visibilities cd info) = showChar '!'
  showsPrec _ (C_Vis x1 x2 x3) = (showString "(Vis") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . ((showChar ' ') . ((shows x3) . (showChar ')')))))))


instance Read C_Visibilities where
  readsPrec d s = readParen (d > 10) (\r -> [ (C_Vis x1 x2 x3,r3) | (_,r0) <- readQualified "Analysis" "Vis" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1, (x3,r3) <- readsPrec 11 r2]) s


instance NonDet C_Visibilities where
  choiceCons = Choice_C_Visibilities
  choicesCons = Choices_C_Visibilities
  failCons = Fail_C_Visibilities
  guardCons = Guard_C_Visibilities
  try (Choice_C_Visibilities cd i x y) = tryChoice cd i x y
  try (Choices_C_Visibilities cd i xs) = tryChoices cd i xs
  try (Fail_C_Visibilities cd info) = Fail cd info
  try (Guard_C_Visibilities cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_Visibilities cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_Visibilities cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_Visibilities cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_Visibilities cd i _) = error ("Analysis.Visibilities.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_Visibilities cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_Visibilities cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_Visibilities where
  generate s = Choices_C_Visibilities defCover (freeID [3] s) [(C_Vis (generate (leftSupply (leftSupply s))) (generate (rightSupply (leftSupply s))) (generate (rightSupply s)))]


instance NormalForm C_Visibilities where
  ($!!) cont (C_Vis x1 x2 x3) cs = ((\y1 cs -> ((\y2 cs -> ((\y3 cs -> cont (C_Vis y1 y2 y3) cs) $!! x3) cs) $!! x2) cs) $!! x1) cs
  ($!!) cont (Choice_C_Visibilities cd i x y) cs = nfChoice cont cd i x y cs
  ($!!) cont (Choices_C_Visibilities cd i xs) cs = nfChoices cont cd i xs cs
  ($!!) cont (Guard_C_Visibilities cd c e) cs = guardCons cd c ((cont $!! e) (addCs c cs))
  ($!!) _ (Fail_C_Visibilities cd info) _ = failCons cd info
  ($##) cont (C_Vis x1 x2 x3) cs = ((\y1 cs -> ((\y2 cs -> ((\y3 cs -> cont (C_Vis y1 y2 y3) cs) $## x3) cs) $## x2) cs) $## x1) cs
  ($##) cont (Choice_C_Visibilities cd i x y) cs = gnfChoice cont cd i x y cs
  ($##) cont (Choices_C_Visibilities cd i xs) cs = gnfChoices cont cd i xs cs
  ($##) cont (Guard_C_Visibilities cd c e) cs = guardCons cd c ((cont $## e) (addCs c cs))
  ($##) _ (Fail_C_Visibilities cd info) _ = failCons cd info
  searchNF search cont (C_Vis x1 x2 x3) = search (\y1 -> search (\y2 -> search (\y3 -> cont (C_Vis y1 y2 y3)) x3) x2) x1
  searchNF _ _ x = error ("Analysis.Visibilities.searchNF: no constructor: " ++ (show x))


instance Unifiable C_Visibilities where
  (=.=) (C_Vis x1 x2 x3) (C_Vis y1 y2 y3) cs = (((x1 =:= y1) cs) & ((((x2 =:= y2) cs) & ((x3 =:= y3) cs)) cs)) cs
  (=.=) _ _ _ = Fail_C_Success defCover defFailInfo
  (=.<=) (C_Vis x1 x2 x3) (C_Vis y1 y2 y3) cs = (((x1 =:<= y1) cs) & ((((x2 =:<= y2) cs) & ((x3 =:<= y3) cs)) cs)) cs
  (=.<=) _ _ _ = Fail_C_Success defCover defFailInfo
  bind i (C_Vis x2 x3 x4) = ((i :=: (ChooseN 0 3)):(concat [(bind (leftID (leftID i)) x2),(bind (rightID (leftID i)) x3),(bind (rightID i) x4)]))
  bind i (Choice_C_Visibilities cd j x y) = [(ConstraintChoice cd j (bind i x) (bind i y))]
  bind i (Choices_C_Visibilities cd j@(FreeID _ _) xs) = bindOrNarrow i cd j xs
  bind i (Choices_C_Visibilities cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind i) xs))]
  bind _ (Choices_C_Visibilities cd i _) = error ("Analysis.Visibilities.bind: Choices with ChoiceID: " ++ (show i))
  bind _ (Fail_C_Visibilities cd info) = [(Unsolvable info)]
  bind i (Guard_C_Visibilities cd c e) = (getConstrList c) ++ (bind i e)
  lazyBind i (C_Vis x2 x3 x4) = [(i :=: (ChooseN 0 3)),((leftID (leftID i)) :=: (LazyBind (lazyBind (leftID (leftID i)) x2))),((rightID (leftID i)) :=: (LazyBind (lazyBind (rightID (leftID i)) x3))),((rightID i) :=: (LazyBind (lazyBind (rightID i) x4)))]
  lazyBind i (Choice_C_Visibilities cd j x y) = [(ConstraintChoice cd j (lazyBind i x) (lazyBind i y))]
  lazyBind i (Choices_C_Visibilities cd j@(FreeID _ _) xs) = lazyBindOrNarrow i cd j xs
  lazyBind i (Choices_C_Visibilities cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind i) xs))]
  lazyBind _ (Choices_C_Visibilities cd i _) = error ("Analysis.Visibilities.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ (Fail_C_Visibilities cd info) = [(Unsolvable info)]
  lazyBind i (Guard_C_Visibilities cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind i e)))]


instance Curry_Prelude.Curry C_Visibilities where
  (=?=) (Choice_C_Visibilities cd i x y) z cs = narrow cd i ((x Curry_Prelude.=?= z) cs) ((y Curry_Prelude.=?= z) cs)
  (=?=) (Choices_C_Visibilities cd i xs) y cs = narrows cs cd i (\x -> (x Curry_Prelude.=?= y) cs) xs
  (=?=) (Guard_C_Visibilities cd c e) y cs = guardCons cd c ((e Curry_Prelude.=?= y) (addCs c cs))
  (=?=) (Fail_C_Visibilities cd info) _ _ = failCons cd info
  (=?=) z (Choice_C_Visibilities cd i x y) cs = narrow cd i ((z Curry_Prelude.=?= x) cs) ((z Curry_Prelude.=?= y) cs)
  (=?=) y (Choices_C_Visibilities cd i xs) cs = narrows cs cd i (\x -> (y Curry_Prelude.=?= x) cs) xs
  (=?=) y (Guard_C_Visibilities cd c e) cs = guardCons cd c ((y Curry_Prelude.=?= e) (addCs c cs))
  (=?=) _ (Fail_C_Visibilities cd info) _ = failCons cd info
  (=?=) (C_Vis x1 x2 x3) (C_Vis y1 y2 y3) cs = Curry_Prelude.d_OP_ampersand_ampersand ((x1 Curry_Prelude.=?= y1) cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x2 Curry_Prelude.=?= y2) cs) ((x3 Curry_Prelude.=?= y3) cs) cs) cs
  (<?=) (Choice_C_Visibilities cd i x y) z cs = narrow cd i ((x Curry_Prelude.<?= z) cs) ((y Curry_Prelude.<?= z) cs)
  (<?=) (Choices_C_Visibilities cd i xs) y cs = narrows cs cd i (\x -> (x Curry_Prelude.<?= y) cs) xs
  (<?=) (Guard_C_Visibilities cd c e) y cs = guardCons cd c ((e Curry_Prelude.<?= y) (addCs c cs))
  (<?=) (Fail_C_Visibilities cd info) _ _ = failCons cd info
  (<?=) z (Choice_C_Visibilities cd i x y) cs = narrow cd i ((z Curry_Prelude.<?= x) cs) ((z Curry_Prelude.<?= y) cs)
  (<?=) y (Choices_C_Visibilities cd i xs) cs = narrows cs cd i (\x -> (y Curry_Prelude.<?= x) cs) xs
  (<?=) y (Guard_C_Visibilities cd c e) cs = guardCons cd c ((y Curry_Prelude.<?= e) (addCs c cs))
  (<?=) _ (Fail_C_Visibilities cd info) _ = failCons cd info
  (<?=) (C_Vis x1 x2 x3) (C_Vis y1 y2 y3) cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x1 Curry_Prelude.=?= y1) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x2 y2 cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x2 Curry_Prelude.=?= y2) cs) ((x3 Curry_Prelude.<?= y3) cs) cs) cs) cs) cs


instance Coverable C_Visibilities where
  cover (C_Vis x1 x2 x3) = C_Vis (cover x1) (cover x2) (cover x3)
  cover (Choice_C_Visibilities cd i x y) = Choice_C_Visibilities (incCover cd) i (cover x) (cover y)
  cover (Choices_C_Visibilities cd i xs) = Choices_C_Visibilities (incCover cd) i (map cover xs)
  cover (Fail_C_Visibilities cd info) = Fail_C_Visibilities (incCover cd) info
  cover (Guard_C_Visibilities cd c e) = Guard_C_Visibilities (incCover cd) c (cover e)


d_C_getDeclName :: C_Declaration -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_C_getDeclName x1 x3500 = case x1 of
     (C_F x2) -> Curry_Prelude.d_C_apply (Curry_FlatCurryGoodies.d_C_funcName x3500) x2 x3500
     (C_T x3) -> Curry_Prelude.d_C_apply (Curry_FlatCurryGoodies.d_C_typeName x3500) x3 x3500
     (Choice_C_Declaration x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_getDeclName x1002 x3500) (d_C_getDeclName x1003 x3500)
     (Choices_C_Declaration x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_getDeclName z x3500) x1002
     (Guard_C_Declaration x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_getDeclName x1002) $! (addCs x1001 x3500))
     (Fail_C_Declaration x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_getFunctionCalls :: Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_FlatCurry.C_FuncDecl (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))))
d_C_getFunctionCalls x1 x3500 = Curry_Prelude.d_C_map d_OP_getFunctionCalls_dot___hash_lambda2 x1 x3500

d_OP_getFunctionCalls_dot___hash_lambda2 :: Curry_FlatCurry.C_FuncDecl -> ConstStore -> Curry_Prelude.OP_Tuple2 Curry_FlatCurry.C_FuncDecl (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))
d_OP_getFunctionCalls_dot___hash_lambda2 x1 x3500 = Curry_Prelude.OP_Tuple2 x1 (Curry_Dependency2.d_C_callsDirectly x1 x3500)

d_C_getCalls :: Curry_Prelude.OP_List C_Declaration -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_Declaration (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))))
d_C_getCalls x1 x3500 = Curry_Prelude.d_C_map d_OP_getCalls_dot___hash_lambda4 x1 x3500

d_OP_getCalls_dot_callHelp_dot_11 :: C_Declaration -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP_getCalls_dot_callHelp_dot_11 x1 x3500 = case x1 of
     (C_F x2) -> Curry_Dependency2.d_C_callsDirectly x2 x3500
     (C_T x3) -> Curry_Dependency2.d_C_callsDirectly2 x3 x3500
     (Choice_C_Declaration x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_getCalls_dot_callHelp_dot_11 x1002 x3500) (d_OP_getCalls_dot_callHelp_dot_11 x1003 x3500)
     (Choices_C_Declaration x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_getCalls_dot_callHelp_dot_11 z x3500) x1002
     (Guard_C_Declaration x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_getCalls_dot_callHelp_dot_11 x1002) $! (addCs x1001 x3500))
     (Fail_C_Declaration x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_getCalls_dot___hash_lambda4 :: C_Declaration -> ConstStore -> Curry_Prelude.OP_Tuple2 C_Declaration (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))
d_OP_getCalls_dot___hash_lambda4 x1 x3500 = Curry_Prelude.OP_Tuple2 x1 (d_OP_getCalls_dot_callHelp_dot_11 x1 x3500)

d_C_fullIteration :: (Curry_Prelude.Curry t1,Curry_Prelude.Curry t0) => (Curry_FiniteMap.C_FM (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) t0 -> ConstStore -> Curry_Prelude.OP_Tuple2 t1 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) t0) -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 t1 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))) -> Curry_FiniteMap.C_FM (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) t0 -> Curry_FiniteMap.C_FM (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) t0 -> ConstStore -> Curry_FiniteMap.C_FM (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) t0
d_C_fullIteration x1 x2 x3 x4 x3500 = let
     x5 = Curry_Prelude.d_OP_dollar (Curry_FiniteMap.d_C_listToFM (acceptCs id Curry_Prelude.d_OP_lt) x3500) (Curry_Prelude.d_C_map (Curry_Prelude.d_C_apply x1 (Curry_FiniteMap.d_C_plusFM x3 x4 x3500) x3500) x2 x3500) x3500
      in (d_OP__case_22 x1 x2 x3 x4 x5 (Curry_FiniteMap.d_C_eqFM x4 x5 x3500) x3500)

nd_C_fullIteration :: (Curry_Prelude.Curry t1,Curry_Prelude.Curry t0) => Func (Curry_FiniteMap.C_FM (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) t0) (Func (Curry_Prelude.OP_Tuple2 t1 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) t0)) -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 t1 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))) -> Curry_FiniteMap.C_FM (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) t0 -> Curry_FiniteMap.C_FM (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) t0 -> IDSupply -> ConstStore -> Curry_FiniteMap.C_FM (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) t0
nd_C_fullIteration x1 x2 x3 x4 x3000 x3500 = let
     x2012 = x3000
      in (seq x2012 (let
          x2007 = leftSupply x2012
          x2011 = rightSupply x2012
           in (seq x2007 (seq x2011 (let
               x5 = let
                    x2006 = leftSupply x2007
                    x2008 = rightSupply x2007
                     in (seq x2006 (seq x2008 (let
                         x2000 = leftSupply x2008
                         x2005 = rightSupply x2008
                          in (seq x2000 (seq x2005 (Curry_Prelude.nd_OP_dollar (Curry_FiniteMap.nd_C_listToFM (wrapDX (wrapDX id) (acceptCs id Curry_Prelude.d_OP_lt)) x2000 x3500) (let
                              x2004 = leftSupply x2005
                              x2003 = rightSupply x2005
                               in (seq x2004 (seq x2003 (Curry_Prelude.nd_C_map (let
                                   x2002 = leftSupply x2003
                                   x2001 = rightSupply x2003
                                    in (seq x2002 (seq x2001 (Curry_Prelude.nd_C_apply x1 (Curry_FiniteMap.nd_C_plusFM x3 x4 x2001 x3500) x2002 x3500)))) x2 x2004 x3500)))) x2006 x3500))))))
                in (let
                    x2010 = leftSupply x2011
                    x2009 = rightSupply x2011
                     in (seq x2010 (seq x2009 (nd_OP__case_22 x1 x2 x3 x4 x5 (Curry_FiniteMap.nd_C_eqFM x4 x5 x2009 x3500) x2010 x3500)))))))))

d_C_initNDResult :: ConstStore -> Curry_FiniteMap.C_FM (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Base.C_NDClass
d_C_initNDResult x3500 = Curry_Prelude.d_C_apply (Curry_FiniteMap.d_C_listToFM (acceptCs id Curry_Prelude.d_OP_lt) x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (d_C_qmark x3500) Curry_Base.C_ND) Curry_Prelude.OP_List) x3500

nd_C_initNDResult :: IDSupply -> ConstStore -> Curry_FiniteMap.C_FM (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Base.C_NDClass
nd_C_initNDResult x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_FiniteMap.nd_C_listToFM (wrapDX (wrapDX id) (acceptCs id Curry_Prelude.d_OP_lt)) x2000 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (d_C_qmark x3500) Curry_Base.C_ND) Curry_Prelude.OP_List) x2001 x3500)))))

d_C_analyseND :: Curry_FlatCurry.C_Prog -> Curry_FiniteMap.C_FM (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Base.C_NDClass -> ConstStore -> Curry_FiniteMap.C_FM (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Base.C_NDClass
d_C_analyseND x1 x2 x3500 = let
     x3 = Curry_Prelude.d_C_apply (Curry_FlatCurryGoodies.d_C_progFuncs x3500) x1 x3500
     x4 = Curry_Prelude.d_OP_dollar (Curry_FiniteMap.d_C_listToFM (acceptCs id Curry_Prelude.d_OP_lt) x3500) (Curry_Prelude.d_C_map d_OP_analyseND_dot___hash_lambda5 x3 x3500) x3500
      in (d_C_fullIteration (acceptCs id d_C_ndFunc) (d_C_getFunctionCalls x3 x3500) x2 x4 x3500)

nd_C_analyseND :: Curry_FlatCurry.C_Prog -> Curry_FiniteMap.C_FM (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Base.C_NDClass -> IDSupply -> ConstStore -> Curry_FiniteMap.C_FM (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Base.C_NDClass
nd_C_analyseND x1 x2 x3000 x3500 = let
     x2009 = x3000
      in (seq x2009 (let
          x2002 = leftSupply x2009
          x2010 = rightSupply x2009
           in (seq x2002 (seq x2010 (let
               x2006 = leftSupply x2010
               x2008 = rightSupply x2010
                in (seq x2006 (seq x2008 (let
                    x3 = let
                         x2001 = leftSupply x2002
                         x2000 = rightSupply x2002
                          in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_FlatCurryGoodies.nd_C_progFuncs x2000 x3500) x1 x2001 x3500)))
                    x4 = let
                         x2005 = leftSupply x2006
                         x2007 = rightSupply x2006
                          in (seq x2005 (seq x2007 (let
                              x2003 = leftSupply x2007
                              x2004 = rightSupply x2007
                               in (seq x2003 (seq x2004 (Curry_Prelude.nd_OP_dollar (Curry_FiniteMap.nd_C_listToFM (wrapDX (wrapDX id) (acceptCs id Curry_Prelude.d_OP_lt)) x2003 x3500) (Curry_Prelude.nd_C_map (wrapDX id d_OP_analyseND_dot___hash_lambda5) x3 x2004 x3500) x2005 x3500))))))
                     in (nd_C_fullIteration (wrapDX (wrapNX id) (acceptCs id nd_C_ndFunc)) (d_C_getFunctionCalls x3 x3500) x2 x4 x2008 x3500)))))))))

d_OP_analyseND_dot___hash_lambda5 :: Curry_FlatCurry.C_FuncDecl -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Base.C_NDClass
d_OP_analyseND_dot___hash_lambda5 x1 x3500 = let
     x2 = Curry_Prelude.d_C_apply (Curry_FlatCurryGoodies.d_C_funcName x3500) x1 x3500
      in (Curry_Prelude.OP_Tuple2 x2 (d_OP__case_21 x2 (Curry_Prelude.d_OP_eq_eq x2 (d_C_qmark x3500) x3500) x3500))

d_C_ndFunc :: Curry_FiniteMap.C_FM (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Base.C_NDClass -> Curry_Prelude.OP_Tuple2 Curry_FlatCurry.C_FuncDecl (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Base.C_NDClass
d_C_ndFunc x1 x2 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x3 x4) -> let
          x5 = Curry_Prelude.d_C_apply (Curry_FlatCurryGoodies.d_C_funcName x3500) x3 x3500
          x6 = Curry_Prelude.d_C_apply (Curry_FlatCurryGoodies.d_C_funcRule x3500) x3 x3500
          x7 = Curry_Prelude.d_OP_dollar (Curry_Prelude.d_C_any (Curry_Prelude.d_C_flip (acceptCs id Curry_Prelude.d_OP_eq_eq) (Curry_Prelude.C_Just Curry_Base.C_ND)) x3500) (Curry_Prelude.d_C_map (Curry_FiniteMap.d_C_lookupFM x1) x4 x3500) x3500
          x8 = Curry_Prelude.OP_Tuple2 x5 (Curry_Prelude.d_OP_dollar Curry_Maybe.d_C_fromJust (Curry_FiniteMap.d_C_lookupFM x1 x5 x3500) x3500)
           in (d_OP__case_20 x5 x6 x7 x8 (Curry_Prelude.d_C_apply (Curry_FlatCurryGoodies.d_C_isRuleExternal x3500) x6 x3500) x3500)
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_ndFunc x1 x1002 x3500) (d_C_ndFunc x1 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_ndFunc x1 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_ndFunc x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_ndFunc :: Curry_FiniteMap.C_FM (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Base.C_NDClass -> Curry_Prelude.OP_Tuple2 Curry_FlatCurry.C_FuncDecl (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> IDSupply -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Base.C_NDClass
nd_C_ndFunc x1 x2 x3000 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x3 x4) -> let
          x2019 = x3000
           in (seq x2019 (let
               x2020 = leftSupply x2019
               x2021 = rightSupply x2019
                in (seq x2020 (seq x2021 (let
                    x2002 = leftSupply x2020
                    x2005 = rightSupply x2020
                     in (seq x2002 (seq x2005 (let
                         x2009 = leftSupply x2021
                         x2022 = rightSupply x2021
                          in (seq x2009 (seq x2022 (let
                              x2013 = leftSupply x2022
                              x2018 = rightSupply x2022
                               in (seq x2013 (seq x2018 (let
                                   x5 = let
                                        x2001 = leftSupply x2002
                                        x2000 = rightSupply x2002
                                         in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_FlatCurryGoodies.nd_C_funcName x2000 x3500) x3 x2001 x3500)))
                                   x6 = let
                                        x2004 = leftSupply x2005
                                        x2003 = rightSupply x2005
                                         in (seq x2004 (seq x2003 (Curry_Prelude.nd_C_apply (Curry_FlatCurryGoodies.nd_C_funcRule x2003 x3500) x3 x2004 x3500)))
                                   x7 = let
                                        x2008 = leftSupply x2009
                                        x2010 = rightSupply x2009
                                         in (seq x2008 (seq x2010 (let
                                             x2006 = leftSupply x2010
                                             x2007 = rightSupply x2010
                                              in (seq x2006 (seq x2007 (Curry_Prelude.nd_OP_dollar (Curry_Prelude.nd_C_any (wrapNX id (Curry_Prelude.nd_C_flip (wrapDX (wrapDX id) (acceptCs id Curry_Prelude.d_OP_eq_eq)) (Curry_Prelude.C_Just Curry_Base.C_ND))) x2006 x3500) (Curry_Prelude.nd_C_map (wrapNX id (Curry_FiniteMap.nd_C_lookupFM x1)) x4 x2007 x3500) x2008 x3500))))))
                                   x8 = Curry_Prelude.OP_Tuple2 x5 (let
                                        x2012 = leftSupply x2013
                                        x2011 = rightSupply x2013
                                         in (seq x2012 (seq x2011 (Curry_Prelude.nd_OP_dollar (wrapDX id Curry_Maybe.d_C_fromJust) (Curry_FiniteMap.nd_C_lookupFM x1 x5 x2011 x3500) x2012 x3500))))
                                    in (let
                                        x2017 = leftSupply x2018
                                        x2016 = rightSupply x2018
                                         in (seq x2017 (seq x2016 (nd_OP__case_20 x5 x6 x7 x8 (let
                                             x2015 = leftSupply x2016
                                             x2014 = rightSupply x2016
                                              in (seq x2015 (seq x2014 (Curry_Prelude.nd_C_apply (Curry_FlatCurryGoodies.nd_C_isRuleExternal x2014 x3500) x6 x2015 x3500)))) x2017 x3500))))))))))))))))))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_ndFunc x1 x1002 x3000 x3500) (nd_C_ndFunc x1 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_ndFunc x1 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_ndFunc x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_isNDExpr :: ConstStore -> Curry_FlatCurry.C_Expr -> ConstStore -> Curry_Prelude.C_Bool
d_C_isNDExpr x3500 = Curry_FlatCurryGoodies.d_C_trExpr d_OP_isNDExpr_dot_cf_dot_40 d_OP_isNDExpr_dot_cf_dot_40 (acceptCs (acceptCs id) d_OP_isNDExpr_dot_combf_dot_40) (acceptCs id d_OP_isNDExpr_dot_letf_dot_40) (acceptCs id d_OP_isNDExpr_dot_freef_dot_40) (acceptCs id d_OP_isNDExpr_dot_orf_dot_40) (acceptCs (acceptCs id) d_OP_isNDExpr_dot_casef_dot_40) (acceptCs id d_OP_isNDExpr_dot_branchf_dot_40) (acceptCs id d_OP_isNDExpr_dot_typedf_dot_40)

nd_C_isNDExpr :: IDSupply -> ConstStore -> Func Curry_FlatCurry.C_Expr Curry_Prelude.C_Bool
nd_C_isNDExpr x3000 x3500 = wrapNX id (Curry_FlatCurryGoodies.nd_C_trExpr (wrapDX id d_OP_isNDExpr_dot_cf_dot_40) (wrapDX id d_OP_isNDExpr_dot_cf_dot_40) (wrapDX (wrapDX (wrapDX id)) (acceptCs (acceptCs id) d_OP_isNDExpr_dot_combf_dot_40)) (wrapDX (wrapDX id) (acceptCs id d_OP_isNDExpr_dot_letf_dot_40)) (wrapDX (wrapDX id) (acceptCs id d_OP_isNDExpr_dot_freef_dot_40)) (wrapDX (wrapDX id) (acceptCs id d_OP_isNDExpr_dot_orf_dot_40)) (wrapDX (wrapDX (wrapDX id)) (acceptCs (acceptCs id) d_OP_isNDExpr_dot_casef_dot_40)) (wrapDX (wrapDX id) (acceptCs id d_OP_isNDExpr_dot_branchf_dot_40)) (wrapDX (wrapDX id) (acceptCs id d_OP_isNDExpr_dot_typedf_dot_40)))

d_OP_isNDExpr_dot_cf_dot_40 :: Curry_Prelude.Curry t0 => t0 -> ConstStore -> Curry_Prelude.C_Bool
d_OP_isNDExpr_dot_cf_dot_40 x1 x3500 = Curry_Prelude.d_C_const Curry_Prelude.C_False x1 x3500

d_OP_isNDExpr_dot_combf_dot_40 :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => t0 -> t1 -> Curry_Prelude.OP_List Curry_Prelude.C_Bool -> ConstStore -> Curry_Prelude.C_Bool
d_OP_isNDExpr_dot_combf_dot_40 x1 x2 x3 x3500 = Curry_Prelude.d_C_apply (Curry_Prelude.d_C_or x3500) x3 x3500

d_OP_isNDExpr_dot_letf_dot_40 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 t0 Curry_Prelude.C_Bool) -> Curry_Prelude.C_Bool -> ConstStore -> Curry_Prelude.C_Bool
d_OP_isNDExpr_dot_letf_dot_40 x1 x2 x3500 = Curry_Prelude.d_OP_dollar (Curry_Prelude.d_C_or x3500) (Curry_Prelude.OP_Cons x2 (Curry_Prelude.d_C_map Curry_Prelude.d_C_snd x1 x3500)) x3500

d_OP_isNDExpr_dot_freef_dot_40 :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => t0 -> t1 -> ConstStore -> Curry_Prelude.C_Bool
d_OP_isNDExpr_dot_freef_dot_40 x1 x2 x3500 = Curry_Prelude.C_True

d_OP_isNDExpr_dot_orf_dot_40 :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => t0 -> t1 -> ConstStore -> Curry_Prelude.C_Bool
d_OP_isNDExpr_dot_orf_dot_40 x1 x2 x3500 = Curry_Prelude.C_True

d_OP_isNDExpr_dot_casef_dot_40 :: Curry_Prelude.Curry t0 => t0 -> Curry_Prelude.C_Bool -> Curry_Prelude.OP_List Curry_Prelude.C_Bool -> ConstStore -> Curry_Prelude.C_Bool
d_OP_isNDExpr_dot_casef_dot_40 x1 x2 x3 x3500 = Curry_Prelude.d_C_apply (Curry_Prelude.d_C_or x3500) (Curry_Prelude.OP_Cons x2 x3) x3500

d_OP_isNDExpr_dot_branchf_dot_40 :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => t0 -> t1 -> ConstStore -> t1
d_OP_isNDExpr_dot_branchf_dot_40 x1 x2 x3500 = x2

d_OP_isNDExpr_dot_typedf_dot_40 :: (Curry_Prelude.Curry t1,Curry_Prelude.Curry t0) => t0 -> t1 -> ConstStore -> t0
d_OP_isNDExpr_dot_typedf_dot_40 x1 x2 x3500 = x1

d_C_qmark :: ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_C_qmark x3500 = Curry_Names.d_C_renameQName (Curry_Prelude.OP_Tuple2 (Curry_Names.d_C_prelude x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '?'#) Curry_Prelude.OP_List)) x3500

d_C_initHOResult :: ConstStore -> Curry_FiniteMap.C_FM (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Base.C_HOClass
d_C_initHOResult x3500 = Curry_FiniteMap.d_C_emptyFM (acceptCs id Curry_Prelude.d_OP_lt) x3500

nd_C_initHOResult :: IDSupply -> ConstStore -> Curry_FiniteMap.C_FM (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Base.C_HOClass
nd_C_initHOResult x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_FiniteMap.nd_C_emptyFM (wrapDX (wrapDX id) (acceptCs id Curry_Prelude.d_OP_lt)) x2000 x3500))

d_C_analyseHOFunc :: Curry_FlatCurry.C_Prog -> Curry_FiniteMap.C_FM (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Base.C_HOClass -> ConstStore -> Curry_FiniteMap.C_FM (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Base.C_HOClass
d_C_analyseHOFunc x1 x2 x3500 = let
     x3 = Curry_Prelude.d_OP_dollar (Curry_Prelude.d_C_map (acceptCs id C_F)) (Curry_Prelude.d_C_apply (Curry_FlatCurryGoodies.d_C_progFuncs x3500) x1 x3500) x3500
     x4 = Curry_Prelude.d_OP_dollar (Curry_Prelude.d_C_map (acceptCs id C_T)) (Curry_Prelude.d_C_apply (Curry_FlatCurryGoodies.d_C_progTypes x3500) x1 x3500) x3500
     x5 = Curry_Prelude.d_OP_plus_plus x3 x4 x3500
     x6 = Curry_Prelude.d_OP_dollar (Curry_FiniteMap.d_C_listToFM (acceptCs id Curry_Prelude.d_OP_lt) x3500) (Curry_Prelude.d_C_map d_OP_analyseHOFunc_dot___hash_lambda6 x5 x3500) x3500
      in (d_C_fullIteration (acceptCs id d_C_ordFunc) (d_C_getCalls x5 x3500) x2 x6 x3500)

nd_C_analyseHOFunc :: Curry_FlatCurry.C_Prog -> Curry_FiniteMap.C_FM (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Base.C_HOClass -> IDSupply -> ConstStore -> Curry_FiniteMap.C_FM (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Base.C_HOClass
nd_C_analyseHOFunc x1 x2 x3000 x3500 = let
     x2016 = x3000
      in (seq x2016 (let
          x2017 = leftSupply x2016
          x2018 = rightSupply x2016
           in (seq x2017 (seq x2018 (let
               x2004 = leftSupply x2017
               x2009 = rightSupply x2017
                in (seq x2004 (seq x2009 (let
                    x2013 = leftSupply x2018
                    x2015 = rightSupply x2018
                     in (seq x2013 (seq x2015 (let
                         x3 = let
                              x2003 = leftSupply x2004
                              x2002 = rightSupply x2004
                               in (seq x2003 (seq x2002 (Curry_Prelude.nd_OP_dollar (wrapNX id (Curry_Prelude.nd_C_map (wrapDX id (acceptCs id C_F)))) (let
                                   x2001 = leftSupply x2002
                                   x2000 = rightSupply x2002
                                    in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_FlatCurryGoodies.nd_C_progFuncs x2000 x3500) x1 x2001 x3500)))) x2003 x3500)))
                         x4 = let
                              x2008 = leftSupply x2009
                              x2007 = rightSupply x2009
                               in (seq x2008 (seq x2007 (Curry_Prelude.nd_OP_dollar (wrapNX id (Curry_Prelude.nd_C_map (wrapDX id (acceptCs id C_T)))) (let
                                   x2006 = leftSupply x2007
                                   x2005 = rightSupply x2007
                                    in (seq x2006 (seq x2005 (Curry_Prelude.nd_C_apply (Curry_FlatCurryGoodies.nd_C_progTypes x2005 x3500) x1 x2006 x3500)))) x2008 x3500)))
                         x5 = Curry_Prelude.d_OP_plus_plus x3 x4 x3500
                         x6 = let
                              x2012 = leftSupply x2013
                              x2014 = rightSupply x2013
                               in (seq x2012 (seq x2014 (let
                                   x2010 = leftSupply x2014
                                   x2011 = rightSupply x2014
                                    in (seq x2010 (seq x2011 (Curry_Prelude.nd_OP_dollar (Curry_FiniteMap.nd_C_listToFM (wrapDX (wrapDX id) (acceptCs id Curry_Prelude.d_OP_lt)) x2010 x3500) (Curry_Prelude.nd_C_map (wrapDX id d_OP_analyseHOFunc_dot___hash_lambda6) x5 x2011 x3500) x2012 x3500))))))
                          in (nd_C_fullIteration (wrapDX (wrapNX id) (acceptCs id nd_C_ordFunc)) (d_C_getCalls x5 x3500) x2 x6 x2015 x3500))))))))))))

d_OP_analyseHOFunc_dot___hash_lambda6 :: C_Declaration -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Base.C_HOClass
d_OP_analyseHOFunc_dot___hash_lambda6 x1 x3500 = Curry_Prelude.OP_Tuple2 (d_C_getDeclName x1 x3500) Curry_Base.C_FO

d_C_analyseHOCons :: Curry_FlatCurry.C_Prog -> ConstStore -> Curry_FiniteMap.C_FM (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Base.C_HOClass
d_C_analyseHOCons x1 x3500 = Curry_Prelude.d_OP_dollar (Curry_FiniteMap.d_C_listToFM (acceptCs id Curry_Prelude.d_OP_lt) x3500) (Curry_Prelude.d_OP_dollar (acceptCs id (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))))))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List)))))))))) Curry_Base.C_FO))) (Curry_Prelude.d_OP_dollar (Curry_Prelude.d_C_map d_C_consOrder) (Curry_Prelude.d_OP_dollar (Curry_Prelude.d_C_concatMap (Curry_FlatCurryGoodies.d_C_typeConsDecls x3500) x3500) (Curry_Prelude.d_OP_dollar (Curry_Prelude.d_C_filter (Curry_Prelude.d_OP_dot Curry_Prelude.d_C_not (Curry_FlatCurryGoodies.d_C_isTypeSyn x3500) x3500)) (Curry_Prelude.d_C_apply (Curry_FlatCurryGoodies.d_C_progTypes x3500) x1 x3500) x3500) x3500) x3500) x3500) x3500

nd_C_analyseHOCons :: Curry_FlatCurry.C_Prog -> IDSupply -> ConstStore -> Curry_FiniteMap.C_FM (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Base.C_HOClass
nd_C_analyseHOCons x1 x3000 x3500 = let
     x2021 = x3000
      in (seq x2021 (let
          x2020 = leftSupply x2021
          x2022 = rightSupply x2021
           in (seq x2020 (seq x2022 (let
               x2000 = leftSupply x2022
               x2019 = rightSupply x2022
                in (seq x2000 (seq x2019 (Curry_Prelude.nd_OP_dollar (Curry_FiniteMap.nd_C_listToFM (wrapDX (wrapDX id) (acceptCs id Curry_Prelude.d_OP_lt)) x2000 x3500) (let
                    x2018 = leftSupply x2019
                    x2017 = rightSupply x2019
                     in (seq x2018 (seq x2017 (Curry_Prelude.nd_OP_dollar (wrapDX id (acceptCs id (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))))))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List)))))))))) Curry_Base.C_FO)))) (let
                         x2016 = leftSupply x2017
                         x2014 = rightSupply x2017
                          in (seq x2016 (seq x2014 (Curry_Prelude.nd_OP_dollar (wrapNX id (Curry_Prelude.nd_C_map (wrapDX id d_C_consOrder))) (let
                              x2013 = leftSupply x2014
                              x2015 = rightSupply x2014
                               in (seq x2013 (seq x2015 (let
                                   x2003 = leftSupply x2015
                                   x2011 = rightSupply x2015
                                    in (seq x2003 (seq x2011 (Curry_Prelude.nd_OP_dollar (let
                                        x2002 = leftSupply x2003
                                        x2001 = rightSupply x2003
                                         in (seq x2002 (seq x2001 (Curry_Prelude.nd_C_concatMap (Curry_FlatCurryGoodies.nd_C_typeConsDecls x2001 x3500) x2002 x3500)))) (let
                                        x2010 = leftSupply x2011
                                        x2012 = rightSupply x2011
                                         in (seq x2010 (seq x2012 (let
                                             x2006 = leftSupply x2012
                                             x2009 = rightSupply x2012
                                              in (seq x2006 (seq x2009 (Curry_Prelude.nd_OP_dollar (wrapNX id (Curry_Prelude.nd_C_filter (let
                                                  x2005 = leftSupply x2006
                                                  x2004 = rightSupply x2006
                                                   in (seq x2005 (seq x2004 (Curry_Prelude.nd_OP_dot (wrapDX id Curry_Prelude.d_C_not) (Curry_FlatCurryGoodies.nd_C_isTypeSyn x2004 x3500) x2005 x3500)))))) (let
                                                  x2008 = leftSupply x2009
                                                  x2007 = rightSupply x2009
                                                   in (seq x2008 (seq x2007 (Curry_Prelude.nd_C_apply (Curry_FlatCurryGoodies.nd_C_progTypes x2007 x3500) x1 x2008 x3500)))) x2010 x3500))))))) x2013 x3500))))))) x2016 x3500)))) x2018 x3500)))) x2020 x3500))))))))

d_C_consOrder :: Curry_FlatCurry.C_ConsDecl -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Base.C_HOClass
d_C_consOrder x1 x3500 = case x1 of
     (Curry_FlatCurry.C_Cons x2 x3 x4 x5) -> Curry_Prelude.OP_Tuple2 x2 (d_OP_consOrder_dot_consOrder'_dot_83 x5 x3500)
     (Curry_FlatCurry.Choice_C_ConsDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_consOrder x1002 x3500) (d_C_consOrder x1003 x3500)
     (Curry_FlatCurry.Choices_C_ConsDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_consOrder z x3500) x1002
     (Curry_FlatCurry.Guard_C_ConsDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_consOrder x1002) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_ConsDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_consOrder_dot_consOrder'_dot_83 :: Curry_Prelude.OP_List Curry_FlatCurry.C_TypeExpr -> ConstStore -> Curry_Base.C_HOClass
d_OP_consOrder_dot_consOrder'_dot_83 x1 x3500 = case x1 of
     Curry_Prelude.OP_List -> Curry_Base.C_FO
     (Curry_Prelude.OP_Cons x2 x3) -> d_OP__case_16 x3 x2 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_consOrder_dot_consOrder'_dot_83 x1002 x3500) (d_OP_consOrder_dot_consOrder'_dot_83 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_consOrder_dot_consOrder'_dot_83 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_consOrder_dot_consOrder'_dot_83 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_hoOr :: Curry_Base.C_HOClass -> Curry_Base.C_HOClass -> ConstStore -> Curry_Base.C_HOClass
d_C_hoOr x1 x2 x3500 = case x1 of
     Curry_Base.C_HO -> Curry_Base.C_HO
     Curry_Base.C_FO -> x2
     (Curry_Base.Choice_C_HOClass x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_hoOr x1002 x2 x3500) (d_C_hoOr x1003 x2 x3500)
     (Curry_Base.Choices_C_HOClass x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_hoOr z x2 x3500) x1002
     (Curry_Base.Guard_C_HOClass x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_hoOr x1002 x2) $! (addCs x1001 x3500))
     (Curry_Base.Fail_C_HOClass x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_ordFunc :: Curry_FiniteMap.C_FM (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Base.C_HOClass -> Curry_Prelude.OP_Tuple2 C_Declaration (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Base.C_HOClass
d_C_ordFunc x1 x2 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x3 x4) -> d_OP__case_15 x1 x3 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_ordFunc x1 x1002 x3500) (d_C_ordFunc x1 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_ordFunc x1 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_ordFunc x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_ordFunc :: Curry_FiniteMap.C_FM (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Base.C_HOClass -> Curry_Prelude.OP_Tuple2 C_Declaration (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> IDSupply -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Base.C_HOClass
nd_C_ordFunc x1 x2 x3000 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x3 x4) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_15 x1 x3 x2000 x3500))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_ordFunc x1 x1002 x3000 x3500) (nd_C_ordFunc x1 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_ordFunc x1 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_ordFunc x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_goThroughConsList :: Curry_FiniteMap.C_FM (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Base.C_HOClass -> Curry_Prelude.OP_List Curry_FlatCurry.C_ConsDecl -> ConstStore -> Curry_Base.C_HOClass
d_C_goThroughConsList x1 x2 x3500 = case x2 of
     Curry_Prelude.OP_List -> Curry_Base.C_FO
     (Curry_Prelude.OP_Cons x3 x4) -> let
          x5 = d_OP_goThroughConsList_dot___hash_selFP2_hash_typeExprs x3 x3500
           in (d_C_hoOr (Curry_Prelude.d_C_foldr (acceptCs id (d_OP_goThroughConsList_dot___hash_lambda8 x1)) Curry_Base.C_FO x5 x3500) (d_C_goThroughConsList x1 x4 x3500) x3500)
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_goThroughConsList x1 x1002 x3500) (d_C_goThroughConsList x1 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_goThroughConsList x1 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_goThroughConsList x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_goThroughConsList :: Curry_FiniteMap.C_FM (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Base.C_HOClass -> Curry_Prelude.OP_List Curry_FlatCurry.C_ConsDecl -> IDSupply -> ConstStore -> Curry_Base.C_HOClass
nd_C_goThroughConsList x1 x2 x3000 x3500 = case x2 of
     Curry_Prelude.OP_List -> Curry_Base.C_FO
     (Curry_Prelude.OP_Cons x3 x4) -> let
          x2002 = x3000
           in (seq x2002 (let
               x5 = d_OP_goThroughConsList_dot___hash_selFP2_hash_typeExprs x3 x3500
                in (let
                    x2000 = leftSupply x2002
                    x2001 = rightSupply x2002
                     in (seq x2000 (seq x2001 (d_C_hoOr (Curry_Prelude.nd_C_foldr (wrapDX (wrapNX id) (acceptCs id (nd_OP_goThroughConsList_dot___hash_lambda8 x1))) Curry_Base.C_FO x5 x2000 x3500) (nd_C_goThroughConsList x1 x4 x2001 x3500) x3500))))))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_goThroughConsList x1 x1002 x3000 x3500) (nd_C_goThroughConsList x1 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_goThroughConsList x1 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_goThroughConsList x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_goThroughConsList_dot___hash_selFP2_hash_typeExprs :: Curry_FlatCurry.C_ConsDecl -> ConstStore -> Curry_Prelude.OP_List Curry_FlatCurry.C_TypeExpr
d_OP_goThroughConsList_dot___hash_selFP2_hash_typeExprs x1 x3500 = case x1 of
     (Curry_FlatCurry.C_Cons x2 x3 x4 x5) -> x5
     (Curry_FlatCurry.Choice_C_ConsDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_goThroughConsList_dot___hash_selFP2_hash_typeExprs x1002 x3500) (d_OP_goThroughConsList_dot___hash_selFP2_hash_typeExprs x1003 x3500)
     (Curry_FlatCurry.Choices_C_ConsDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_goThroughConsList_dot___hash_selFP2_hash_typeExprs z x3500) x1002
     (Curry_FlatCurry.Guard_C_ConsDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_goThroughConsList_dot___hash_selFP2_hash_typeExprs x1002) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_ConsDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_goThroughConsList_dot___hash_lambda8 :: Curry_FiniteMap.C_FM (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Base.C_HOClass -> Curry_FlatCurry.C_TypeExpr -> Curry_Base.C_HOClass -> ConstStore -> Curry_Base.C_HOClass
d_OP_goThroughConsList_dot___hash_lambda8 x1 x2 x3 x3500 = d_C_hoOr x3 (d_C_ordHelp1 x1 x2 x3500) x3500

nd_OP_goThroughConsList_dot___hash_lambda8 :: Curry_FiniteMap.C_FM (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Base.C_HOClass -> Curry_FlatCurry.C_TypeExpr -> Curry_Base.C_HOClass -> IDSupply -> ConstStore -> Curry_Base.C_HOClass
nd_OP_goThroughConsList_dot___hash_lambda8 x1 x2 x3 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (d_C_hoOr x3 (nd_C_ordHelp1 x1 x2 x2000 x3500) x3500))

d_C_ordHelp1 :: Curry_FiniteMap.C_FM (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Base.C_HOClass -> Curry_FlatCurry.C_TypeExpr -> ConstStore -> Curry_Base.C_HOClass
d_C_ordHelp1 x1 x2 x3500 = case x2 of
     (Curry_FlatCurry.C_TVar x3) -> Curry_Base.C_FO
     (Curry_FlatCurry.C_FuncType x4 x5) -> Curry_Base.C_HO
     (Curry_FlatCurry.C_TCons x6 x7) -> d_C_hoOr (Curry_Maybe.d_C_fromMaybe Curry_Base.C_FO (Curry_FiniteMap.d_C_lookupFM x1 x6 x3500) x3500) (Curry_Prelude.d_C_foldr (acceptCs id (d_OP_ordHelp1_dot___hash_lambda9 x1)) Curry_Base.C_FO x7 x3500) x3500
     (Curry_FlatCurry.Choice_C_TypeExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_ordHelp1 x1 x1002 x3500) (d_C_ordHelp1 x1 x1003 x3500)
     (Curry_FlatCurry.Choices_C_TypeExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_ordHelp1 x1 z x3500) x1002
     (Curry_FlatCurry.Guard_C_TypeExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_ordHelp1 x1 x1002) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_TypeExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_ordHelp1 :: Curry_FiniteMap.C_FM (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Base.C_HOClass -> Curry_FlatCurry.C_TypeExpr -> IDSupply -> ConstStore -> Curry_Base.C_HOClass
nd_C_ordHelp1 x1 x2 x3000 x3500 = case x2 of
     (Curry_FlatCurry.C_TVar x3) -> Curry_Base.C_FO
     (Curry_FlatCurry.C_FuncType x4 x5) -> Curry_Base.C_HO
     (Curry_FlatCurry.C_TCons x6 x7) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2000 = leftSupply x2002
               x2001 = rightSupply x2002
                in (seq x2000 (seq x2001 (d_C_hoOr (Curry_Maybe.d_C_fromMaybe Curry_Base.C_FO (Curry_FiniteMap.nd_C_lookupFM x1 x6 x2000 x3500) x3500) (Curry_Prelude.nd_C_foldr (wrapDX (wrapNX id) (acceptCs id (nd_OP_ordHelp1_dot___hash_lambda9 x1))) Curry_Base.C_FO x7 x2001 x3500) x3500)))))
     (Curry_FlatCurry.Choice_C_TypeExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_ordHelp1 x1 x1002 x3000 x3500) (nd_C_ordHelp1 x1 x1003 x3000 x3500)
     (Curry_FlatCurry.Choices_C_TypeExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_ordHelp1 x1 z x3000 x3500) x1002
     (Curry_FlatCurry.Guard_C_TypeExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_ordHelp1 x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_TypeExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_ordHelp1_dot___hash_lambda9 :: Curry_FiniteMap.C_FM (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Base.C_HOClass -> Curry_FlatCurry.C_TypeExpr -> Curry_Base.C_HOClass -> ConstStore -> Curry_Base.C_HOClass
d_OP_ordHelp1_dot___hash_lambda9 x1 x2 x3 x3500 = d_C_hoOr x3 (d_C_ordHelp1 x1 x2 x3500) x3500

nd_OP_ordHelp1_dot___hash_lambda9 :: Curry_FiniteMap.C_FM (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Base.C_HOClass -> Curry_FlatCurry.C_TypeExpr -> Curry_Base.C_HOClass -> IDSupply -> ConstStore -> Curry_Base.C_HOClass
nd_OP_ordHelp1_dot___hash_lambda9 x1 x2 x3 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (d_C_hoOr x3 (nd_C_ordHelp1 x1 x2 x2000 x3500) x3500))

d_C_ordHelp2 :: Curry_FlatCurry.C_TypeExpr -> Curry_Prelude.C_Int -> Curry_FiniteMap.C_FM (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Base.C_HOClass -> ConstStore -> Curry_Base.C_HOClass
d_C_ordHelp2 x1 x2 x3 x3500 = d_OP__case_13 x1 x2 x3 (Curry_Prelude.d_OP_eq_eq x2 (Curry_Prelude.C_Int 0#) x3500) x3500

nd_C_ordHelp2 :: Curry_FlatCurry.C_TypeExpr -> Curry_Prelude.C_Int -> Curry_FiniteMap.C_FM (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Base.C_HOClass -> IDSupply -> ConstStore -> Curry_Base.C_HOClass
nd_C_ordHelp2 x1 x2 x3 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (nd_OP__case_13 x1 x2 x3 (Curry_Prelude.d_OP_eq_eq x2 (Curry_Prelude.C_Int 0#) x3500) x2000 x3500))

d_C_getPrivateFunc :: C_Visibilities -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_C_getPrivateFunc x1 x3500 = case x1 of
     (C_Vis x2 x3 x4) -> d_OP__case_6 x2 x3500
     (Choice_C_Visibilities x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_getPrivateFunc x1002 x3500) (d_C_getPrivateFunc x1003 x3500)
     (Choices_C_Visibilities x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_getPrivateFunc z x3500) x1002
     (Guard_C_Visibilities x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_getPrivateFunc x1002) $! (addCs x1001 x3500))
     (Fail_C_Visibilities x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_getPublicFunc :: C_Visibilities -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_C_getPublicFunc x1 x3500 = case x1 of
     (C_Vis x2 x3 x4) -> d_OP__case_5 x2 x3500
     (Choice_C_Visibilities x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_getPublicFunc x1002 x3500) (d_C_getPublicFunc x1003 x3500)
     (Choices_C_Visibilities x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_getPublicFunc z x3500) x1002
     (Guard_C_Visibilities x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_getPublicFunc x1002) $! (addCs x1001 x3500))
     (Fail_C_Visibilities x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_getPrivateType :: C_Visibilities -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_C_getPrivateType x1 x3500 = case x1 of
     (C_Vis x2 x3 x4) -> d_OP__case_4 x3 x3500
     (Choice_C_Visibilities x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_getPrivateType x1002 x3500) (d_C_getPrivateType x1003 x3500)
     (Choices_C_Visibilities x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_getPrivateType z x3500) x1002
     (Guard_C_Visibilities x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_getPrivateType x1002) $! (addCs x1001 x3500))
     (Fail_C_Visibilities x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_getPublicType :: C_Visibilities -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_C_getPublicType x1 x3500 = case x1 of
     (C_Vis x2 x3 x4) -> d_OP__case_3 x3 x3500
     (Choice_C_Visibilities x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_getPublicType x1002 x3500) (d_C_getPublicType x1003 x3500)
     (Choices_C_Visibilities x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_getPublicType z x3500) x1002
     (Guard_C_Visibilities x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_getPublicType x1002) $! (addCs x1001 x3500))
     (Fail_C_Visibilities x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_getPrivateCons :: C_Visibilities -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_C_getPrivateCons x1 x3500 = case x1 of
     (C_Vis x2 x3 x4) -> d_OP__case_2 x4 x3500
     (Choice_C_Visibilities x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_getPrivateCons x1002 x3500) (d_C_getPrivateCons x1003 x3500)
     (Choices_C_Visibilities x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_getPrivateCons z x3500) x1002
     (Guard_C_Visibilities x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_getPrivateCons x1002) $! (addCs x1001 x3500))
     (Fail_C_Visibilities x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_getPublicCons :: C_Visibilities -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_C_getPublicCons x1 x3500 = case x1 of
     (C_Vis x2 x3 x4) -> d_OP__case_1 x4 x3500
     (Choice_C_Visibilities x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_getPublicCons x1002 x3500) (d_C_getPublicCons x1003 x3500)
     (Choices_C_Visibilities x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_getPublicCons z x3500) x1002
     (Guard_C_Visibilities x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_getPublicCons x1002) $! (addCs x1001 x3500))
     (Fail_C_Visibilities x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_analyzeVisibility :: Curry_FlatCurry.C_Prog -> ConstStore -> C_Visibilities
d_C_analyzeVisibility x1 x3500 = let
     x2 = Curry_Prelude.d_C_apply (Curry_FlatCurryGoodies.d_C_progTypes x3500) x1 x3500
      in (C_Vis (d_C_splitVisibleFuncs (Curry_Prelude.d_C_apply (Curry_FlatCurryGoodies.d_C_progFuncs x3500) x1 x3500) x3500) (d_C_splitVisibleTypes x2 x3500) (d_C_splitVisibleCons (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_concatMap (Curry_FlatCurryGoodies.d_C_typeConsDecls x3500) x3500) (Curry_Prelude.d_C_filter (Curry_Prelude.d_OP_dot Curry_Prelude.d_C_not (Curry_FlatCurryGoodies.d_C_isTypeSyn x3500) x3500) x2 x3500) x3500) x3500))

d_C_splitVisibleFuncs :: Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))
d_C_splitVisibleFuncs x1 x3500 = let
     x2 = Curry_List.d_C_partition d_OP_splitVisibleFuncs_dot___hash_lambda12 x1 x3500
     x3 = d_OP_splitVisibleFuncs_dot___hash_selFP4_hash_pubs x2 x3500
     x4 = d_OP_splitVisibleFuncs_dot___hash_selFP5_hash_privs x2 x3500
      in (Curry_Prelude.OP_Tuple2 (Curry_Prelude.d_C_map (Curry_FlatCurryGoodies.d_C_funcName x3500) x3 x3500) (Curry_Prelude.d_C_map (Curry_FlatCurryGoodies.d_C_funcName x3500) x4 x3500))

d_OP_splitVisibleFuncs_dot___hash_lambda12 :: Curry_FlatCurry.C_FuncDecl -> ConstStore -> Curry_Prelude.C_Bool
d_OP_splitVisibleFuncs_dot___hash_lambda12 x1 x3500 = Curry_Prelude.d_OP_eq_eq (Curry_Prelude.d_C_apply (Curry_FlatCurryGoodies.d_C_funcVisibility x3500) x1 x3500) Curry_FlatCurry.C_Public x3500

d_OP_splitVisibleFuncs_dot___hash_selFP4_hash_pubs :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) -> ConstStore -> Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl
d_OP_splitVisibleFuncs_dot___hash_selFP4_hash_pubs x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_splitVisibleFuncs_dot___hash_selFP4_hash_pubs x1002 x3500) (d_OP_splitVisibleFuncs_dot___hash_selFP4_hash_pubs x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_splitVisibleFuncs_dot___hash_selFP4_hash_pubs z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_splitVisibleFuncs_dot___hash_selFP4_hash_pubs x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_splitVisibleFuncs_dot___hash_selFP5_hash_privs :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) -> ConstStore -> Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl
d_OP_splitVisibleFuncs_dot___hash_selFP5_hash_privs x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_splitVisibleFuncs_dot___hash_selFP5_hash_privs x1002 x3500) (d_OP_splitVisibleFuncs_dot___hash_selFP5_hash_privs x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_splitVisibleFuncs_dot___hash_selFP5_hash_privs z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_splitVisibleFuncs_dot___hash_selFP5_hash_privs x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_splitVisibleTypes :: Curry_Prelude.OP_List Curry_FlatCurry.C_TypeDecl -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))
d_C_splitVisibleTypes x1 x3500 = let
     x2 = Curry_List.d_C_partition d_OP_splitVisibleTypes_dot___hash_lambda13 x1 x3500
     x3 = d_OP_splitVisibleTypes_dot___hash_selFP7_hash_pubs x2 x3500
     x4 = d_OP_splitVisibleTypes_dot___hash_selFP8_hash_privs x2 x3500
      in (Curry_Prelude.OP_Tuple2 (Curry_Prelude.d_C_map (Curry_FlatCurryGoodies.d_C_typeName x3500) x3 x3500) (Curry_Prelude.d_C_map (Curry_FlatCurryGoodies.d_C_typeName x3500) x4 x3500))

d_OP_splitVisibleTypes_dot___hash_lambda13 :: Curry_FlatCurry.C_TypeDecl -> ConstStore -> Curry_Prelude.C_Bool
d_OP_splitVisibleTypes_dot___hash_lambda13 x1 x3500 = Curry_Prelude.d_OP_eq_eq (Curry_Prelude.d_C_apply (Curry_FlatCurryGoodies.d_C_typeVisibility x3500) x1 x3500) Curry_FlatCurry.C_Public x3500

d_OP_splitVisibleTypes_dot___hash_selFP7_hash_pubs :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_FlatCurry.C_TypeDecl) (Curry_Prelude.OP_List Curry_FlatCurry.C_TypeDecl) -> ConstStore -> Curry_Prelude.OP_List Curry_FlatCurry.C_TypeDecl
d_OP_splitVisibleTypes_dot___hash_selFP7_hash_pubs x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_splitVisibleTypes_dot___hash_selFP7_hash_pubs x1002 x3500) (d_OP_splitVisibleTypes_dot___hash_selFP7_hash_pubs x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_splitVisibleTypes_dot___hash_selFP7_hash_pubs z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_splitVisibleTypes_dot___hash_selFP7_hash_pubs x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_splitVisibleTypes_dot___hash_selFP8_hash_privs :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_FlatCurry.C_TypeDecl) (Curry_Prelude.OP_List Curry_FlatCurry.C_TypeDecl) -> ConstStore -> Curry_Prelude.OP_List Curry_FlatCurry.C_TypeDecl
d_OP_splitVisibleTypes_dot___hash_selFP8_hash_privs x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_splitVisibleTypes_dot___hash_selFP8_hash_privs x1002 x3500) (d_OP_splitVisibleTypes_dot___hash_selFP8_hash_privs x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_splitVisibleTypes_dot___hash_selFP8_hash_privs z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_splitVisibleTypes_dot___hash_selFP8_hash_privs x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_splitVisibleCons :: Curry_Prelude.OP_List Curry_FlatCurry.C_ConsDecl -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))
d_C_splitVisibleCons x1 x3500 = let
     x2 = Curry_List.d_C_partition d_OP_splitVisibleCons_dot___hash_lambda14 x1 x3500
     x3 = d_OP_splitVisibleCons_dot___hash_selFP10_hash_pubs x2 x3500
     x4 = d_OP_splitVisibleCons_dot___hash_selFP11_hash_privs x2 x3500
      in (Curry_Prelude.OP_Tuple2 (Curry_Prelude.d_C_map (Curry_FlatCurryGoodies.d_C_consName x3500) x3 x3500) (Curry_Prelude.d_C_map (Curry_FlatCurryGoodies.d_C_consName x3500) x4 x3500))

d_OP_splitVisibleCons_dot___hash_lambda14 :: Curry_FlatCurry.C_ConsDecl -> ConstStore -> Curry_Prelude.C_Bool
d_OP_splitVisibleCons_dot___hash_lambda14 x1 x3500 = Curry_Prelude.d_OP_eq_eq (Curry_Prelude.d_C_apply (Curry_FlatCurryGoodies.d_C_consVisibility x3500) x1 x3500) Curry_FlatCurry.C_Public x3500

d_OP_splitVisibleCons_dot___hash_selFP10_hash_pubs :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_FlatCurry.C_ConsDecl) (Curry_Prelude.OP_List Curry_FlatCurry.C_ConsDecl) -> ConstStore -> Curry_Prelude.OP_List Curry_FlatCurry.C_ConsDecl
d_OP_splitVisibleCons_dot___hash_selFP10_hash_pubs x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_splitVisibleCons_dot___hash_selFP10_hash_pubs x1002 x3500) (d_OP_splitVisibleCons_dot___hash_selFP10_hash_pubs x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_splitVisibleCons_dot___hash_selFP10_hash_pubs z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_splitVisibleCons_dot___hash_selFP10_hash_pubs x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_splitVisibleCons_dot___hash_selFP11_hash_privs :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_FlatCurry.C_ConsDecl) (Curry_Prelude.OP_List Curry_FlatCurry.C_ConsDecl) -> ConstStore -> Curry_Prelude.OP_List Curry_FlatCurry.C_ConsDecl
d_OP_splitVisibleCons_dot___hash_selFP11_hash_privs x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_splitVisibleCons_dot___hash_selFP11_hash_privs x1002 x3500) (d_OP_splitVisibleCons_dot___hash_selFP11_hash_privs x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_splitVisibleCons_dot___hash_selFP11_hash_privs z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_splitVisibleCons_dot___hash_selFP11_hash_privs x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_bar_plus_plus_bar :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t0) (Curry_Prelude.OP_List t1) -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t0) (Curry_Prelude.OP_List t1) -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t0) (Curry_Prelude.OP_List t1)
d_OP_bar_plus_plus_bar x1 x2 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x3 x4) -> d_OP__case_0 x3 x4 x2 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_bar_plus_plus_bar x1002 x2 x3500) (d_OP_bar_plus_plus_bar x1003 x2 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_bar_plus_plus_bar z x2 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_bar_plus_plus_bar x1002 x2) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_0 x3 x4 x2 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x5 x6) -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.d_OP_plus_plus x3 x5 x3500) (Curry_Prelude.d_OP_plus_plus x4 x6 x3500)
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_0 x3 x4 x1002 x3500) (d_OP__case_0 x3 x4 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_0 x3 x4 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_0 x3 x4 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_0 x3 x4 x2 x3000 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x5 x6) -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.d_OP_plus_plus x3 x5 x3500) (Curry_Prelude.d_OP_plus_plus x4 x6 x3500)
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_0 x3 x4 x1002 x3000 x3500) (nd_OP__case_0 x3 x4 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_0 x3 x4 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_0 x3 x4 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_1 x4 x3500 = case x4 of
     (Curry_Prelude.OP_Tuple2 x5 x6) -> x5
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_1 x1002 x3500) (d_OP__case_1 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_1 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_1 x4 x3000 x3500 = case x4 of
     (Curry_Prelude.OP_Tuple2 x5 x6) -> x5
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_1 x1002 x3000 x3500) (nd_OP__case_1 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_1 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_2 x4 x3500 = case x4 of
     (Curry_Prelude.OP_Tuple2 x5 x6) -> x6
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_2 x1002 x3500) (d_OP__case_2 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_2 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_2 x4 x3000 x3500 = case x4 of
     (Curry_Prelude.OP_Tuple2 x5 x6) -> x6
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_2 x1002 x3000 x3500) (nd_OP__case_2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_3 x3 x3500 = case x3 of
     (Curry_Prelude.OP_Tuple2 x5 x6) -> x5
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_3 x1002 x3500) (d_OP__case_3 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_3 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_3 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_3 x3 x3000 x3500 = case x3 of
     (Curry_Prelude.OP_Tuple2 x5 x6) -> x5
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_3 x1002 x3000 x3500) (nd_OP__case_3 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_3 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_3 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_4 x3 x3500 = case x3 of
     (Curry_Prelude.OP_Tuple2 x5 x6) -> x6
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_4 x1002 x3500) (d_OP__case_4 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_4 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_4 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_4 x3 x3000 x3500 = case x3 of
     (Curry_Prelude.OP_Tuple2 x5 x6) -> x6
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_4 x1002 x3000 x3500) (nd_OP__case_4 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_4 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_4 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_5 x2 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x5 x6) -> x5
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_5 x1002 x3500) (d_OP__case_5 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_5 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_5 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_5 x2 x3000 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x5 x6) -> x5
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_5 x1002 x3000 x3500) (nd_OP__case_5 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_5 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_5 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_6 x2 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x5 x6) -> x6
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_6 x1002 x3500) (d_OP__case_6 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_6 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_6 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_6 x2 x3000 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x5 x6) -> x6
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_6 x1002 x3000 x3500) (nd_OP__case_6 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_6 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_6 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_13 x1 x2 x3 x4 x3500 = case x4 of
     Curry_Prelude.C_True -> d_OP__case_12 x3 x1 x3500
     Curry_Prelude.C_False -> d_OP__case_8 x2 x3 x1 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_13 x1 x2 x3 x1002 x3500) (d_OP__case_13 x1 x2 x3 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_13 x1 x2 x3 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_13 x1 x2 x3 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_13 x1 x2 x3 x4 x3000 x3500 = case x4 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_12 x3 x1 x2000 x3500))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_8 x2 x3 x1 x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_13 x1 x2 x3 x1002 x3000 x3500) (nd_OP__case_13 x1 x2 x3 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_13 x1 x2 x3 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_13 x1 x2 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_8 x2 x3 x1 x3500 = case x1 of
     (Curry_FlatCurry.C_TVar x16) -> let
          x17 = x16
           in (d_OP__case_7 x17 (Curry_Prelude.d_OP_eq_eq x17 (Curry_Prelude.C_Int -42#) x3500) x3500)
     (Curry_FlatCurry.C_FuncType x18 x19) -> let
          x20 = d_C_ordHelp2 x18 (Curry_Prelude.C_Int 0#) x3 x3500
          x21 = d_C_ordHelp2 x19 (Curry_Prelude.d_OP_minus x2 (Curry_Prelude.C_Int 1#) x3500) x3 x3500
           in (d_C_hoOr x20 x21 x3500)
     (Curry_FlatCurry.C_TCons x22 x23) -> Curry_Prelude.d_C_failed x3500
     (Curry_FlatCurry.Choice_C_TypeExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_8 x2 x3 x1002 x3500) (d_OP__case_8 x2 x3 x1003 x3500)
     (Curry_FlatCurry.Choices_C_TypeExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_8 x2 x3 z x3500) x1002
     (Curry_FlatCurry.Guard_C_TypeExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_8 x2 x3 x1002) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_TypeExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_8 x2 x3 x1 x3000 x3500 = case x1 of
     (Curry_FlatCurry.C_TVar x16) -> let
          x2000 = x3000
           in (seq x2000 (let
               x17 = x16
                in (nd_OP__case_7 x17 (Curry_Prelude.d_OP_eq_eq x17 (Curry_Prelude.C_Int -42#) x3500) x2000 x3500)))
     (Curry_FlatCurry.C_FuncType x18 x19) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2000 = leftSupply x2002
               x2001 = rightSupply x2002
                in (seq x2000 (seq x2001 (let
                    x20 = nd_C_ordHelp2 x18 (Curry_Prelude.C_Int 0#) x3 x2000 x3500
                    x21 = nd_C_ordHelp2 x19 (Curry_Prelude.d_OP_minus x2 (Curry_Prelude.C_Int 1#) x3500) x3 x2001 x3500
                     in (d_C_hoOr x20 x21 x3500))))))
     (Curry_FlatCurry.C_TCons x22 x23) -> Curry_Prelude.d_C_failed x3500
     (Curry_FlatCurry.Choice_C_TypeExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_8 x2 x3 x1002 x3000 x3500) (nd_OP__case_8 x2 x3 x1003 x3000 x3500)
     (Curry_FlatCurry.Choices_C_TypeExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_8 x2 x3 z x3000 x3500) x1002
     (Curry_FlatCurry.Guard_C_TypeExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_8 x2 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_TypeExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_7 x17 x18 x3500 = case x18 of
     Curry_Prelude.C_True -> Curry_Base.C_HO
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_7 x17 x1002 x3500) (d_OP__case_7 x17 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_7 x17 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_7 x17 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_7 x17 x18 x3000 x3500 = case x18 of
     Curry_Prelude.C_True -> Curry_Base.C_HO
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_7 x17 x1002 x3000 x3500) (nd_OP__case_7 x17 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_7 x17 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_7 x17 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_12 x3 x1 x3500 = case x1 of
     (Curry_FlatCurry.C_FuncType x4 x5) -> Curry_Base.C_HO
     (Curry_FlatCurry.C_TVar x6) -> let
          x7 = x6
           in (d_OP__case_11 x7 (Curry_Prelude.d_OP_eq_eq x7 (Curry_Prelude.C_Int -42#) x3500) x3500)
     (Curry_FlatCurry.C_TCons x8 x9) -> d_OP__case_10 x3 x8 x9 x3500
     (Curry_FlatCurry.Choice_C_TypeExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_12 x3 x1002 x3500) (d_OP__case_12 x3 x1003 x3500)
     (Curry_FlatCurry.Choices_C_TypeExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_12 x3 z x3500) x1002
     (Curry_FlatCurry.Guard_C_TypeExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_12 x3 x1002) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_TypeExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_12 x3 x1 x3000 x3500 = case x1 of
     (Curry_FlatCurry.C_FuncType x4 x5) -> Curry_Base.C_HO
     (Curry_FlatCurry.C_TVar x6) -> let
          x2000 = x3000
           in (seq x2000 (let
               x7 = x6
                in (nd_OP__case_11 x7 (Curry_Prelude.d_OP_eq_eq x7 (Curry_Prelude.C_Int -42#) x3500) x2000 x3500)))
     (Curry_FlatCurry.C_TCons x8 x9) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_10 x3 x8 x9 x2000 x3500))
     (Curry_FlatCurry.Choice_C_TypeExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_12 x3 x1002 x3000 x3500) (nd_OP__case_12 x3 x1003 x3000 x3500)
     (Curry_FlatCurry.Choices_C_TypeExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_12 x3 z x3000 x3500) x1002
     (Curry_FlatCurry.Guard_C_TypeExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_12 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_TypeExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_10 x3 x8 x9 x3500 = case x9 of
     (Curry_Prelude.OP_Cons x10 x11) -> let
          x12 = d_C_ordHelp2 x10 (Curry_Prelude.C_Int 0#) x3 x3500
          x13 = d_C_ordHelp2 (Curry_FlatCurry.C_TCons x8 x11) (Curry_Prelude.C_Int 0#) x3 x3500
           in (d_C_hoOr x12 x13 x3500)
     Curry_Prelude.OP_List -> d_OP__case_9 x3 x8 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_10 x3 x8 x1002 x3500) (d_OP__case_10 x3 x8 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_10 x3 x8 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_10 x3 x8 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_10 x3 x8 x9 x3000 x3500 = case x9 of
     (Curry_Prelude.OP_Cons x10 x11) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2000 = leftSupply x2002
               x2001 = rightSupply x2002
                in (seq x2000 (seq x2001 (let
                    x12 = nd_C_ordHelp2 x10 (Curry_Prelude.C_Int 0#) x3 x2000 x3500
                    x13 = nd_C_ordHelp2 (Curry_FlatCurry.C_TCons x8 x11) (Curry_Prelude.C_Int 0#) x3 x2001 x3500
                     in (d_C_hoOr x12 x13 x3500))))))
     Curry_Prelude.OP_List -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_9 x3 x8 x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_10 x3 x8 x1002 x3000 x3500) (nd_OP__case_10 x3 x8 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_10 x3 x8 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_10 x3 x8 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_9 x3 x8 x3500 = case x8 of
     (Curry_Prelude.OP_Tuple2 x14 x15) -> Curry_Maybe.d_C_fromMaybe Curry_Base.C_FO (Curry_FiniteMap.d_C_lookupFM x3 (Curry_Prelude.OP_Tuple2 x14 x15) x3500) x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_9 x3 x1002 x3500) (d_OP__case_9 x3 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_9 x3 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_9 x3 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_9 x3 x8 x3000 x3500 = case x8 of
     (Curry_Prelude.OP_Tuple2 x14 x15) -> let
          x2000 = x3000
           in (seq x2000 (Curry_Maybe.d_C_fromMaybe Curry_Base.C_FO (Curry_FiniteMap.nd_C_lookupFM x3 (Curry_Prelude.OP_Tuple2 x14 x15) x2000 x3500) x3500))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_9 x3 x1002 x3000 x3500) (nd_OP__case_9 x3 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_9 x3 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_9 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_11 x7 x8 x3500 = case x8 of
     Curry_Prelude.C_True -> Curry_Base.C_HO
     Curry_Prelude.C_False -> Curry_Base.C_FO
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_11 x7 x1002 x3500) (d_OP__case_11 x7 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_11 x7 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_11 x7 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_11 x7 x8 x3000 x3500 = case x8 of
     Curry_Prelude.C_True -> Curry_Base.C_HO
     Curry_Prelude.C_False -> Curry_Base.C_FO
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_11 x7 x1002 x3000 x3500) (nd_OP__case_11 x7 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_11 x7 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_11 x7 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_15 x1 x3 x3500 = case x3 of
     (C_T x5) -> d_OP__case_14 x1 x5 x3500
     (C_F x14) -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.d_C_apply (Curry_FlatCurryGoodies.d_C_funcName x3500) x14 x3500) (d_C_ordHelp2 (Curry_Prelude.d_C_apply (Curry_FlatCurryGoodies.d_C_funcType x3500) x14 x3500) (Curry_Prelude.d_C_apply (Curry_FlatCurryGoodies.d_C_funcArity x3500) x14 x3500) x1 x3500)
     (Choice_C_Declaration x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_15 x1 x1002 x3500) (d_OP__case_15 x1 x1003 x3500)
     (Choices_C_Declaration x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_15 x1 z x3500) x1002
     (Guard_C_Declaration x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_15 x1 x1002) $! (addCs x1001 x3500))
     (Fail_C_Declaration x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_15 x1 x3 x3000 x3500 = case x3 of
     (C_T x5) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_14 x1 x5 x2000 x3500))
     (C_F x14) -> let
          x2012 = x3000
           in (seq x2012 (let
               x2002 = leftSupply x2012
               x2010 = rightSupply x2012
                in (seq x2002 (seq x2010 (Curry_Prelude.OP_Tuple2 (let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_FlatCurryGoodies.nd_C_funcName x2000 x3500) x14 x2001 x3500)))) (let
                    x2009 = leftSupply x2010
                    x2011 = rightSupply x2010
                     in (seq x2009 (seq x2011 (let
                         x2005 = leftSupply x2011
                         x2008 = rightSupply x2011
                          in (seq x2005 (seq x2008 (nd_C_ordHelp2 (let
                              x2004 = leftSupply x2005
                              x2003 = rightSupply x2005
                               in (seq x2004 (seq x2003 (Curry_Prelude.nd_C_apply (Curry_FlatCurryGoodies.nd_C_funcType x2003 x3500) x14 x2004 x3500)))) (let
                              x2007 = leftSupply x2008
                              x2006 = rightSupply x2008
                               in (seq x2007 (seq x2006 (Curry_Prelude.nd_C_apply (Curry_FlatCurryGoodies.nd_C_funcArity x2006 x3500) x14 x2007 x3500)))) x1 x2009 x3500))))))))))))
     (Choice_C_Declaration x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_15 x1 x1002 x3000 x3500) (nd_OP__case_15 x1 x1003 x3000 x3500)
     (Choices_C_Declaration x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_15 x1 z x3000 x3500) x1002
     (Guard_C_Declaration x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_15 x1 x1002 x3000) $! (addCs x1001 x3500))
     (Fail_C_Declaration x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_14 x1 x5 x3500 = case x5 of
     (Curry_FlatCurry.C_Type x6 x7 x8 x9) -> Curry_Prelude.OP_Tuple2 x6 (d_C_goThroughConsList x1 x9 x3500)
     (Curry_FlatCurry.C_TypeSyn x10 x11 x12 x13) -> Curry_Prelude.OP_Tuple2 x10 (d_C_ordHelp1 x1 x13 x3500)
     (Curry_FlatCurry.Choice_C_TypeDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_14 x1 x1002 x3500) (d_OP__case_14 x1 x1003 x3500)
     (Curry_FlatCurry.Choices_C_TypeDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_14 x1 z x3500) x1002
     (Curry_FlatCurry.Guard_C_TypeDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_14 x1 x1002) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_TypeDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_14 x1 x5 x3000 x3500 = case x5 of
     (Curry_FlatCurry.C_Type x6 x7 x8 x9) -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.OP_Tuple2 x6 (nd_C_goThroughConsList x1 x9 x2000 x3500)))
     (Curry_FlatCurry.C_TypeSyn x10 x11 x12 x13) -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.OP_Tuple2 x10 (nd_C_ordHelp1 x1 x13 x2000 x3500)))
     (Curry_FlatCurry.Choice_C_TypeDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_14 x1 x1002 x3000 x3500) (nd_OP__case_14 x1 x1003 x3000 x3500)
     (Curry_FlatCurry.Choices_C_TypeDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_14 x1 z x3000 x3500) x1002
     (Curry_FlatCurry.Guard_C_TypeDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_14 x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_TypeDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_16 x3 x2 x3500 = case x2 of
     (Curry_FlatCurry.C_FuncType x4 x5) -> Curry_Base.C_HO
     (Curry_FlatCurry.C_TCons x6 x7) -> d_OP_consOrder_dot_consOrder'_dot_83 (Curry_Prelude.d_OP_plus_plus x7 x3 x3500) x3500
     (Curry_FlatCurry.C_TVar x8) -> d_OP_consOrder_dot_consOrder'_dot_83 x3 x3500
     (Curry_FlatCurry.Choice_C_TypeExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_16 x3 x1002 x3500) (d_OP__case_16 x3 x1003 x3500)
     (Curry_FlatCurry.Choices_C_TypeExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_16 x3 z x3500) x1002
     (Curry_FlatCurry.Guard_C_TypeExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_16 x3 x1002) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_TypeExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_16 x3 x2 x3000 x3500 = case x2 of
     (Curry_FlatCurry.C_FuncType x4 x5) -> Curry_Base.C_HO
     (Curry_FlatCurry.C_TCons x6 x7) -> d_OP_consOrder_dot_consOrder'_dot_83 (Curry_Prelude.d_OP_plus_plus x7 x3 x3500) x3500
     (Curry_FlatCurry.C_TVar x8) -> d_OP_consOrder_dot_consOrder'_dot_83 x3 x3500
     (Curry_FlatCurry.Choice_C_TypeExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_16 x3 x1002 x3000 x3500) (nd_OP__case_16 x3 x1003 x3000 x3500)
     (Curry_FlatCurry.Choices_C_TypeExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_16 x3 z x3000 x3500) x1002
     (Curry_FlatCurry.Guard_C_TypeExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_16 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_TypeExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_20 x5 x6 x7 x8 x9 x3500 = case x9 of
     Curry_Prelude.C_True -> x8
     Curry_Prelude.C_False -> d_OP__case_19 x5 x6 x7 x8 (Curry_Prelude.d_C_apply (d_C_isNDExpr x3500) (Curry_Prelude.d_C_apply (Curry_FlatCurryGoodies.d_C_ruleBody x3500) x6 x3500) x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_20 x5 x6 x7 x8 x1002 x3500) (d_OP__case_20 x5 x6 x7 x8 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_20 x5 x6 x7 x8 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_20 x5 x6 x7 x8 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_20 x5 x6 x7 x8 x9 x3000 x3500 = case x9 of
     Curry_Prelude.C_True -> x8
     Curry_Prelude.C_False -> let
          x2008 = x3000
           in (seq x2008 (let
               x2007 = leftSupply x2008
               x2005 = rightSupply x2008
                in (seq x2007 (seq x2005 (nd_OP__case_19 x5 x6 x7 x8 (let
                    x2004 = leftSupply x2005
                    x2006 = rightSupply x2005
                     in (seq x2004 (seq x2006 (let
                         x2000 = leftSupply x2006
                         x2003 = rightSupply x2006
                          in (seq x2000 (seq x2003 (Curry_Prelude.nd_C_apply (nd_C_isNDExpr x2000 x3500) (let
                              x2002 = leftSupply x2003
                              x2001 = rightSupply x2003
                               in (seq x2002 (seq x2001 (Curry_Prelude.nd_C_apply (Curry_FlatCurryGoodies.nd_C_ruleBody x2001 x3500) x6 x2002 x3500)))) x2004 x3500))))))) x2007 x3500)))))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_20 x5 x6 x7 x8 x1002 x3000 x3500) (nd_OP__case_20 x5 x6 x7 x8 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_20 x5 x6 x7 x8 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_20 x5 x6 x7 x8 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_19 x5 x6 x7 x8 x9 x3500 = case x9 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Tuple2 x5 Curry_Base.C_ND
     Curry_Prelude.C_False -> d_OP__case_18 x5 x8 x7 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_19 x5 x6 x7 x8 x1002 x3500) (d_OP__case_19 x5 x6 x7 x8 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_19 x5 x6 x7 x8 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_19 x5 x6 x7 x8 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_19 x5 x6 x7 x8 x9 x3000 x3500 = case x9 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Tuple2 x5 Curry_Base.C_ND
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_18 x5 x8 x7 x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_19 x5 x6 x7 x8 x1002 x3000 x3500) (nd_OP__case_19 x5 x6 x7 x8 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_19 x5 x6 x7 x8 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_19 x5 x6 x7 x8 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_18 x5 x8 x7 x3500 = case x7 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Tuple2 x5 Curry_Base.C_ND
     Curry_Prelude.C_False -> d_OP__case_17 x8 (Curry_Prelude.d_C_otherwise x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_18 x5 x8 x1002 x3500) (d_OP__case_18 x5 x8 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_18 x5 x8 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_18 x5 x8 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_18 x5 x8 x7 x3000 x3500 = case x7 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Tuple2 x5 Curry_Base.C_ND
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_17 x8 (Curry_Prelude.d_C_otherwise x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_18 x5 x8 x1002 x3000 x3500) (nd_OP__case_18 x5 x8 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_18 x5 x8 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_18 x5 x8 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_17 x8 x9 x3500 = case x9 of
     Curry_Prelude.C_True -> x8
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_17 x8 x1002 x3500) (d_OP__case_17 x8 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_17 x8 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_17 x8 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_17 x8 x9 x3000 x3500 = case x9 of
     Curry_Prelude.C_True -> x8
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_17 x8 x1002 x3000 x3500) (nd_OP__case_17 x8 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_17 x8 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_17 x8 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_21 x2 x3 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Base.C_ND
     Curry_Prelude.C_False -> Curry_Base.C_D
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_21 x2 x1002 x3500) (d_OP__case_21 x2 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_21 x2 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_21 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_21 x2 x3 x3000 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Base.C_ND
     Curry_Prelude.C_False -> Curry_Base.C_D
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_21 x2 x1002 x3000 x3500) (nd_OP__case_21 x2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_21 x2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_21 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_22 x1 x2 x3 x4 x5 x6 x3500 = case x6 of
     Curry_Prelude.C_True -> x4
     Curry_Prelude.C_False -> d_C_fullIteration x1 x2 x3 x5 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_22 x1 x2 x3 x4 x5 x1002 x3500) (d_OP__case_22 x1 x2 x3 x4 x5 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_22 x1 x2 x3 x4 x5 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_22 x1 x2 x3 x4 x5 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_22 x1 x2 x3 x4 x5 x6 x3000 x3500 = case x6 of
     Curry_Prelude.C_True -> x4
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_C_fullIteration x1 x2 x3 x5 x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_22 x1 x2 x3 x4 x5 x1002 x3000 x3500) (nd_OP__case_22 x1 x2 x3 x4 x5 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_22 x1 x2 x3 x4 x5 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_22 x1 x2 x3 x4 x5 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo
