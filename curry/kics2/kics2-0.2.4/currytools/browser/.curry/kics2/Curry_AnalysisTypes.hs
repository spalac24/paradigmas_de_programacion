{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Curry_AnalysisTypes (C_FunctionAnalysis (..), C_AnalysisResult (..), C_ModuleAnalysis (..), C_ModuleAnalysisResult (..), C_ContentsKind (..)) where

import Basics
import qualified Curry_FlatCurry
import qualified Curry_Prelude
data C_FunctionAnalysis t0
     = C_LocalAnalysis (Curry_FlatCurry.C_FuncDecl -> ConstStore -> t0)
     | HO_C_LocalAnalysis (Func Curry_FlatCurry.C_FuncDecl t0)
     | C_LocalDataAnalysis (Curry_Prelude.OP_List Curry_FlatCurry.C_TypeDecl -> ConstStore -> Curry_FlatCurry.C_FuncDecl -> ConstStore -> t0)
     | HO_C_LocalDataAnalysis (Func (Curry_Prelude.OP_List Curry_FlatCurry.C_TypeDecl) (Func Curry_FlatCurry.C_FuncDecl t0))
     | C_GlobalAnalysis (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) t0))
     | HO_C_GlobalAnalysis (Func (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) t0)))
     | C_GlobalDataAnalysis (Curry_Prelude.OP_List Curry_FlatCurry.C_TypeDecl -> ConstStore -> Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) t0))
     | HO_C_GlobalDataAnalysis (Func (Curry_Prelude.OP_List Curry_FlatCurry.C_TypeDecl) (Func (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) t0))))
     | Choice_C_FunctionAnalysis Cover ID (C_FunctionAnalysis t0) (C_FunctionAnalysis t0)
     | Choices_C_FunctionAnalysis Cover ID ([C_FunctionAnalysis t0])
     | Fail_C_FunctionAnalysis Cover FailInfo
     | Guard_C_FunctionAnalysis Cover Constraints (C_FunctionAnalysis t0)

instance Show t0 => Show (C_FunctionAnalysis t0) where
  showsPrec d (Choice_C_FunctionAnalysis cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_FunctionAnalysis cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_FunctionAnalysis cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_FunctionAnalysis cd info) = showChar '!'
  showsPrec _ (C_LocalAnalysis x1) = (showString "(LocalAnalysis") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (HO_C_LocalAnalysis x1) = (showString "(LocalAnalysis") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (C_LocalDataAnalysis x1) = (showString "(LocalDataAnalysis") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (HO_C_LocalDataAnalysis x1) = (showString "(LocalDataAnalysis") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (C_GlobalAnalysis x1) = (showString "(GlobalAnalysis") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (HO_C_GlobalAnalysis x1) = (showString "(GlobalAnalysis") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (C_GlobalDataAnalysis x1) = (showString "(GlobalDataAnalysis") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (HO_C_GlobalDataAnalysis x1) = (showString "(GlobalDataAnalysis") . ((showChar ' ') . ((shows x1) . (showChar ')')))


instance Read t0 => Read (C_FunctionAnalysis t0) where
  readsPrec d s = (readParen (d > 10) (\r -> [ (C_LocalAnalysis x1,r1) | (_,r0) <- readQualified "AnalysisTypes" "LocalAnalysis" r, (x1,r1) <- readsPrec 11 r0]) s) ++ ((readParen (d > 10) (\r -> [ (C_LocalDataAnalysis x1,r1) | (_,r0) <- readQualified "AnalysisTypes" "LocalDataAnalysis" r, (x1,r1) <- readsPrec 11 r0]) s) ++ ((readParen (d > 10) (\r -> [ (C_GlobalAnalysis x1,r1) | (_,r0) <- readQualified "AnalysisTypes" "GlobalAnalysis" r, (x1,r1) <- readsPrec 11 r0]) s) ++ (readParen (d > 10) (\r -> [ (C_GlobalDataAnalysis x1,r1) | (_,r0) <- readQualified "AnalysisTypes" "GlobalDataAnalysis" r, (x1,r1) <- readsPrec 11 r0]) s)))


instance NonDet (C_FunctionAnalysis t0) where
  choiceCons = Choice_C_FunctionAnalysis
  choicesCons = Choices_C_FunctionAnalysis
  failCons = Fail_C_FunctionAnalysis
  guardCons = Guard_C_FunctionAnalysis
  try (Choice_C_FunctionAnalysis cd i x y) = tryChoice cd i x y
  try (Choices_C_FunctionAnalysis cd i xs) = tryChoices cd i xs
  try (Fail_C_FunctionAnalysis cd info) = Fail cd info
  try (Guard_C_FunctionAnalysis cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_FunctionAnalysis cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_FunctionAnalysis cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_FunctionAnalysis cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_FunctionAnalysis cd i _) = error ("AnalysisTypes.FunctionAnalysis.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_FunctionAnalysis cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_FunctionAnalysis cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable t0 => Generable (C_FunctionAnalysis t0) where
  generate s = Choices_C_FunctionAnalysis defCover (freeID [1,1,1,1] s) [(C_LocalAnalysis (generate (leftSupply s))),(C_LocalDataAnalysis (generate (leftSupply s))),(C_GlobalAnalysis (generate (leftSupply s))),(C_GlobalDataAnalysis (generate (leftSupply s)))]


instance NormalForm t0 => NormalForm (C_FunctionAnalysis t0) where
  ($!!) cont (C_LocalAnalysis x1) cs = ((\y1 cs -> cont (C_LocalAnalysis y1) cs) $!! x1) cs
  ($!!) cont (HO_C_LocalAnalysis x1) cs = ((\y1 cs -> cont (HO_C_LocalAnalysis y1) cs) $!! x1) cs
  ($!!) cont (C_LocalDataAnalysis x1) cs = ((\y1 cs -> cont (C_LocalDataAnalysis y1) cs) $!! x1) cs
  ($!!) cont (HO_C_LocalDataAnalysis x1) cs = ((\y1 cs -> cont (HO_C_LocalDataAnalysis y1) cs) $!! x1) cs
  ($!!) cont (C_GlobalAnalysis x1) cs = ((\y1 cs -> cont (C_GlobalAnalysis y1) cs) $!! x1) cs
  ($!!) cont (HO_C_GlobalAnalysis x1) cs = ((\y1 cs -> cont (HO_C_GlobalAnalysis y1) cs) $!! x1) cs
  ($!!) cont (C_GlobalDataAnalysis x1) cs = ((\y1 cs -> cont (C_GlobalDataAnalysis y1) cs) $!! x1) cs
  ($!!) cont (HO_C_GlobalDataAnalysis x1) cs = ((\y1 cs -> cont (HO_C_GlobalDataAnalysis y1) cs) $!! x1) cs
  ($!!) cont (Choice_C_FunctionAnalysis cd i x y) cs = nfChoice cont cd i x y cs
  ($!!) cont (Choices_C_FunctionAnalysis cd i xs) cs = nfChoices cont cd i xs cs
  ($!!) cont (Guard_C_FunctionAnalysis cd c e) cs = guardCons cd c ((cont $!! e) (addCs c cs))
  ($!!) _ (Fail_C_FunctionAnalysis cd info) _ = failCons cd info
  ($##) cont (C_LocalAnalysis x1) cs = ((\y1 cs -> cont (C_LocalAnalysis y1) cs) $## x1) cs
  ($##) cont (HO_C_LocalAnalysis x1) cs = ((\y1 cs -> cont (HO_C_LocalAnalysis y1) cs) $## x1) cs
  ($##) cont (C_LocalDataAnalysis x1) cs = ((\y1 cs -> cont (C_LocalDataAnalysis y1) cs) $## x1) cs
  ($##) cont (HO_C_LocalDataAnalysis x1) cs = ((\y1 cs -> cont (HO_C_LocalDataAnalysis y1) cs) $## x1) cs
  ($##) cont (C_GlobalAnalysis x1) cs = ((\y1 cs -> cont (C_GlobalAnalysis y1) cs) $## x1) cs
  ($##) cont (HO_C_GlobalAnalysis x1) cs = ((\y1 cs -> cont (HO_C_GlobalAnalysis y1) cs) $## x1) cs
  ($##) cont (C_GlobalDataAnalysis x1) cs = ((\y1 cs -> cont (C_GlobalDataAnalysis y1) cs) $## x1) cs
  ($##) cont (HO_C_GlobalDataAnalysis x1) cs = ((\y1 cs -> cont (HO_C_GlobalDataAnalysis y1) cs) $## x1) cs
  ($##) cont (Choice_C_FunctionAnalysis cd i x y) cs = gnfChoice cont cd i x y cs
  ($##) cont (Choices_C_FunctionAnalysis cd i xs) cs = gnfChoices cont cd i xs cs
  ($##) cont (Guard_C_FunctionAnalysis cd c e) cs = guardCons cd c ((cont $## e) (addCs c cs))
  ($##) _ (Fail_C_FunctionAnalysis cd info) _ = failCons cd info
  searchNF search cont (C_LocalAnalysis x1) = search (\y1 -> cont (C_LocalAnalysis y1)) x1
  searchNF search cont (HO_C_LocalAnalysis x1) = search (\y1 -> cont (HO_C_LocalAnalysis y1)) x1
  searchNF search cont (C_LocalDataAnalysis x1) = search (\y1 -> cont (C_LocalDataAnalysis y1)) x1
  searchNF search cont (HO_C_LocalDataAnalysis x1) = search (\y1 -> cont (HO_C_LocalDataAnalysis y1)) x1
  searchNF search cont (C_GlobalAnalysis x1) = search (\y1 -> cont (C_GlobalAnalysis y1)) x1
  searchNF search cont (HO_C_GlobalAnalysis x1) = search (\y1 -> cont (HO_C_GlobalAnalysis y1)) x1
  searchNF search cont (C_GlobalDataAnalysis x1) = search (\y1 -> cont (C_GlobalDataAnalysis y1)) x1
  searchNF search cont (HO_C_GlobalDataAnalysis x1) = search (\y1 -> cont (HO_C_GlobalDataAnalysis y1)) x1
  searchNF _ _ x = error ("AnalysisTypes.FunctionAnalysis.searchNF: no constructor: " ++ (show x))


instance Unifiable t0 => Unifiable (C_FunctionAnalysis t0) where
  (=.=) (C_LocalAnalysis x1) (C_LocalAnalysis y1) cs = (x1 =:= y1) cs
  (=.=) (HO_C_LocalAnalysis x1) (HO_C_LocalAnalysis y1) cs = (x1 =:= y1) cs
  (=.=) (C_LocalDataAnalysis x1) (C_LocalDataAnalysis y1) cs = (x1 =:= y1) cs
  (=.=) (HO_C_LocalDataAnalysis x1) (HO_C_LocalDataAnalysis y1) cs = (x1 =:= y1) cs
  (=.=) (C_GlobalAnalysis x1) (C_GlobalAnalysis y1) cs = (x1 =:= y1) cs
  (=.=) (HO_C_GlobalAnalysis x1) (HO_C_GlobalAnalysis y1) cs = (x1 =:= y1) cs
  (=.=) (C_GlobalDataAnalysis x1) (C_GlobalDataAnalysis y1) cs = (x1 =:= y1) cs
  (=.=) (HO_C_GlobalDataAnalysis x1) (HO_C_GlobalDataAnalysis y1) cs = (x1 =:= y1) cs
  (=.=) _ _ _ = Fail_C_Success defCover defFailInfo
  (=.<=) (C_LocalAnalysis x1) (C_LocalAnalysis y1) cs = (x1 =:<= y1) cs
  (=.<=) (HO_C_LocalAnalysis x1) (HO_C_LocalAnalysis y1) cs = (x1 =:<= y1) cs
  (=.<=) (C_LocalDataAnalysis x1) (C_LocalDataAnalysis y1) cs = (x1 =:<= y1) cs
  (=.<=) (HO_C_LocalDataAnalysis x1) (HO_C_LocalDataAnalysis y1) cs = (x1 =:<= y1) cs
  (=.<=) (C_GlobalAnalysis x1) (C_GlobalAnalysis y1) cs = (x1 =:<= y1) cs
  (=.<=) (HO_C_GlobalAnalysis x1) (HO_C_GlobalAnalysis y1) cs = (x1 =:<= y1) cs
  (=.<=) (C_GlobalDataAnalysis x1) (C_GlobalDataAnalysis y1) cs = (x1 =:<= y1) cs
  (=.<=) (HO_C_GlobalDataAnalysis x1) (HO_C_GlobalDataAnalysis y1) cs = (x1 =:<= y1) cs
  (=.<=) _ _ _ = Fail_C_Success defCover defFailInfo
  bind i (C_LocalAnalysis x2) = ((i :=: (ChooseN 0 1)):(concat [(bind (leftID i) x2)]))
  bind i (HO_C_LocalAnalysis x2) = ((i :=: (ChooseN 0 1)):(concat [(bind (leftID i) x2)]))
  bind i (C_LocalDataAnalysis x2) = ((i :=: (ChooseN 1 1)):(concat [(bind (leftID i) x2)]))
  bind i (HO_C_LocalDataAnalysis x2) = ((i :=: (ChooseN 1 1)):(concat [(bind (leftID i) x2)]))
  bind i (C_GlobalAnalysis x2) = ((i :=: (ChooseN 2 1)):(concat [(bind (leftID i) x2)]))
  bind i (HO_C_GlobalAnalysis x2) = ((i :=: (ChooseN 2 1)):(concat [(bind (leftID i) x2)]))
  bind i (C_GlobalDataAnalysis x2) = ((i :=: (ChooseN 3 1)):(concat [(bind (leftID i) x2)]))
  bind i (HO_C_GlobalDataAnalysis x2) = ((i :=: (ChooseN 3 1)):(concat [(bind (leftID i) x2)]))
  bind i (Choice_C_FunctionAnalysis cd j x y) = [(ConstraintChoice cd j (bind i x) (bind i y))]
  bind i (Choices_C_FunctionAnalysis cd j@(FreeID _ _) xs) = bindOrNarrow i cd j xs
  bind i (Choices_C_FunctionAnalysis cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind i) xs))]
  bind _ (Choices_C_FunctionAnalysis cd i _) = error ("AnalysisTypes.FunctionAnalysis.bind: Choices with ChoiceID: " ++ (show i))
  bind _ (Fail_C_FunctionAnalysis cd info) = [(Unsolvable info)]
  bind i (Guard_C_FunctionAnalysis cd c e) = (getConstrList c) ++ (bind i e)
  lazyBind i (C_LocalAnalysis x2) = [(i :=: (ChooseN 0 1)),((leftID i) :=: (LazyBind (lazyBind (leftID i) x2)))]
  lazyBind i (HO_C_LocalAnalysis x2) = [(i :=: (ChooseN 0 1)),((leftID i) :=: (LazyBind (lazyBind (leftID i) x2)))]
  lazyBind i (C_LocalDataAnalysis x2) = [(i :=: (ChooseN 1 1)),((leftID i) :=: (LazyBind (lazyBind (leftID i) x2)))]
  lazyBind i (HO_C_LocalDataAnalysis x2) = [(i :=: (ChooseN 1 1)),((leftID i) :=: (LazyBind (lazyBind (leftID i) x2)))]
  lazyBind i (C_GlobalAnalysis x2) = [(i :=: (ChooseN 2 1)),((leftID i) :=: (LazyBind (lazyBind (leftID i) x2)))]
  lazyBind i (HO_C_GlobalAnalysis x2) = [(i :=: (ChooseN 2 1)),((leftID i) :=: (LazyBind (lazyBind (leftID i) x2)))]
  lazyBind i (C_GlobalDataAnalysis x2) = [(i :=: (ChooseN 3 1)),((leftID i) :=: (LazyBind (lazyBind (leftID i) x2)))]
  lazyBind i (HO_C_GlobalDataAnalysis x2) = [(i :=: (ChooseN 3 1)),((leftID i) :=: (LazyBind (lazyBind (leftID i) x2)))]
  lazyBind i (Choice_C_FunctionAnalysis cd j x y) = [(ConstraintChoice cd j (lazyBind i x) (lazyBind i y))]
  lazyBind i (Choices_C_FunctionAnalysis cd j@(FreeID _ _) xs) = lazyBindOrNarrow i cd j xs
  lazyBind i (Choices_C_FunctionAnalysis cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind i) xs))]
  lazyBind _ (Choices_C_FunctionAnalysis cd i _) = error ("AnalysisTypes.FunctionAnalysis.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ (Fail_C_FunctionAnalysis cd info) = [(Unsolvable info)]
  lazyBind i (Guard_C_FunctionAnalysis cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind i e)))]


instance Curry_Prelude.Curry t0 => Curry_Prelude.Curry (C_FunctionAnalysis t0) where
  (=?=) (Choice_C_FunctionAnalysis cd i x y) z cs = narrow cd i ((x Curry_Prelude.=?= z) cs) ((y Curry_Prelude.=?= z) cs)
  (=?=) (Choices_C_FunctionAnalysis cd i xs) y cs = narrows cs cd i (\x -> (x Curry_Prelude.=?= y) cs) xs
  (=?=) (Guard_C_FunctionAnalysis cd c e) y cs = guardCons cd c ((e Curry_Prelude.=?= y) (addCs c cs))
  (=?=) (Fail_C_FunctionAnalysis cd info) _ _ = failCons cd info
  (=?=) z (Choice_C_FunctionAnalysis cd i x y) cs = narrow cd i ((z Curry_Prelude.=?= x) cs) ((z Curry_Prelude.=?= y) cs)
  (=?=) y (Choices_C_FunctionAnalysis cd i xs) cs = narrows cs cd i (\x -> (y Curry_Prelude.=?= x) cs) xs
  (=?=) y (Guard_C_FunctionAnalysis cd c e) cs = guardCons cd c ((y Curry_Prelude.=?= e) (addCs c cs))
  (=?=) _ (Fail_C_FunctionAnalysis cd info) _ = failCons cd info
  (=?=) (C_LocalAnalysis x1) (C_LocalAnalysis y1) cs = (x1 Curry_Prelude.=?= y1) cs
  (=?=) (HO_C_LocalAnalysis x1) (HO_C_LocalAnalysis y1) cs = (x1 Curry_Prelude.=?= y1) cs
  (=?=) (C_LocalDataAnalysis x1) (C_LocalDataAnalysis y1) cs = (x1 Curry_Prelude.=?= y1) cs
  (=?=) (HO_C_LocalDataAnalysis x1) (HO_C_LocalDataAnalysis y1) cs = (x1 Curry_Prelude.=?= y1) cs
  (=?=) (C_GlobalAnalysis x1) (C_GlobalAnalysis y1) cs = (x1 Curry_Prelude.=?= y1) cs
  (=?=) (HO_C_GlobalAnalysis x1) (HO_C_GlobalAnalysis y1) cs = (x1 Curry_Prelude.=?= y1) cs
  (=?=) (C_GlobalDataAnalysis x1) (C_GlobalDataAnalysis y1) cs = (x1 Curry_Prelude.=?= y1) cs
  (=?=) (HO_C_GlobalDataAnalysis x1) (HO_C_GlobalDataAnalysis y1) cs = (x1 Curry_Prelude.=?= y1) cs
  (=?=) _ _ _ = Curry_Prelude.C_False
  (<?=) (Choice_C_FunctionAnalysis cd i x y) z cs = narrow cd i ((x Curry_Prelude.<?= z) cs) ((y Curry_Prelude.<?= z) cs)
  (<?=) (Choices_C_FunctionAnalysis cd i xs) y cs = narrows cs cd i (\x -> (x Curry_Prelude.<?= y) cs) xs
  (<?=) (Guard_C_FunctionAnalysis cd c e) y cs = guardCons cd c ((e Curry_Prelude.<?= y) (addCs c cs))
  (<?=) (Fail_C_FunctionAnalysis cd info) _ _ = failCons cd info
  (<?=) z (Choice_C_FunctionAnalysis cd i x y) cs = narrow cd i ((z Curry_Prelude.<?= x) cs) ((z Curry_Prelude.<?= y) cs)
  (<?=) y (Choices_C_FunctionAnalysis cd i xs) cs = narrows cs cd i (\x -> (y Curry_Prelude.<?= x) cs) xs
  (<?=) y (Guard_C_FunctionAnalysis cd c e) cs = guardCons cd c ((y Curry_Prelude.<?= e) (addCs c cs))
  (<?=) _ (Fail_C_FunctionAnalysis cd info) _ = failCons cd info
  (<?=) (C_LocalAnalysis x1) (C_LocalAnalysis y1) cs = (x1 Curry_Prelude.<?= y1) cs
  (<?=) (C_LocalAnalysis _) (C_LocalDataAnalysis _) _ = Curry_Prelude.C_True
  (<?=) (C_LocalAnalysis _) (HO_C_LocalDataAnalysis _) _ = Curry_Prelude.C_True
  (<?=) (C_LocalAnalysis _) (C_GlobalAnalysis _) _ = Curry_Prelude.C_True
  (<?=) (C_LocalAnalysis _) (HO_C_GlobalAnalysis _) _ = Curry_Prelude.C_True
  (<?=) (C_LocalAnalysis _) (C_GlobalDataAnalysis _) _ = Curry_Prelude.C_True
  (<?=) (C_LocalAnalysis _) (HO_C_GlobalDataAnalysis _) _ = Curry_Prelude.C_True
  (<?=) (HO_C_LocalAnalysis x1) (HO_C_LocalAnalysis y1) cs = (x1 Curry_Prelude.<?= y1) cs
  (<?=) (HO_C_LocalAnalysis _) (C_LocalDataAnalysis _) _ = Curry_Prelude.C_True
  (<?=) (HO_C_LocalAnalysis _) (HO_C_LocalDataAnalysis _) _ = Curry_Prelude.C_True
  (<?=) (HO_C_LocalAnalysis _) (C_GlobalAnalysis _) _ = Curry_Prelude.C_True
  (<?=) (HO_C_LocalAnalysis _) (HO_C_GlobalAnalysis _) _ = Curry_Prelude.C_True
  (<?=) (HO_C_LocalAnalysis _) (C_GlobalDataAnalysis _) _ = Curry_Prelude.C_True
  (<?=) (HO_C_LocalAnalysis _) (HO_C_GlobalDataAnalysis _) _ = Curry_Prelude.C_True
  (<?=) (C_LocalDataAnalysis x1) (C_LocalDataAnalysis y1) cs = (x1 Curry_Prelude.<?= y1) cs
  (<?=) (C_LocalDataAnalysis _) (C_GlobalAnalysis _) _ = Curry_Prelude.C_True
  (<?=) (C_LocalDataAnalysis _) (HO_C_GlobalAnalysis _) _ = Curry_Prelude.C_True
  (<?=) (C_LocalDataAnalysis _) (C_GlobalDataAnalysis _) _ = Curry_Prelude.C_True
  (<?=) (C_LocalDataAnalysis _) (HO_C_GlobalDataAnalysis _) _ = Curry_Prelude.C_True
  (<?=) (HO_C_LocalDataAnalysis x1) (HO_C_LocalDataAnalysis y1) cs = (x1 Curry_Prelude.<?= y1) cs
  (<?=) (HO_C_LocalDataAnalysis _) (C_GlobalAnalysis _) _ = Curry_Prelude.C_True
  (<?=) (HO_C_LocalDataAnalysis _) (HO_C_GlobalAnalysis _) _ = Curry_Prelude.C_True
  (<?=) (HO_C_LocalDataAnalysis _) (C_GlobalDataAnalysis _) _ = Curry_Prelude.C_True
  (<?=) (HO_C_LocalDataAnalysis _) (HO_C_GlobalDataAnalysis _) _ = Curry_Prelude.C_True
  (<?=) (C_GlobalAnalysis x1) (C_GlobalAnalysis y1) cs = (x1 Curry_Prelude.<?= y1) cs
  (<?=) (C_GlobalAnalysis _) (C_GlobalDataAnalysis _) _ = Curry_Prelude.C_True
  (<?=) (C_GlobalAnalysis _) (HO_C_GlobalDataAnalysis _) _ = Curry_Prelude.C_True
  (<?=) (HO_C_GlobalAnalysis x1) (HO_C_GlobalAnalysis y1) cs = (x1 Curry_Prelude.<?= y1) cs
  (<?=) (HO_C_GlobalAnalysis _) (C_GlobalDataAnalysis _) _ = Curry_Prelude.C_True
  (<?=) (HO_C_GlobalAnalysis _) (HO_C_GlobalDataAnalysis _) _ = Curry_Prelude.C_True
  (<?=) (C_GlobalDataAnalysis x1) (C_GlobalDataAnalysis y1) cs = (x1 Curry_Prelude.<?= y1) cs
  (<?=) (HO_C_GlobalDataAnalysis x1) (HO_C_GlobalDataAnalysis y1) cs = (x1 Curry_Prelude.<?= y1) cs
  (<?=) _ _ _ = Curry_Prelude.C_False


instance Coverable t0 => Coverable (C_FunctionAnalysis t0) where
  cover (C_LocalAnalysis x1) = C_LocalAnalysis (cover x1)
  cover (HO_C_LocalAnalysis x1) = HO_C_LocalAnalysis (cover x1)
  cover (C_LocalDataAnalysis x1) = C_LocalDataAnalysis (cover x1)
  cover (HO_C_LocalDataAnalysis x1) = HO_C_LocalDataAnalysis (cover x1)
  cover (C_GlobalAnalysis x1) = C_GlobalAnalysis (cover x1)
  cover (HO_C_GlobalAnalysis x1) = HO_C_GlobalAnalysis (cover x1)
  cover (C_GlobalDataAnalysis x1) = C_GlobalDataAnalysis (cover x1)
  cover (HO_C_GlobalDataAnalysis x1) = HO_C_GlobalDataAnalysis (cover x1)
  cover (Choice_C_FunctionAnalysis cd i x y) = Choice_C_FunctionAnalysis (incCover cd) i (cover x) (cover y)
  cover (Choices_C_FunctionAnalysis cd i xs) = Choices_C_FunctionAnalysis (incCover cd) i (map cover xs)
  cover (Fail_C_FunctionAnalysis cd info) = Fail_C_FunctionAnalysis (incCover cd) info
  cover (Guard_C_FunctionAnalysis cd c e) = Guard_C_FunctionAnalysis (incCover cd) c (cover e)


data C_AnalysisResult
     = C_MsgResult (Curry_Prelude.OP_List Curry_Prelude.C_Char)
     | C_ActionResult (Curry_Prelude.C_IO Curry_Prelude.OP_Unit)
     | Choice_C_AnalysisResult Cover ID C_AnalysisResult C_AnalysisResult
     | Choices_C_AnalysisResult Cover ID ([C_AnalysisResult])
     | Fail_C_AnalysisResult Cover FailInfo
     | Guard_C_AnalysisResult Cover Constraints C_AnalysisResult

instance Show C_AnalysisResult where
  showsPrec d (Choice_C_AnalysisResult cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_AnalysisResult cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_AnalysisResult cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_AnalysisResult cd info) = showChar '!'
  showsPrec _ (C_MsgResult x1) = (showString "(MsgResult") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (C_ActionResult x1) = (showString "(ActionResult") . ((showChar ' ') . ((shows x1) . (showChar ')')))


instance Read C_AnalysisResult where
  readsPrec d s = (readParen (d > 10) (\r -> [ (C_MsgResult x1,r1) | (_,r0) <- readQualified "AnalysisTypes" "MsgResult" r, (x1,r1) <- readsPrec 11 r0]) s) ++ (readParen (d > 10) (\r -> [ (C_ActionResult x1,r1) | (_,r0) <- readQualified "AnalysisTypes" "ActionResult" r, (x1,r1) <- readsPrec 11 r0]) s)


instance NonDet C_AnalysisResult where
  choiceCons = Choice_C_AnalysisResult
  choicesCons = Choices_C_AnalysisResult
  failCons = Fail_C_AnalysisResult
  guardCons = Guard_C_AnalysisResult
  try (Choice_C_AnalysisResult cd i x y) = tryChoice cd i x y
  try (Choices_C_AnalysisResult cd i xs) = tryChoices cd i xs
  try (Fail_C_AnalysisResult cd info) = Fail cd info
  try (Guard_C_AnalysisResult cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_AnalysisResult cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_AnalysisResult cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_AnalysisResult cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_AnalysisResult cd i _) = error ("AnalysisTypes.AnalysisResult.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_AnalysisResult cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_AnalysisResult cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_AnalysisResult where
  generate s = Choices_C_AnalysisResult defCover (freeID [1,1] s) [(C_MsgResult (generate (leftSupply s))),(C_ActionResult (generate (leftSupply s)))]


instance NormalForm C_AnalysisResult where
  ($!!) cont (C_MsgResult x1) cs = ((\y1 cs -> cont (C_MsgResult y1) cs) $!! x1) cs
  ($!!) cont (C_ActionResult x1) cs = ((\y1 cs -> cont (C_ActionResult y1) cs) $!! x1) cs
  ($!!) cont (Choice_C_AnalysisResult cd i x y) cs = nfChoice cont cd i x y cs
  ($!!) cont (Choices_C_AnalysisResult cd i xs) cs = nfChoices cont cd i xs cs
  ($!!) cont (Guard_C_AnalysisResult cd c e) cs = guardCons cd c ((cont $!! e) (addCs c cs))
  ($!!) _ (Fail_C_AnalysisResult cd info) _ = failCons cd info
  ($##) cont (C_MsgResult x1) cs = ((\y1 cs -> cont (C_MsgResult y1) cs) $## x1) cs
  ($##) cont (C_ActionResult x1) cs = ((\y1 cs -> cont (C_ActionResult y1) cs) $## x1) cs
  ($##) cont (Choice_C_AnalysisResult cd i x y) cs = gnfChoice cont cd i x y cs
  ($##) cont (Choices_C_AnalysisResult cd i xs) cs = gnfChoices cont cd i xs cs
  ($##) cont (Guard_C_AnalysisResult cd c e) cs = guardCons cd c ((cont $## e) (addCs c cs))
  ($##) _ (Fail_C_AnalysisResult cd info) _ = failCons cd info
  searchNF search cont (C_MsgResult x1) = search (\y1 -> cont (C_MsgResult y1)) x1
  searchNF search cont (C_ActionResult x1) = search (\y1 -> cont (C_ActionResult y1)) x1
  searchNF _ _ x = error ("AnalysisTypes.AnalysisResult.searchNF: no constructor: " ++ (show x))


instance Unifiable C_AnalysisResult where
  (=.=) (C_MsgResult x1) (C_MsgResult y1) cs = (x1 =:= y1) cs
  (=.=) (C_ActionResult x1) (C_ActionResult y1) cs = (x1 =:= y1) cs
  (=.=) _ _ _ = Fail_C_Success defCover defFailInfo
  (=.<=) (C_MsgResult x1) (C_MsgResult y1) cs = (x1 =:<= y1) cs
  (=.<=) (C_ActionResult x1) (C_ActionResult y1) cs = (x1 =:<= y1) cs
  (=.<=) _ _ _ = Fail_C_Success defCover defFailInfo
  bind i (C_MsgResult x2) = ((i :=: (ChooseN 0 1)):(concat [(bind (leftID i) x2)]))
  bind i (C_ActionResult x2) = ((i :=: (ChooseN 1 1)):(concat [(bind (leftID i) x2)]))
  bind i (Choice_C_AnalysisResult cd j x y) = [(ConstraintChoice cd j (bind i x) (bind i y))]
  bind i (Choices_C_AnalysisResult cd j@(FreeID _ _) xs) = bindOrNarrow i cd j xs
  bind i (Choices_C_AnalysisResult cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind i) xs))]
  bind _ (Choices_C_AnalysisResult cd i _) = error ("AnalysisTypes.AnalysisResult.bind: Choices with ChoiceID: " ++ (show i))
  bind _ (Fail_C_AnalysisResult cd info) = [(Unsolvable info)]
  bind i (Guard_C_AnalysisResult cd c e) = (getConstrList c) ++ (bind i e)
  lazyBind i (C_MsgResult x2) = [(i :=: (ChooseN 0 1)),((leftID i) :=: (LazyBind (lazyBind (leftID i) x2)))]
  lazyBind i (C_ActionResult x2) = [(i :=: (ChooseN 1 1)),((leftID i) :=: (LazyBind (lazyBind (leftID i) x2)))]
  lazyBind i (Choice_C_AnalysisResult cd j x y) = [(ConstraintChoice cd j (lazyBind i x) (lazyBind i y))]
  lazyBind i (Choices_C_AnalysisResult cd j@(FreeID _ _) xs) = lazyBindOrNarrow i cd j xs
  lazyBind i (Choices_C_AnalysisResult cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind i) xs))]
  lazyBind _ (Choices_C_AnalysisResult cd i _) = error ("AnalysisTypes.AnalysisResult.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ (Fail_C_AnalysisResult cd info) = [(Unsolvable info)]
  lazyBind i (Guard_C_AnalysisResult cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind i e)))]


instance Curry_Prelude.Curry C_AnalysisResult where
  (=?=) (Choice_C_AnalysisResult cd i x y) z cs = narrow cd i ((x Curry_Prelude.=?= z) cs) ((y Curry_Prelude.=?= z) cs)
  (=?=) (Choices_C_AnalysisResult cd i xs) y cs = narrows cs cd i (\x -> (x Curry_Prelude.=?= y) cs) xs
  (=?=) (Guard_C_AnalysisResult cd c e) y cs = guardCons cd c ((e Curry_Prelude.=?= y) (addCs c cs))
  (=?=) (Fail_C_AnalysisResult cd info) _ _ = failCons cd info
  (=?=) z (Choice_C_AnalysisResult cd i x y) cs = narrow cd i ((z Curry_Prelude.=?= x) cs) ((z Curry_Prelude.=?= y) cs)
  (=?=) y (Choices_C_AnalysisResult cd i xs) cs = narrows cs cd i (\x -> (y Curry_Prelude.=?= x) cs) xs
  (=?=) y (Guard_C_AnalysisResult cd c e) cs = guardCons cd c ((y Curry_Prelude.=?= e) (addCs c cs))
  (=?=) _ (Fail_C_AnalysisResult cd info) _ = failCons cd info
  (=?=) (C_MsgResult x1) (C_MsgResult y1) cs = (x1 Curry_Prelude.=?= y1) cs
  (=?=) (C_ActionResult x1) (C_ActionResult y1) cs = (x1 Curry_Prelude.=?= y1) cs
  (=?=) _ _ _ = Curry_Prelude.C_False
  (<?=) (Choice_C_AnalysisResult cd i x y) z cs = narrow cd i ((x Curry_Prelude.<?= z) cs) ((y Curry_Prelude.<?= z) cs)
  (<?=) (Choices_C_AnalysisResult cd i xs) y cs = narrows cs cd i (\x -> (x Curry_Prelude.<?= y) cs) xs
  (<?=) (Guard_C_AnalysisResult cd c e) y cs = guardCons cd c ((e Curry_Prelude.<?= y) (addCs c cs))
  (<?=) (Fail_C_AnalysisResult cd info) _ _ = failCons cd info
  (<?=) z (Choice_C_AnalysisResult cd i x y) cs = narrow cd i ((z Curry_Prelude.<?= x) cs) ((z Curry_Prelude.<?= y) cs)
  (<?=) y (Choices_C_AnalysisResult cd i xs) cs = narrows cs cd i (\x -> (y Curry_Prelude.<?= x) cs) xs
  (<?=) y (Guard_C_AnalysisResult cd c e) cs = guardCons cd c ((y Curry_Prelude.<?= e) (addCs c cs))
  (<?=) _ (Fail_C_AnalysisResult cd info) _ = failCons cd info
  (<?=) (C_MsgResult x1) (C_MsgResult y1) cs = (x1 Curry_Prelude.<?= y1) cs
  (<?=) (C_MsgResult _) (C_ActionResult _) _ = Curry_Prelude.C_True
  (<?=) (C_ActionResult x1) (C_ActionResult y1) cs = (x1 Curry_Prelude.<?= y1) cs
  (<?=) _ _ _ = Curry_Prelude.C_False


instance Coverable C_AnalysisResult where
  cover (C_MsgResult x1) = C_MsgResult (cover x1)
  cover (C_ActionResult x1) = C_ActionResult (cover x1)
  cover (Choice_C_AnalysisResult cd i x y) = Choice_C_AnalysisResult (incCover cd) i (cover x) (cover y)
  cover (Choices_C_AnalysisResult cd i xs) = Choices_C_AnalysisResult (incCover cd) i (map cover xs)
  cover (Fail_C_AnalysisResult cd info) = Fail_C_AnalysisResult (incCover cd) info
  cover (Guard_C_AnalysisResult cd c e) = Guard_C_AnalysisResult (incCover cd) c (cover e)


data C_ModuleAnalysis t0
     = C_InterfaceAnalysis (Curry_FlatCurry.C_Prog -> ConstStore -> t0)
     | HO_C_InterfaceAnalysis (Func Curry_FlatCurry.C_Prog t0)
     | C_FlatCurryAnalysis (Curry_FlatCurry.C_Prog -> ConstStore -> t0)
     | HO_C_FlatCurryAnalysis (Func Curry_FlatCurry.C_Prog t0)
     | C_SourceCodeAnalysis (Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO t0)
     | HO_C_SourceCodeAnalysis (Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.C_IO t0))
     | Choice_C_ModuleAnalysis Cover ID (C_ModuleAnalysis t0) (C_ModuleAnalysis t0)
     | Choices_C_ModuleAnalysis Cover ID ([C_ModuleAnalysis t0])
     | Fail_C_ModuleAnalysis Cover FailInfo
     | Guard_C_ModuleAnalysis Cover Constraints (C_ModuleAnalysis t0)

instance Show t0 => Show (C_ModuleAnalysis t0) where
  showsPrec d (Choice_C_ModuleAnalysis cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_ModuleAnalysis cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_ModuleAnalysis cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_ModuleAnalysis cd info) = showChar '!'
  showsPrec _ (C_InterfaceAnalysis x1) = (showString "(InterfaceAnalysis") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (HO_C_InterfaceAnalysis x1) = (showString "(InterfaceAnalysis") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (C_FlatCurryAnalysis x1) = (showString "(FlatCurryAnalysis") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (HO_C_FlatCurryAnalysis x1) = (showString "(FlatCurryAnalysis") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (C_SourceCodeAnalysis x1) = (showString "(SourceCodeAnalysis") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (HO_C_SourceCodeAnalysis x1) = (showString "(SourceCodeAnalysis") . ((showChar ' ') . ((shows x1) . (showChar ')')))


instance Read t0 => Read (C_ModuleAnalysis t0) where
  readsPrec d s = (readParen (d > 10) (\r -> [ (C_InterfaceAnalysis x1,r1) | (_,r0) <- readQualified "AnalysisTypes" "InterfaceAnalysis" r, (x1,r1) <- readsPrec 11 r0]) s) ++ ((readParen (d > 10) (\r -> [ (C_FlatCurryAnalysis x1,r1) | (_,r0) <- readQualified "AnalysisTypes" "FlatCurryAnalysis" r, (x1,r1) <- readsPrec 11 r0]) s) ++ (readParen (d > 10) (\r -> [ (C_SourceCodeAnalysis x1,r1) | (_,r0) <- readQualified "AnalysisTypes" "SourceCodeAnalysis" r, (x1,r1) <- readsPrec 11 r0]) s))


instance NonDet (C_ModuleAnalysis t0) where
  choiceCons = Choice_C_ModuleAnalysis
  choicesCons = Choices_C_ModuleAnalysis
  failCons = Fail_C_ModuleAnalysis
  guardCons = Guard_C_ModuleAnalysis
  try (Choice_C_ModuleAnalysis cd i x y) = tryChoice cd i x y
  try (Choices_C_ModuleAnalysis cd i xs) = tryChoices cd i xs
  try (Fail_C_ModuleAnalysis cd info) = Fail cd info
  try (Guard_C_ModuleAnalysis cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_ModuleAnalysis cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_ModuleAnalysis cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_ModuleAnalysis cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_ModuleAnalysis cd i _) = error ("AnalysisTypes.ModuleAnalysis.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_ModuleAnalysis cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_ModuleAnalysis cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable t0 => Generable (C_ModuleAnalysis t0) where
  generate s = Choices_C_ModuleAnalysis defCover (freeID [1,1,1] s) [(C_InterfaceAnalysis (generate (leftSupply s))),(C_FlatCurryAnalysis (generate (leftSupply s))),(C_SourceCodeAnalysis (generate (leftSupply s)))]


instance NormalForm t0 => NormalForm (C_ModuleAnalysis t0) where
  ($!!) cont (C_InterfaceAnalysis x1) cs = ((\y1 cs -> cont (C_InterfaceAnalysis y1) cs) $!! x1) cs
  ($!!) cont (HO_C_InterfaceAnalysis x1) cs = ((\y1 cs -> cont (HO_C_InterfaceAnalysis y1) cs) $!! x1) cs
  ($!!) cont (C_FlatCurryAnalysis x1) cs = ((\y1 cs -> cont (C_FlatCurryAnalysis y1) cs) $!! x1) cs
  ($!!) cont (HO_C_FlatCurryAnalysis x1) cs = ((\y1 cs -> cont (HO_C_FlatCurryAnalysis y1) cs) $!! x1) cs
  ($!!) cont (C_SourceCodeAnalysis x1) cs = ((\y1 cs -> cont (C_SourceCodeAnalysis y1) cs) $!! x1) cs
  ($!!) cont (HO_C_SourceCodeAnalysis x1) cs = ((\y1 cs -> cont (HO_C_SourceCodeAnalysis y1) cs) $!! x1) cs
  ($!!) cont (Choice_C_ModuleAnalysis cd i x y) cs = nfChoice cont cd i x y cs
  ($!!) cont (Choices_C_ModuleAnalysis cd i xs) cs = nfChoices cont cd i xs cs
  ($!!) cont (Guard_C_ModuleAnalysis cd c e) cs = guardCons cd c ((cont $!! e) (addCs c cs))
  ($!!) _ (Fail_C_ModuleAnalysis cd info) _ = failCons cd info
  ($##) cont (C_InterfaceAnalysis x1) cs = ((\y1 cs -> cont (C_InterfaceAnalysis y1) cs) $## x1) cs
  ($##) cont (HO_C_InterfaceAnalysis x1) cs = ((\y1 cs -> cont (HO_C_InterfaceAnalysis y1) cs) $## x1) cs
  ($##) cont (C_FlatCurryAnalysis x1) cs = ((\y1 cs -> cont (C_FlatCurryAnalysis y1) cs) $## x1) cs
  ($##) cont (HO_C_FlatCurryAnalysis x1) cs = ((\y1 cs -> cont (HO_C_FlatCurryAnalysis y1) cs) $## x1) cs
  ($##) cont (C_SourceCodeAnalysis x1) cs = ((\y1 cs -> cont (C_SourceCodeAnalysis y1) cs) $## x1) cs
  ($##) cont (HO_C_SourceCodeAnalysis x1) cs = ((\y1 cs -> cont (HO_C_SourceCodeAnalysis y1) cs) $## x1) cs
  ($##) cont (Choice_C_ModuleAnalysis cd i x y) cs = gnfChoice cont cd i x y cs
  ($##) cont (Choices_C_ModuleAnalysis cd i xs) cs = gnfChoices cont cd i xs cs
  ($##) cont (Guard_C_ModuleAnalysis cd c e) cs = guardCons cd c ((cont $## e) (addCs c cs))
  ($##) _ (Fail_C_ModuleAnalysis cd info) _ = failCons cd info
  searchNF search cont (C_InterfaceAnalysis x1) = search (\y1 -> cont (C_InterfaceAnalysis y1)) x1
  searchNF search cont (HO_C_InterfaceAnalysis x1) = search (\y1 -> cont (HO_C_InterfaceAnalysis y1)) x1
  searchNF search cont (C_FlatCurryAnalysis x1) = search (\y1 -> cont (C_FlatCurryAnalysis y1)) x1
  searchNF search cont (HO_C_FlatCurryAnalysis x1) = search (\y1 -> cont (HO_C_FlatCurryAnalysis y1)) x1
  searchNF search cont (C_SourceCodeAnalysis x1) = search (\y1 -> cont (C_SourceCodeAnalysis y1)) x1
  searchNF search cont (HO_C_SourceCodeAnalysis x1) = search (\y1 -> cont (HO_C_SourceCodeAnalysis y1)) x1
  searchNF _ _ x = error ("AnalysisTypes.ModuleAnalysis.searchNF: no constructor: " ++ (show x))


instance Unifiable t0 => Unifiable (C_ModuleAnalysis t0) where
  (=.=) (C_InterfaceAnalysis x1) (C_InterfaceAnalysis y1) cs = (x1 =:= y1) cs
  (=.=) (HO_C_InterfaceAnalysis x1) (HO_C_InterfaceAnalysis y1) cs = (x1 =:= y1) cs
  (=.=) (C_FlatCurryAnalysis x1) (C_FlatCurryAnalysis y1) cs = (x1 =:= y1) cs
  (=.=) (HO_C_FlatCurryAnalysis x1) (HO_C_FlatCurryAnalysis y1) cs = (x1 =:= y1) cs
  (=.=) (C_SourceCodeAnalysis x1) (C_SourceCodeAnalysis y1) cs = (x1 =:= y1) cs
  (=.=) (HO_C_SourceCodeAnalysis x1) (HO_C_SourceCodeAnalysis y1) cs = (x1 =:= y1) cs
  (=.=) _ _ _ = Fail_C_Success defCover defFailInfo
  (=.<=) (C_InterfaceAnalysis x1) (C_InterfaceAnalysis y1) cs = (x1 =:<= y1) cs
  (=.<=) (HO_C_InterfaceAnalysis x1) (HO_C_InterfaceAnalysis y1) cs = (x1 =:<= y1) cs
  (=.<=) (C_FlatCurryAnalysis x1) (C_FlatCurryAnalysis y1) cs = (x1 =:<= y1) cs
  (=.<=) (HO_C_FlatCurryAnalysis x1) (HO_C_FlatCurryAnalysis y1) cs = (x1 =:<= y1) cs
  (=.<=) (C_SourceCodeAnalysis x1) (C_SourceCodeAnalysis y1) cs = (x1 =:<= y1) cs
  (=.<=) (HO_C_SourceCodeAnalysis x1) (HO_C_SourceCodeAnalysis y1) cs = (x1 =:<= y1) cs
  (=.<=) _ _ _ = Fail_C_Success defCover defFailInfo
  bind i (C_InterfaceAnalysis x2) = ((i :=: (ChooseN 0 1)):(concat [(bind (leftID i) x2)]))
  bind i (HO_C_InterfaceAnalysis x2) = ((i :=: (ChooseN 0 1)):(concat [(bind (leftID i) x2)]))
  bind i (C_FlatCurryAnalysis x2) = ((i :=: (ChooseN 1 1)):(concat [(bind (leftID i) x2)]))
  bind i (HO_C_FlatCurryAnalysis x2) = ((i :=: (ChooseN 1 1)):(concat [(bind (leftID i) x2)]))
  bind i (C_SourceCodeAnalysis x2) = ((i :=: (ChooseN 2 1)):(concat [(bind (leftID i) x2)]))
  bind i (HO_C_SourceCodeAnalysis x2) = ((i :=: (ChooseN 2 1)):(concat [(bind (leftID i) x2)]))
  bind i (Choice_C_ModuleAnalysis cd j x y) = [(ConstraintChoice cd j (bind i x) (bind i y))]
  bind i (Choices_C_ModuleAnalysis cd j@(FreeID _ _) xs) = bindOrNarrow i cd j xs
  bind i (Choices_C_ModuleAnalysis cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind i) xs))]
  bind _ (Choices_C_ModuleAnalysis cd i _) = error ("AnalysisTypes.ModuleAnalysis.bind: Choices with ChoiceID: " ++ (show i))
  bind _ (Fail_C_ModuleAnalysis cd info) = [(Unsolvable info)]
  bind i (Guard_C_ModuleAnalysis cd c e) = (getConstrList c) ++ (bind i e)
  lazyBind i (C_InterfaceAnalysis x2) = [(i :=: (ChooseN 0 1)),((leftID i) :=: (LazyBind (lazyBind (leftID i) x2)))]
  lazyBind i (HO_C_InterfaceAnalysis x2) = [(i :=: (ChooseN 0 1)),((leftID i) :=: (LazyBind (lazyBind (leftID i) x2)))]
  lazyBind i (C_FlatCurryAnalysis x2) = [(i :=: (ChooseN 1 1)),((leftID i) :=: (LazyBind (lazyBind (leftID i) x2)))]
  lazyBind i (HO_C_FlatCurryAnalysis x2) = [(i :=: (ChooseN 1 1)),((leftID i) :=: (LazyBind (lazyBind (leftID i) x2)))]
  lazyBind i (C_SourceCodeAnalysis x2) = [(i :=: (ChooseN 2 1)),((leftID i) :=: (LazyBind (lazyBind (leftID i) x2)))]
  lazyBind i (HO_C_SourceCodeAnalysis x2) = [(i :=: (ChooseN 2 1)),((leftID i) :=: (LazyBind (lazyBind (leftID i) x2)))]
  lazyBind i (Choice_C_ModuleAnalysis cd j x y) = [(ConstraintChoice cd j (lazyBind i x) (lazyBind i y))]
  lazyBind i (Choices_C_ModuleAnalysis cd j@(FreeID _ _) xs) = lazyBindOrNarrow i cd j xs
  lazyBind i (Choices_C_ModuleAnalysis cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind i) xs))]
  lazyBind _ (Choices_C_ModuleAnalysis cd i _) = error ("AnalysisTypes.ModuleAnalysis.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ (Fail_C_ModuleAnalysis cd info) = [(Unsolvable info)]
  lazyBind i (Guard_C_ModuleAnalysis cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind i e)))]


instance Curry_Prelude.Curry t0 => Curry_Prelude.Curry (C_ModuleAnalysis t0) where
  (=?=) (Choice_C_ModuleAnalysis cd i x y) z cs = narrow cd i ((x Curry_Prelude.=?= z) cs) ((y Curry_Prelude.=?= z) cs)
  (=?=) (Choices_C_ModuleAnalysis cd i xs) y cs = narrows cs cd i (\x -> (x Curry_Prelude.=?= y) cs) xs
  (=?=) (Guard_C_ModuleAnalysis cd c e) y cs = guardCons cd c ((e Curry_Prelude.=?= y) (addCs c cs))
  (=?=) (Fail_C_ModuleAnalysis cd info) _ _ = failCons cd info
  (=?=) z (Choice_C_ModuleAnalysis cd i x y) cs = narrow cd i ((z Curry_Prelude.=?= x) cs) ((z Curry_Prelude.=?= y) cs)
  (=?=) y (Choices_C_ModuleAnalysis cd i xs) cs = narrows cs cd i (\x -> (y Curry_Prelude.=?= x) cs) xs
  (=?=) y (Guard_C_ModuleAnalysis cd c e) cs = guardCons cd c ((y Curry_Prelude.=?= e) (addCs c cs))
  (=?=) _ (Fail_C_ModuleAnalysis cd info) _ = failCons cd info
  (=?=) (C_InterfaceAnalysis x1) (C_InterfaceAnalysis y1) cs = (x1 Curry_Prelude.=?= y1) cs
  (=?=) (HO_C_InterfaceAnalysis x1) (HO_C_InterfaceAnalysis y1) cs = (x1 Curry_Prelude.=?= y1) cs
  (=?=) (C_FlatCurryAnalysis x1) (C_FlatCurryAnalysis y1) cs = (x1 Curry_Prelude.=?= y1) cs
  (=?=) (HO_C_FlatCurryAnalysis x1) (HO_C_FlatCurryAnalysis y1) cs = (x1 Curry_Prelude.=?= y1) cs
  (=?=) (C_SourceCodeAnalysis x1) (C_SourceCodeAnalysis y1) cs = (x1 Curry_Prelude.=?= y1) cs
  (=?=) (HO_C_SourceCodeAnalysis x1) (HO_C_SourceCodeAnalysis y1) cs = (x1 Curry_Prelude.=?= y1) cs
  (=?=) _ _ _ = Curry_Prelude.C_False
  (<?=) (Choice_C_ModuleAnalysis cd i x y) z cs = narrow cd i ((x Curry_Prelude.<?= z) cs) ((y Curry_Prelude.<?= z) cs)
  (<?=) (Choices_C_ModuleAnalysis cd i xs) y cs = narrows cs cd i (\x -> (x Curry_Prelude.<?= y) cs) xs
  (<?=) (Guard_C_ModuleAnalysis cd c e) y cs = guardCons cd c ((e Curry_Prelude.<?= y) (addCs c cs))
  (<?=) (Fail_C_ModuleAnalysis cd info) _ _ = failCons cd info
  (<?=) z (Choice_C_ModuleAnalysis cd i x y) cs = narrow cd i ((z Curry_Prelude.<?= x) cs) ((z Curry_Prelude.<?= y) cs)
  (<?=) y (Choices_C_ModuleAnalysis cd i xs) cs = narrows cs cd i (\x -> (y Curry_Prelude.<?= x) cs) xs
  (<?=) y (Guard_C_ModuleAnalysis cd c e) cs = guardCons cd c ((y Curry_Prelude.<?= e) (addCs c cs))
  (<?=) _ (Fail_C_ModuleAnalysis cd info) _ = failCons cd info
  (<?=) (C_InterfaceAnalysis x1) (C_InterfaceAnalysis y1) cs = (x1 Curry_Prelude.<?= y1) cs
  (<?=) (C_InterfaceAnalysis _) (C_FlatCurryAnalysis _) _ = Curry_Prelude.C_True
  (<?=) (C_InterfaceAnalysis _) (HO_C_FlatCurryAnalysis _) _ = Curry_Prelude.C_True
  (<?=) (C_InterfaceAnalysis _) (C_SourceCodeAnalysis _) _ = Curry_Prelude.C_True
  (<?=) (C_InterfaceAnalysis _) (HO_C_SourceCodeAnalysis _) _ = Curry_Prelude.C_True
  (<?=) (HO_C_InterfaceAnalysis x1) (HO_C_InterfaceAnalysis y1) cs = (x1 Curry_Prelude.<?= y1) cs
  (<?=) (HO_C_InterfaceAnalysis _) (C_FlatCurryAnalysis _) _ = Curry_Prelude.C_True
  (<?=) (HO_C_InterfaceAnalysis _) (HO_C_FlatCurryAnalysis _) _ = Curry_Prelude.C_True
  (<?=) (HO_C_InterfaceAnalysis _) (C_SourceCodeAnalysis _) _ = Curry_Prelude.C_True
  (<?=) (HO_C_InterfaceAnalysis _) (HO_C_SourceCodeAnalysis _) _ = Curry_Prelude.C_True
  (<?=) (C_FlatCurryAnalysis x1) (C_FlatCurryAnalysis y1) cs = (x1 Curry_Prelude.<?= y1) cs
  (<?=) (C_FlatCurryAnalysis _) (C_SourceCodeAnalysis _) _ = Curry_Prelude.C_True
  (<?=) (C_FlatCurryAnalysis _) (HO_C_SourceCodeAnalysis _) _ = Curry_Prelude.C_True
  (<?=) (HO_C_FlatCurryAnalysis x1) (HO_C_FlatCurryAnalysis y1) cs = (x1 Curry_Prelude.<?= y1) cs
  (<?=) (HO_C_FlatCurryAnalysis _) (C_SourceCodeAnalysis _) _ = Curry_Prelude.C_True
  (<?=) (HO_C_FlatCurryAnalysis _) (HO_C_SourceCodeAnalysis _) _ = Curry_Prelude.C_True
  (<?=) (C_SourceCodeAnalysis x1) (C_SourceCodeAnalysis y1) cs = (x1 Curry_Prelude.<?= y1) cs
  (<?=) (HO_C_SourceCodeAnalysis x1) (HO_C_SourceCodeAnalysis y1) cs = (x1 Curry_Prelude.<?= y1) cs
  (<?=) _ _ _ = Curry_Prelude.C_False


instance Coverable t0 => Coverable (C_ModuleAnalysis t0) where
  cover (C_InterfaceAnalysis x1) = C_InterfaceAnalysis (cover x1)
  cover (HO_C_InterfaceAnalysis x1) = HO_C_InterfaceAnalysis (cover x1)
  cover (C_FlatCurryAnalysis x1) = C_FlatCurryAnalysis (cover x1)
  cover (HO_C_FlatCurryAnalysis x1) = HO_C_FlatCurryAnalysis (cover x1)
  cover (C_SourceCodeAnalysis x1) = C_SourceCodeAnalysis (cover x1)
  cover (HO_C_SourceCodeAnalysis x1) = HO_C_SourceCodeAnalysis (cover x1)
  cover (Choice_C_ModuleAnalysis cd i x y) = Choice_C_ModuleAnalysis (incCover cd) i (cover x) (cover y)
  cover (Choices_C_ModuleAnalysis cd i xs) = Choices_C_ModuleAnalysis (incCover cd) i (map cover xs)
  cover (Fail_C_ModuleAnalysis cd info) = Fail_C_ModuleAnalysis (incCover cd) info
  cover (Guard_C_ModuleAnalysis cd c e) = Guard_C_ModuleAnalysis (incCover cd) c (cover e)


data C_ModuleAnalysisResult
     = C_ContentsResult C_ContentsKind (Curry_Prelude.OP_List Curry_Prelude.C_Char)
     | C_ModuleAction (Curry_Prelude.C_IO Curry_Prelude.OP_Unit)
     | Choice_C_ModuleAnalysisResult Cover ID C_ModuleAnalysisResult C_ModuleAnalysisResult
     | Choices_C_ModuleAnalysisResult Cover ID ([C_ModuleAnalysisResult])
     | Fail_C_ModuleAnalysisResult Cover FailInfo
     | Guard_C_ModuleAnalysisResult Cover Constraints C_ModuleAnalysisResult

instance Show C_ModuleAnalysisResult where
  showsPrec d (Choice_C_ModuleAnalysisResult cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_ModuleAnalysisResult cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_ModuleAnalysisResult cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_ModuleAnalysisResult cd info) = showChar '!'
  showsPrec _ (C_ContentsResult x1 x2) = (showString "(ContentsResult") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . (showChar ')')))))
  showsPrec _ (C_ModuleAction x1) = (showString "(ModuleAction") . ((showChar ' ') . ((shows x1) . (showChar ')')))


instance Read C_ModuleAnalysisResult where
  readsPrec d s = (readParen (d > 10) (\r -> [ (C_ContentsResult x1 x2,r2) | (_,r0) <- readQualified "AnalysisTypes" "ContentsResult" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1]) s) ++ (readParen (d > 10) (\r -> [ (C_ModuleAction x1,r1) | (_,r0) <- readQualified "AnalysisTypes" "ModuleAction" r, (x1,r1) <- readsPrec 11 r0]) s)


instance NonDet C_ModuleAnalysisResult where
  choiceCons = Choice_C_ModuleAnalysisResult
  choicesCons = Choices_C_ModuleAnalysisResult
  failCons = Fail_C_ModuleAnalysisResult
  guardCons = Guard_C_ModuleAnalysisResult
  try (Choice_C_ModuleAnalysisResult cd i x y) = tryChoice cd i x y
  try (Choices_C_ModuleAnalysisResult cd i xs) = tryChoices cd i xs
  try (Fail_C_ModuleAnalysisResult cd info) = Fail cd info
  try (Guard_C_ModuleAnalysisResult cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_ModuleAnalysisResult cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_ModuleAnalysisResult cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_ModuleAnalysisResult cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_ModuleAnalysisResult cd i _) = error ("AnalysisTypes.ModuleAnalysisResult.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_ModuleAnalysisResult cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_ModuleAnalysisResult cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_ModuleAnalysisResult where
  generate s = Choices_C_ModuleAnalysisResult defCover (freeID [2,1] s) [(C_ContentsResult (generate (leftSupply s)) (generate (rightSupply s))),(C_ModuleAction (generate (leftSupply s)))]


instance NormalForm C_ModuleAnalysisResult where
  ($!!) cont (C_ContentsResult x1 x2) cs = ((\y1 cs -> ((\y2 cs -> cont (C_ContentsResult y1 y2) cs) $!! x2) cs) $!! x1) cs
  ($!!) cont (C_ModuleAction x1) cs = ((\y1 cs -> cont (C_ModuleAction y1) cs) $!! x1) cs
  ($!!) cont (Choice_C_ModuleAnalysisResult cd i x y) cs = nfChoice cont cd i x y cs
  ($!!) cont (Choices_C_ModuleAnalysisResult cd i xs) cs = nfChoices cont cd i xs cs
  ($!!) cont (Guard_C_ModuleAnalysisResult cd c e) cs = guardCons cd c ((cont $!! e) (addCs c cs))
  ($!!) _ (Fail_C_ModuleAnalysisResult cd info) _ = failCons cd info
  ($##) cont (C_ContentsResult x1 x2) cs = ((\y1 cs -> ((\y2 cs -> cont (C_ContentsResult y1 y2) cs) $## x2) cs) $## x1) cs
  ($##) cont (C_ModuleAction x1) cs = ((\y1 cs -> cont (C_ModuleAction y1) cs) $## x1) cs
  ($##) cont (Choice_C_ModuleAnalysisResult cd i x y) cs = gnfChoice cont cd i x y cs
  ($##) cont (Choices_C_ModuleAnalysisResult cd i xs) cs = gnfChoices cont cd i xs cs
  ($##) cont (Guard_C_ModuleAnalysisResult cd c e) cs = guardCons cd c ((cont $## e) (addCs c cs))
  ($##) _ (Fail_C_ModuleAnalysisResult cd info) _ = failCons cd info
  searchNF search cont (C_ContentsResult x1 x2) = search (\y1 -> search (\y2 -> cont (C_ContentsResult y1 y2)) x2) x1
  searchNF search cont (C_ModuleAction x1) = search (\y1 -> cont (C_ModuleAction y1)) x1
  searchNF _ _ x = error ("AnalysisTypes.ModuleAnalysisResult.searchNF: no constructor: " ++ (show x))


instance Unifiable C_ModuleAnalysisResult where
  (=.=) (C_ContentsResult x1 x2) (C_ContentsResult y1 y2) cs = (((x1 =:= y1) cs) & ((x2 =:= y2) cs)) cs
  (=.=) (C_ModuleAction x1) (C_ModuleAction y1) cs = (x1 =:= y1) cs
  (=.=) _ _ _ = Fail_C_Success defCover defFailInfo
  (=.<=) (C_ContentsResult x1 x2) (C_ContentsResult y1 y2) cs = (((x1 =:<= y1) cs) & ((x2 =:<= y2) cs)) cs
  (=.<=) (C_ModuleAction x1) (C_ModuleAction y1) cs = (x1 =:<= y1) cs
  (=.<=) _ _ _ = Fail_C_Success defCover defFailInfo
  bind i (C_ContentsResult x2 x3) = ((i :=: (ChooseN 0 2)):(concat [(bind (leftID i) x2),(bind (rightID i) x3)]))
  bind i (C_ModuleAction x2) = ((i :=: (ChooseN 1 1)):(concat [(bind (leftID i) x2)]))
  bind i (Choice_C_ModuleAnalysisResult cd j x y) = [(ConstraintChoice cd j (bind i x) (bind i y))]
  bind i (Choices_C_ModuleAnalysisResult cd j@(FreeID _ _) xs) = bindOrNarrow i cd j xs
  bind i (Choices_C_ModuleAnalysisResult cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind i) xs))]
  bind _ (Choices_C_ModuleAnalysisResult cd i _) = error ("AnalysisTypes.ModuleAnalysisResult.bind: Choices with ChoiceID: " ++ (show i))
  bind _ (Fail_C_ModuleAnalysisResult cd info) = [(Unsolvable info)]
  bind i (Guard_C_ModuleAnalysisResult cd c e) = (getConstrList c) ++ (bind i e)
  lazyBind i (C_ContentsResult x2 x3) = [(i :=: (ChooseN 0 2)),((leftID i) :=: (LazyBind (lazyBind (leftID i) x2))),((rightID i) :=: (LazyBind (lazyBind (rightID i) x3)))]
  lazyBind i (C_ModuleAction x2) = [(i :=: (ChooseN 1 1)),((leftID i) :=: (LazyBind (lazyBind (leftID i) x2)))]
  lazyBind i (Choice_C_ModuleAnalysisResult cd j x y) = [(ConstraintChoice cd j (lazyBind i x) (lazyBind i y))]
  lazyBind i (Choices_C_ModuleAnalysisResult cd j@(FreeID _ _) xs) = lazyBindOrNarrow i cd j xs
  lazyBind i (Choices_C_ModuleAnalysisResult cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind i) xs))]
  lazyBind _ (Choices_C_ModuleAnalysisResult cd i _) = error ("AnalysisTypes.ModuleAnalysisResult.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ (Fail_C_ModuleAnalysisResult cd info) = [(Unsolvable info)]
  lazyBind i (Guard_C_ModuleAnalysisResult cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind i e)))]


instance Curry_Prelude.Curry C_ModuleAnalysisResult where
  (=?=) (Choice_C_ModuleAnalysisResult cd i x y) z cs = narrow cd i ((x Curry_Prelude.=?= z) cs) ((y Curry_Prelude.=?= z) cs)
  (=?=) (Choices_C_ModuleAnalysisResult cd i xs) y cs = narrows cs cd i (\x -> (x Curry_Prelude.=?= y) cs) xs
  (=?=) (Guard_C_ModuleAnalysisResult cd c e) y cs = guardCons cd c ((e Curry_Prelude.=?= y) (addCs c cs))
  (=?=) (Fail_C_ModuleAnalysisResult cd info) _ _ = failCons cd info
  (=?=) z (Choice_C_ModuleAnalysisResult cd i x y) cs = narrow cd i ((z Curry_Prelude.=?= x) cs) ((z Curry_Prelude.=?= y) cs)
  (=?=) y (Choices_C_ModuleAnalysisResult cd i xs) cs = narrows cs cd i (\x -> (y Curry_Prelude.=?= x) cs) xs
  (=?=) y (Guard_C_ModuleAnalysisResult cd c e) cs = guardCons cd c ((y Curry_Prelude.=?= e) (addCs c cs))
  (=?=) _ (Fail_C_ModuleAnalysisResult cd info) _ = failCons cd info
  (=?=) (C_ContentsResult x1 x2) (C_ContentsResult y1 y2) cs = Curry_Prelude.d_OP_ampersand_ampersand ((x1 Curry_Prelude.=?= y1) cs) ((x2 Curry_Prelude.=?= y2) cs) cs
  (=?=) (C_ModuleAction x1) (C_ModuleAction y1) cs = (x1 Curry_Prelude.=?= y1) cs
  (=?=) _ _ _ = Curry_Prelude.C_False
  (<?=) (Choice_C_ModuleAnalysisResult cd i x y) z cs = narrow cd i ((x Curry_Prelude.<?= z) cs) ((y Curry_Prelude.<?= z) cs)
  (<?=) (Choices_C_ModuleAnalysisResult cd i xs) y cs = narrows cs cd i (\x -> (x Curry_Prelude.<?= y) cs) xs
  (<?=) (Guard_C_ModuleAnalysisResult cd c e) y cs = guardCons cd c ((e Curry_Prelude.<?= y) (addCs c cs))
  (<?=) (Fail_C_ModuleAnalysisResult cd info) _ _ = failCons cd info
  (<?=) z (Choice_C_ModuleAnalysisResult cd i x y) cs = narrow cd i ((z Curry_Prelude.<?= x) cs) ((z Curry_Prelude.<?= y) cs)
  (<?=) y (Choices_C_ModuleAnalysisResult cd i xs) cs = narrows cs cd i (\x -> (y Curry_Prelude.<?= x) cs) xs
  (<?=) y (Guard_C_ModuleAnalysisResult cd c e) cs = guardCons cd c ((y Curry_Prelude.<?= e) (addCs c cs))
  (<?=) _ (Fail_C_ModuleAnalysisResult cd info) _ = failCons cd info
  (<?=) (C_ContentsResult x1 x2) (C_ContentsResult y1 y2) cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x1 Curry_Prelude.=?= y1) cs) ((x2 Curry_Prelude.<?= y2) cs) cs) cs
  (<?=) (C_ContentsResult _ _) (C_ModuleAction _) _ = Curry_Prelude.C_True
  (<?=) (C_ModuleAction x1) (C_ModuleAction y1) cs = (x1 Curry_Prelude.<?= y1) cs
  (<?=) _ _ _ = Curry_Prelude.C_False


instance Coverable C_ModuleAnalysisResult where
  cover (C_ContentsResult x1 x2) = C_ContentsResult (cover x1) (cover x2)
  cover (C_ModuleAction x1) = C_ModuleAction (cover x1)
  cover (Choice_C_ModuleAnalysisResult cd i x y) = Choice_C_ModuleAnalysisResult (incCover cd) i (cover x) (cover y)
  cover (Choices_C_ModuleAnalysisResult cd i xs) = Choices_C_ModuleAnalysisResult (incCover cd) i (map cover xs)
  cover (Fail_C_ModuleAnalysisResult cd info) = Fail_C_ModuleAnalysisResult (incCover cd) info
  cover (Guard_C_ModuleAnalysisResult cd c e) = Guard_C_ModuleAnalysisResult (incCover cd) c (cover e)


data C_ContentsKind
     = C_CurryProg
     | C_LCurryProg
     | C_FlatCurryExp
     | C_OtherText
     | Choice_C_ContentsKind Cover ID C_ContentsKind C_ContentsKind
     | Choices_C_ContentsKind Cover ID ([C_ContentsKind])
     | Fail_C_ContentsKind Cover FailInfo
     | Guard_C_ContentsKind Cover Constraints C_ContentsKind

instance Show C_ContentsKind where
  showsPrec d (Choice_C_ContentsKind cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_ContentsKind cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_ContentsKind cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_ContentsKind cd info) = showChar '!'
  showsPrec _ C_CurryProg = showString "CurryProg"
  showsPrec _ C_LCurryProg = showString "LCurryProg"
  showsPrec _ C_FlatCurryExp = showString "FlatCurryExp"
  showsPrec _ C_OtherText = showString "OtherText"


instance Read C_ContentsKind where
  readsPrec _ s = (readParen False (\r -> [ (C_CurryProg,r0) | (_,r0) <- readQualified "AnalysisTypes" "CurryProg" r]) s) ++ ((readParen False (\r -> [ (C_LCurryProg,r0) | (_,r0) <- readQualified "AnalysisTypes" "LCurryProg" r]) s) ++ ((readParen False (\r -> [ (C_FlatCurryExp,r0) | (_,r0) <- readQualified "AnalysisTypes" "FlatCurryExp" r]) s) ++ (readParen False (\r -> [ (C_OtherText,r0) | (_,r0) <- readQualified "AnalysisTypes" "OtherText" r]) s)))


instance NonDet C_ContentsKind where
  choiceCons = Choice_C_ContentsKind
  choicesCons = Choices_C_ContentsKind
  failCons = Fail_C_ContentsKind
  guardCons = Guard_C_ContentsKind
  try (Choice_C_ContentsKind cd i x y) = tryChoice cd i x y
  try (Choices_C_ContentsKind cd i xs) = tryChoices cd i xs
  try (Fail_C_ContentsKind cd info) = Fail cd info
  try (Guard_C_ContentsKind cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_ContentsKind cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_ContentsKind cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_ContentsKind cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_ContentsKind cd i _) = error ("AnalysisTypes.ContentsKind.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_ContentsKind cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_ContentsKind cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_ContentsKind where
  generate s = Choices_C_ContentsKind defCover (freeID [0,0,0,0] s) [C_CurryProg,C_LCurryProg,C_FlatCurryExp,C_OtherText]


instance NormalForm C_ContentsKind where
  ($!!) cont C_CurryProg cs = cont C_CurryProg cs
  ($!!) cont C_LCurryProg cs = cont C_LCurryProg cs
  ($!!) cont C_FlatCurryExp cs = cont C_FlatCurryExp cs
  ($!!) cont C_OtherText cs = cont C_OtherText cs
  ($!!) cont (Choice_C_ContentsKind cd i x y) cs = nfChoice cont cd i x y cs
  ($!!) cont (Choices_C_ContentsKind cd i xs) cs = nfChoices cont cd i xs cs
  ($!!) cont (Guard_C_ContentsKind cd c e) cs = guardCons cd c ((cont $!! e) (addCs c cs))
  ($!!) _ (Fail_C_ContentsKind cd info) _ = failCons cd info
  ($##) cont C_CurryProg cs = cont C_CurryProg cs
  ($##) cont C_LCurryProg cs = cont C_LCurryProg cs
  ($##) cont C_FlatCurryExp cs = cont C_FlatCurryExp cs
  ($##) cont C_OtherText cs = cont C_OtherText cs
  ($##) cont (Choice_C_ContentsKind cd i x y) cs = gnfChoice cont cd i x y cs
  ($##) cont (Choices_C_ContentsKind cd i xs) cs = gnfChoices cont cd i xs cs
  ($##) cont (Guard_C_ContentsKind cd c e) cs = guardCons cd c ((cont $## e) (addCs c cs))
  ($##) _ (Fail_C_ContentsKind cd info) _ = failCons cd info
  searchNF _ cont C_CurryProg = cont C_CurryProg
  searchNF _ cont C_LCurryProg = cont C_LCurryProg
  searchNF _ cont C_FlatCurryExp = cont C_FlatCurryExp
  searchNF _ cont C_OtherText = cont C_OtherText
  searchNF _ _ x = error ("AnalysisTypes.ContentsKind.searchNF: no constructor: " ++ (show x))


instance Unifiable C_ContentsKind where
  (=.=) C_CurryProg C_CurryProg cs = C_Success
  (=.=) C_LCurryProg C_LCurryProg cs = C_Success
  (=.=) C_FlatCurryExp C_FlatCurryExp cs = C_Success
  (=.=) C_OtherText C_OtherText cs = C_Success
  (=.=) _ _ _ = Fail_C_Success defCover defFailInfo
  (=.<=) C_CurryProg C_CurryProg cs = C_Success
  (=.<=) C_LCurryProg C_LCurryProg cs = C_Success
  (=.<=) C_FlatCurryExp C_FlatCurryExp cs = C_Success
  (=.<=) C_OtherText C_OtherText cs = C_Success
  (=.<=) _ _ _ = Fail_C_Success defCover defFailInfo
  bind i C_CurryProg = ((i :=: (ChooseN 0 0)):(concat []))
  bind i C_LCurryProg = ((i :=: (ChooseN 1 0)):(concat []))
  bind i C_FlatCurryExp = ((i :=: (ChooseN 2 0)):(concat []))
  bind i C_OtherText = ((i :=: (ChooseN 3 0)):(concat []))
  bind i (Choice_C_ContentsKind cd j x y) = [(ConstraintChoice cd j (bind i x) (bind i y))]
  bind i (Choices_C_ContentsKind cd j@(FreeID _ _) xs) = bindOrNarrow i cd j xs
  bind i (Choices_C_ContentsKind cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind i) xs))]
  bind _ (Choices_C_ContentsKind cd i _) = error ("AnalysisTypes.ContentsKind.bind: Choices with ChoiceID: " ++ (show i))
  bind _ (Fail_C_ContentsKind cd info) = [(Unsolvable info)]
  bind i (Guard_C_ContentsKind cd c e) = (getConstrList c) ++ (bind i e)
  lazyBind i C_CurryProg = [(i :=: (ChooseN 0 0))]
  lazyBind i C_LCurryProg = [(i :=: (ChooseN 1 0))]
  lazyBind i C_FlatCurryExp = [(i :=: (ChooseN 2 0))]
  lazyBind i C_OtherText = [(i :=: (ChooseN 3 0))]
  lazyBind i (Choice_C_ContentsKind cd j x y) = [(ConstraintChoice cd j (lazyBind i x) (lazyBind i y))]
  lazyBind i (Choices_C_ContentsKind cd j@(FreeID _ _) xs) = lazyBindOrNarrow i cd j xs
  lazyBind i (Choices_C_ContentsKind cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind i) xs))]
  lazyBind _ (Choices_C_ContentsKind cd i _) = error ("AnalysisTypes.ContentsKind.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ (Fail_C_ContentsKind cd info) = [(Unsolvable info)]
  lazyBind i (Guard_C_ContentsKind cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind i e)))]


instance Curry_Prelude.Curry C_ContentsKind where
  (=?=) (Choice_C_ContentsKind cd i x y) z cs = narrow cd i ((x Curry_Prelude.=?= z) cs) ((y Curry_Prelude.=?= z) cs)
  (=?=) (Choices_C_ContentsKind cd i xs) y cs = narrows cs cd i (\x -> (x Curry_Prelude.=?= y) cs) xs
  (=?=) (Guard_C_ContentsKind cd c e) y cs = guardCons cd c ((e Curry_Prelude.=?= y) (addCs c cs))
  (=?=) (Fail_C_ContentsKind cd info) _ _ = failCons cd info
  (=?=) z (Choice_C_ContentsKind cd i x y) cs = narrow cd i ((z Curry_Prelude.=?= x) cs) ((z Curry_Prelude.=?= y) cs)
  (=?=) y (Choices_C_ContentsKind cd i xs) cs = narrows cs cd i (\x -> (y Curry_Prelude.=?= x) cs) xs
  (=?=) y (Guard_C_ContentsKind cd c e) cs = guardCons cd c ((y Curry_Prelude.=?= e) (addCs c cs))
  (=?=) _ (Fail_C_ContentsKind cd info) _ = failCons cd info
  (=?=) C_CurryProg C_CurryProg cs = Curry_Prelude.C_True
  (=?=) C_LCurryProg C_LCurryProg cs = Curry_Prelude.C_True
  (=?=) C_FlatCurryExp C_FlatCurryExp cs = Curry_Prelude.C_True
  (=?=) C_OtherText C_OtherText cs = Curry_Prelude.C_True
  (=?=) _ _ _ = Curry_Prelude.C_False
  (<?=) (Choice_C_ContentsKind cd i x y) z cs = narrow cd i ((x Curry_Prelude.<?= z) cs) ((y Curry_Prelude.<?= z) cs)
  (<?=) (Choices_C_ContentsKind cd i xs) y cs = narrows cs cd i (\x -> (x Curry_Prelude.<?= y) cs) xs
  (<?=) (Guard_C_ContentsKind cd c e) y cs = guardCons cd c ((e Curry_Prelude.<?= y) (addCs c cs))
  (<?=) (Fail_C_ContentsKind cd info) _ _ = failCons cd info
  (<?=) z (Choice_C_ContentsKind cd i x y) cs = narrow cd i ((z Curry_Prelude.<?= x) cs) ((z Curry_Prelude.<?= y) cs)
  (<?=) y (Choices_C_ContentsKind cd i xs) cs = narrows cs cd i (\x -> (y Curry_Prelude.<?= x) cs) xs
  (<?=) y (Guard_C_ContentsKind cd c e) cs = guardCons cd c ((y Curry_Prelude.<?= e) (addCs c cs))
  (<?=) _ (Fail_C_ContentsKind cd info) _ = failCons cd info
  (<?=) C_CurryProg C_CurryProg cs = Curry_Prelude.C_True
  (<?=) C_CurryProg C_LCurryProg _ = Curry_Prelude.C_True
  (<?=) C_CurryProg C_FlatCurryExp _ = Curry_Prelude.C_True
  (<?=) C_CurryProg C_OtherText _ = Curry_Prelude.C_True
  (<?=) C_LCurryProg C_LCurryProg cs = Curry_Prelude.C_True
  (<?=) C_LCurryProg C_FlatCurryExp _ = Curry_Prelude.C_True
  (<?=) C_LCurryProg C_OtherText _ = Curry_Prelude.C_True
  (<?=) C_FlatCurryExp C_FlatCurryExp cs = Curry_Prelude.C_True
  (<?=) C_FlatCurryExp C_OtherText _ = Curry_Prelude.C_True
  (<?=) C_OtherText C_OtherText cs = Curry_Prelude.C_True
  (<?=) _ _ _ = Curry_Prelude.C_False


instance Coverable C_ContentsKind where
  cover C_CurryProg = C_CurryProg
  cover C_LCurryProg = C_LCurryProg
  cover C_FlatCurryExp = C_FlatCurryExp
  cover C_OtherText = C_OtherText
  cover (Choice_C_ContentsKind cd i x y) = Choice_C_ContentsKind (incCover cd) i (cover x) (cover y)
  cover (Choices_C_ContentsKind cd i xs) = Choices_C_ContentsKind (incCover cd) i (map cover xs)
  cover (Fail_C_ContentsKind cd info) = Fail_C_ContentsKind (incCover cd) info
  cover (Guard_C_ContentsKind cd c e) = Guard_C_ContentsKind (incCover cd) c (cover e)

